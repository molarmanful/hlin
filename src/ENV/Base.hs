{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ENV.Base where

import ANY
import Control.Monad.Except
import Control.Monad.State
import Data.Hashable (Hashable)
import Data.List (stripPrefix)
import qualified Data.Map as M
import GHC.Conc (atomically)
import qualified StmContainers.Map as CM
import Types
import Util

instance Show ENV where
  show :: ENV -> String
  show ENV {stack, code} = unwords $ show <$> [stack, code]

run :: String -> IO ENV
run s =
  dENV >>= unENVS do
    loadLines ls
    modify \env -> env {code = parse ls}
    loop
  where
    ls = lines s

loadLines :: [String] -> ENVS ()
loadLines =
  zipWithM_
    ( \i v -> do
        ENV {lns} <- get
        setCM (v, UN) (PATH ("", i)) lns
    )
    [0 ..]

loop :: ENVS ()
loop = do
  env@ENV {code} <- get
  case code of
    [] -> return ()
    x : xs -> do
      put env {code = xs}
      choice x
      loop

choice :: ANY -> ENVS ()
choice (CMD a) = cmd a
choice a = push a

cmd :: String -> ENVS ()
cmd ('\\' : k) = push $ CMD k
cmd (stripPrefix "=$$" -> Just k) = arg1 (setgvar k)
cmd (stripPrefix "=$" -> Just k) = arg1 (setvar k)
cmd c@(stripPrefix "$$" -> Just k) = pushgvar c k
cmd c@('$' : k) = pushvar c k
cmd c = cmd' c

cmd' :: [Char] -> ENVS ()
-- flow
cmd' "#" = arg1 eval
-- stack
cmd' "dup" = mods1 (\a -> [a, a])
cmd' "pop" = mods1 (const [])
cmd' "swap" = mods2 (\a b -> [b, a])
cmd' "rot" = mods3 (\a b c -> [b, c, a])
cmd' "rot_" = mods3 (\a b c -> [c, a, b])
cmd' "dupd" = mods2 (\a b -> [a, a, b])
cmd' "over" = mods2 (\a b -> [a, b, a])
cmd' "ddup" = mods2 (\a b -> [a, b, a, b])
cmd' "edup" = mods3 (\a b c -> [a, b, c, a, b, c])
cmd' "nip" = mod2 (const id)
cmd' "ppop" = mods2 (\_ _ -> [])
cmd' "qpop" = mods3 (\_ _ _ -> [])
cmd' "swapd" = mods3 (\a b c -> [b, a, c])
cmd' "tuck" = mods2 (\a b -> [b, a, b])
-- TODO: index-based stack manipulation
-- math
cmd' "_" = mod1 (vec1 $ fNUM1 negate)
cmd' "+" = mod2 (vec2 $ fNUM2 (+))
cmd' "-" = mod2 (vec2 $ fNUM2 (-))
cmd' "*" = mod2 (vec2 $ fNUM2 (*))
cmd' "/" = mod2 (vec2 $ fNUM2 (/))
cmd' x = throwError $ "CMD \"" ++ x ++ "\" not found"

-- convenience

dENV :: IO ENV
dENV = do
  lns <- liftIO CM.newIO
  gscope <- liftIO CM.newIO
  gids <- liftIO CM.newIO
  return
    ENV
      { lns,
        code = [],
        path = PATH ("", 0),
        stack = [],
        scope = M.empty,
        gscope,
        ids = M.empty,
        gids,
        arr = []
      }

eval :: ANY -> ENVS ()
eval (FN p a) = do
  env@ENV {code} <- get
  let e = env {code = a, path = p}
   in case code of
        [] -> put e
        _ -> eval' e env
eval a = do
  ENV {path} <- get
  eval $ toFN path a

evale :: ANY -> ENVS ()
evale (FN p a) = do
  env <- get
  let e = env {code = a, path = p}
   in eval' e env
evale a = do
  ENV {path} <- get
  eval $ toFN path a

eval' :: (MonadIO m, MonadState ENV m) => ENV -> ENV -> m ()
eval' a b = do
  ENV {stack} <- liftIO $ unENVS loop a
  put b {stack}

push :: ANY -> ENVS ()
push a = modify \env -> env {stack = a : stack env}

pushs :: [ANY] -> ENVS ()
pushs a = modify \env -> env {stack = reverse a ++ stack env}

setvar :: String -> ANY -> ENVS ()
setvar k v = modify \env -> env {scope = M.insert k v $ scope env}

setgvar :: String -> ANY -> ENVS ()
setgvar k v = do
  ENV {gscope} <- get
  setCM v k gscope

pushvar :: String -> String -> ENVS ()
pushvar c k = do
  ENV {scope} <- get
  case M.lookup k scope of
    Nothing -> pushgvar c k
    Just v -> push v

pushgvar :: String -> String -> ENVS ()
pushgvar c k = do
  ENV {gscope} <- get
  mv <- getCM k gscope
  case mv of
    Nothing -> cmd' c
    Just v -> push v

argN :: Int -> ([ANY] -> ENVS ()) -> ENVS ()
argN n f = do
  env@ENV {stack} <- get
  if isMin n stack
    then
      let (a, b) = splitAt n stack
       in do
            put env {stack = b}
            f $ reverse a
    else throwError $ "stack length < " ++ show n

modN :: Int -> ([ANY] -> ANY) -> ENVS ()
modN n f = argN n $ push . f

modsN :: Int -> ([ANY] -> [ANY]) -> ENVS ()
modsN n f = argN n $ pushs . f

arg1 :: (ANY -> ENVS ()) -> ENVS ()
arg1 = argN 1 . lsap1

arg2 :: (ANY -> ANY -> ENVS ()) -> ENVS ()
arg2 = argN 2 . lsap2

arg3 :: (ANY -> ANY -> ANY -> ENVS ()) -> ENVS ()
arg3 = argN 3 . lsap3

mod1 :: (ANY -> ANY) -> ENVS ()
mod1 = modN 1 . lsap1

mod2 :: (ANY -> ANY -> ANY) -> ENVS ()
mod2 = modN 2 . lsap2

mod3 :: (ANY -> ANY -> ANY -> ANY) -> ENVS ()
mod3 = modN 3 . lsap3

mods1 :: (ANY -> [ANY]) -> ENVS ()
mods1 = modsN 1 . lsap1

mods2 :: (ANY -> ANY -> [ANY]) -> ENVS ()
mods2 = modsN 2 . lsap2

mods3 :: (ANY -> ANY -> ANY -> [ANY]) -> ENVS ()
mods3 = modsN 3 . lsap3

unENVS :: Monad m => ExceptT e (StateT s m) a -> s -> m s
unENVS f = execStateT (runExceptT f)

setCM :: (MonadIO m, Hashable key) => value -> key -> CM.Map key value -> m ()
setCM v k = liftIO . atomically . CM.insert v k

getCM :: (MonadIO m, Hashable key) => key -> CM.Map key value -> m (Maybe value)
getCM k = liftIO . atomically . CM.lookup k