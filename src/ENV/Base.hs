{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ENV.Base where

import ANY
import Control.Monad.Except
import Control.Monad.State
import Data.Hashable (Hashable)
import Data.List (stripPrefix)
import qualified Data.Map as M
import qualified Data.Text as T
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
        setCM (LINE (v, Nothing)) (PATH ("", i)) lns
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
cmd ('\\' : k@(_ : _)) = do
  ENV {path} <- get
  push $ toFN path $ CMD k
cmd (stripPrefix "=$$" -> Just k@(_ : _)) = arg1 (setgvar k)
cmd (stripPrefix "=$" -> Just k@(_ : _)) = arg1 (setvar k)
cmd c@(stripPrefix "$$" -> Just k@(_ : _)) = pushgvar c k
cmd c@('$' : k@(_ : _)) = pushvar c k
cmd ('#' : _ : _) = return ()
cmd c = evalvar c c

cmd' :: [Char] -> ENVS ()
-- vars
cmd' "UN" = push UN
cmd' "$T" = push $ TF True
cmd' "$F" = push $ TF False
cmd' "$L" = do
  ENV {path = PATH (_, n)} <- get
  push $ NUM $ fromIntegral n
cmd' "$PI" = push $ NUM pi
cmd' "$E" = push $ NUM $ exp 1
-- conversion
cmd' ">?" = mod1 toTF
cmd' ">N" = mod1 toNUM
cmd' ">S" = mod1 toSTR
cmd' ">F" = do
  ENV {path} <- get
  mod1 $ toFN path
cmd' ">Q" = mod1 toSEQ
cmd' ">A" = mod1 toARR
cmd' ">M" = mod1 toMAP
cmd' "," = mod2 \a b -> SEQ [a, b]
cmd' ",," = mod1 \a -> SEQ [a]
cmd' ",`" = do
  env@ENV {stack} <- get
  put env {stack = [SEQ $ reverse stack]}
cmd' "\\" = cmd ",," >> cmd ">F"
cmd' "'" = mod1 \a -> do
  toARR a

-- flow
cmd' "." = do
  env@ENV {code, path} <- get
  case code of
    [] -> cmd ";"
    c : cs -> do
      put env {code = cs}
      case c of
        STR a -> push $ STR $ unescText a
        a@(CMD _) -> mod1 \f -> FN path [f, a]
        _ -> push c
cmd' "#" = arg1 eval
cmd' "@@" = arg1 $ evalLn . toInt
cmd' "@~" = cmd "$L" >> cmd "+" >> cmd "@@"
cmd' "@" = push (NUM 0) >> cmd "@~"
cmd' ";" = push (NUM 1) >> cmd "@~"
cmd' ";;" = push (NUM (-1)) >> cmd "@~"
-- I/O
cmd' ">o" = arg1 $ liftIO . putStr . toStr
cmd' "n>o" = arg1 $ liftIO . putStrLn . toStr
cmd' "f>o" = arg1 $ liftIO . print
-- stack
cmd' "dup" = mods1 \a -> [a, a]
cmd' "pop" = mods1 $ const []
cmd' "swap" = mods2 \a b -> [b, a]
cmd' "rot" = mods3 \a b c -> [b, c, a]
cmd' "rot_" = mods3 \a b c -> [c, a, b]
cmd' "dupd" = mods2 \a b -> [a, a, b]
cmd' "over" = mods2 \a b -> [a, b, a]
cmd' "ddup" = mods2 \a b -> [a, b, a, b]
cmd' "edup" = mods3 \a b c -> [a, b, c, a, b, c]
cmd' "nip" = mod2 $ const id
cmd' "ppop" = mods2 \_ _ -> []
cmd' "qpop" = mods3 \_ _ _ -> []
cmd' "swapd" = mods3 \a b c -> [b, a, c]
cmd' "tuck" = mods2 \a b -> [b, a, b]
cmd' "dups" = do
  ENV {stack} <- get
  push $ SEQ $ reverse stack
-- TODO: index-based stack manipulation
cmd' "dip" = arg2 \a f -> evale f >> push a
-- math
cmd' "_" = modv1 $ fNUM1 negate
cmd' "__" = modv1 $ fSTR1 T.reverse
cmd' "+" = modv2 $ fNUM2 (+)
cmd' "++" = modv2 $ fSTR2 (<>)
cmd' "-" = modv2 $ fNUM2 (-)
cmd' "--" = modv2 $ fSTR2 (`T.replace` "")
cmd' "*" = modv2 $ fNUM2 (*)
cmd' "**" = modv2 $ \a b -> STR $ T.replicate (toInt b) $ toSTRW a
cmd' "/" = modv2 $ fNUM2 (/)
cmd' "%" = undefined
cmd' x = throwError $ "\"" ++ x ++ "\" not found"

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
  eval' (env {code = a, path = p}) env
evale a = do
  ENV {path} <- get
  evale $ toFN path a

eval' :: (MonadIO m, MonadState ENV m) => ENV -> ENV -> m ()
eval' a b = do
  ENV {stack} <- liftIO $ unENVS loop a
  put b {stack}

evalLn :: Int -> ENVS ()
evalLn n = do
  l <- fnLn n
  case l of
    Nothing -> return ()
    Just (LINE (_, a)) -> mapM_ eval a

fnLn :: Int -> ENVS (Maybe LINE)
fnLn n = do
  ENV {lns, path = PATH (fp, _)} <- get
  l <- getLn n
  case l of
    Nothing -> return Nothing
    Just (LINE (a, Nothing)) -> do
      setCM l' p lns
      return $ Just l'
      where
        p = PATH (fp, n)
        l' = LINE (a, Just $ FN p $ parse [a])
    Just (LINE _) -> return l

getLn :: Int -> ENVS (Maybe LINE)
getLn n = do
  ENV {lns, path = PATH (fp, _)} <- get
  getCM (PATH (fp, n)) lns

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

getvar :: String -> ENVS (Maybe ANY)
getvar k = do
  ENV {scope} <- get
  case M.lookup k scope of
    Nothing -> getgvar k
    v@(Just _) -> return v

getgvar :: String -> ENVS (Maybe ANY)
getgvar k = do
  ENV {gscope} <- get
  getCM k gscope

pushvar :: String -> String -> ENVS ()
pushvar = fvar getvar push

pushgvar :: String -> String -> ENVS ()
pushgvar = fvar getgvar push

evalvar :: String -> String -> ENVS ()
evalvar = fvar getvar eval

evalgvar :: String -> String -> ENVS ()
evalgvar = fvar getgvar eval

fvar :: (t1 -> ENVS (Maybe t2)) -> (t2 -> ENVS ()) -> String -> t1 -> ENVS ()
fvar g f c k =
  g k >>= \case
    Nothing -> cmd' c
    Just v -> f v

argN :: Int -> ([ANY] -> ENVS ()) -> ENVS ()
argN n f = do
  env@ENV {stack} <- get
  if isMin n stack
    then
      let (a, b) = splitAt n stack
       in do
            put env {stack = b}
            f $ reverse a
    else throwError $ "stack len < " ++ show n

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

modv1 :: (ANY -> ANY) -> ENVS ()
modv1 = mod1 . vec1

modv2 :: (ANY -> ANY -> ANY) -> ENVS ()
modv2 = mod2 . vec2

unENVS :: (Monad m) => ExceptT String (StateT s m) a -> s -> m s
unENVS f = execStateT do
  r <- runExceptT f
  case r of
    Left e -> error e
    Right a -> return a

setCM :: (MonadIO m, Hashable key) => value -> key -> CM.Map key value -> m ()
setCM v k = liftIO . atomically . CM.insert v k

getCM :: (MonadIO m, Hashable key) => key -> CM.Map key value -> m (Maybe value)
getCM k = liftIO . atomically . CM.lookup k