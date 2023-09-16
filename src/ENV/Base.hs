{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ENV.Base where

import ANY
import Data.List (stripPrefix)
import Data.Map (Map, (!))
import qualified Data.Map as M
import GHC.Conc (atomically)
import qualified StmContainers.Map as CM
import Types
import Util

instance Show ENV where
  show :: ENV -> String
  show ENV {stack, code} = unwords $ show <$> [stack, code]

run :: String -> IO ENV
run s = loop $ return dENV {code = parse s}

loop :: IO ENV -> IO ENV
loop env = do
  e@ENV {code} <- env
  case code of
    [] -> return e
    x : xs -> loop $ choice x e {code = xs}

choice :: ANY -> ENV -> IO ENV
choice (CMD a) = cmd a
choice a = return . push a

cmd :: String -> ENV -> IO ENV
-- syntax/sugar
cmd (stripPrefix "=$$" -> Just k) env = arg1 (setgvar k) env
cmd (stripPrefix "=$" -> Just k) env = return $ arg1 (setvar k) env
-- flow
-- stack
cmd "dup" env = rmods1 (\a -> [a, a]) env
cmd "pop" env = rmods1 (const []) env
cmd "swap" env = rmods2 (\a b -> [b, a]) env
cmd "rot" env = rmods3 (\a b c -> [b, c, a]) env
cmd "rot_" env = rmods3 (\a b c -> [c, a, b]) env
cmd "dupd" env = rmods2 (\a b -> [a, a, b]) env
cmd "over" env = rmods2 (\a b -> [a, b, a]) env
cmd "ddup" env = rmods2 (\a b -> [a, b, a, b]) env
cmd "edup" env = rmods3 (\a b c -> [a, b, c, a, b, c]) env
cmd "nip" env = rmod2 (const id) env
cmd "ppop" env = rmods2 (\_ _ -> []) env
cmd "qpop" env = rmods3 (\_ _ _ -> []) env
cmd "swapd" env = rmods3 (\a b c -> [b, a, c]) env
cmd "tuck" env = rmods2 (\a b -> [b, a, b]) env
-- math
cmd "_" env = rmod1 (vec1 $ fNUM1 negate) env
cmd "+" env = rmod2 (vec2 $ fNUM2 (+)) env
cmd "-" env = rmod2 (vec2 $ fNUM2 (-)) env
cmd "*" env = rmod2 (vec2 $ fNUM2 (*)) env
cmd "/" env = rmod2 (vec2 $ fNUM2 (/)) env
cmd a _ = error $ "CMD \"" ++ a ++ "\" not found"

-- convenience

push :: ANY -> ENV -> ENV
push a env@ENV {stack} = env {stack = a : stack}

pushs :: [ANY] -> ENV -> ENV
pushs a env@ENV {stack} = env {stack = reverse a ++ stack}

setvar :: String -> ANY -> ENV -> ENV
setvar k v env@ENV {scope} = env {scope = M.insert k v scope}

setgvar :: String -> ANY -> ENV -> IO ENV
setgvar k v env@ENV {gscope} = do
  gs <- gscope
  atomically $ CM.insert v k gs
  return env

argN :: Int -> ([ANY] -> ENV -> b) -> ENV -> b
argN n f env@ENV {stack} =
  if isMin n stack
    then
      let (a, b) = splitAt n stack
       in f (reverse a) env {stack = b}
    else error $ "stack length < " ++ show n

modN :: Int -> ([ANY] -> ANY) -> ENV -> ENV
modN n f = argN n $ push . f

rmodN :: Int -> ([ANY] -> ANY) -> ENV -> IO ENV
rmodN n f = return . modN n f

modsN :: Int -> ([ANY] -> [ANY]) -> ENV -> ENV
modsN n f = argN n $ pushs . f

rmodsN :: Int -> ([ANY] -> [ANY]) -> ENV -> IO ENV
rmodsN n f = return . modsN n f

arg1 :: (ANY -> ENV -> b) -> ENV -> b
arg1 = argN 1 . lsap1

arg2 :: (ANY -> ANY -> ENV -> b) -> ENV -> b
arg2 = argN 1 . lsap2

mod1 :: (ANY -> ANY) -> ENV -> ENV
mod1 = modN 1 . lsap1

mod2 :: (ANY -> ANY -> ANY) -> ENV -> ENV
mod2 = modN 2 . lsap2

mod3 :: (ANY -> ANY -> ANY -> ANY) -> ENV -> ENV
mod3 = modN 3 . lsap3

mods1 :: (ANY -> [ANY]) -> ENV -> ENV
mods1 = modsN 1 . lsap1

mods2 :: (ANY -> ANY -> [ANY]) -> ENV -> ENV
mods2 = modsN 2 . lsap2

mods3 :: (ANY -> ANY -> ANY -> [ANY]) -> ENV -> ENV
mods3 = modsN 3 . lsap3

rmod1 :: (ANY -> ANY) -> ENV -> IO ENV
rmod1 = rmodN 1 . lsap1

rmod2 :: (ANY -> ANY -> ANY) -> ENV -> IO ENV
rmod2 = rmodN 2 . lsap2

rmod3 :: (ANY -> ANY -> ANY -> ANY) -> ENV -> IO ENV
rmod3 = rmodN 3 . lsap3

rmods1 :: (ANY -> [ANY]) -> ENV -> IO ENV
rmods1 = rmodsN 1 . lsap1

rmods2 :: (ANY -> ANY -> [ANY]) -> ENV -> IO ENV
rmods2 = rmodsN 2 . lsap2

rmods3 :: (ANY -> ANY -> ANY -> [ANY]) -> ENV -> IO ENV
rmods3 = rmodsN 3 . lsap3