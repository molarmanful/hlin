module ENV.Base where

import ANY
import Types
import Util

loop :: ENV -> ENV
loop env@(ENV {code}) = case code of
  [] -> env
  x : xs -> choice x $ env {code = xs}
    where
      choice (CMD a) e = cmd a e
      choice a e = push a e

push :: ANY -> ENV -> ENV
push a env@(ENV {stack}) = env {stack = a : stack}

pushs :: [ANY] -> ENV -> ENV
pushs a env@(ENV {stack}) = env {stack = a ++ stack}

argN :: Int -> ([ANY] -> ENV -> b) -> ENV -> b
argN n f env@(ENV {stack}) =
  if isMin n stack
    then
      let (a, b) = splitAt n stack
       in f (reverse a) $ env {stack = b}
    else error $ "stack length < " ++ show n

arg1 :: (ANY -> ENV -> b) -> ENV -> b
arg1 = argN 1 . lsap1

arg2 :: (ANY -> ANY -> ENV -> b) -> ENV -> b
arg2 = argN 1 . lsap2

modN :: Int -> ([ANY] -> ANY) -> ENV -> ENV
modN n f = argN n $ push . f

mod1 :: (ANY -> ANY) -> ENV -> ENV
mod1 = modN 1 . lsap1

mod2 :: (ANY -> ANY -> ANY) -> ENV -> ENV
mod2 = modN 2 . lsap2

mod3 :: (ANY -> ANY -> ANY -> ANY) -> ENV -> ENV
mod3 = modN 3 . lsap3

cmd :: String -> ENV -> ENV
cmd "_" env = mod1 (vec1 $ fNUM1 negate) env
cmd "+" env = mod2 (vec2 $ fNUM2 (+)) env
cmd "-" env = mod2 (vec2 $ fNUM2 (-)) env
cmd "*" env = mod2 (vec2 $ fNUM2 (*)) env
cmd "/" env = mod2 (vec2 $ fNUM2 (/)) env
cmd a _ = error $ "CMD \"" ++ a ++ "\" not found"