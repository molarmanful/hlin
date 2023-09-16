{-# OPTIONS_GHC -Wno-orphans #-}

module ENV.Base where

import ANY
import Types
import Util

instance Show ENV where
  show :: ENV -> String
  show (ENV {stack, code}) = unwords $ show <$> [stack, code]

run :: String -> ENV
run s = loop dENV {code = parse s}

loop :: ENV -> ENV
loop env@(ENV {code}) = case code of
  [] -> env
  x : xs -> loop $ choice x env {code = xs}

choice :: ANY -> ENV -> ENV
choice (CMD a) = cmd a
choice a = push a

cmd :: String -> ENV -> ENV
-- stack
cmd "dup" env = mods1 (\a -> [a, a]) env
cmd "pop" env = mods1 (const []) env
cmd "swap" env = mods2 (\a b -> [b, a]) env
cmd "rot" env = mods3 (\a b c -> [b, c, a]) env
cmd "rot_" env = mods3 (\a b c -> [c, a, b]) env
cmd "dupd" env = mods2 (\a b -> [a, a, b]) env
cmd "over" env = mods2 (\a b -> [a, b, a]) env
cmd "ddup" env = mods2 (\a b -> [a, b, a, b]) env
cmd "edup" env = mods3 (\a b c -> [a, b, c, a, b, c]) env
cmd "nip" env = mod2 (const id) env
cmd "ppop" env = mods2 (\_ _ -> []) env
cmd "qpop" env = mods3 (\_ _ _ -> []) env
cmd "swapd" env = mods3 (\a b c -> [b, a, c]) env
cmd "tuck" env = mods2 (\a b -> [b, a, b]) env
-- math
cmd "_" env = mod1 (vec1 $ fNUM1 negate) env
cmd "+" env = mod2 (vec2 $ fNUM2 (+)) env
cmd "-" env = mod2 (vec2 $ fNUM2 (-)) env
cmd "*" env = mod2 (vec2 $ fNUM2 (*)) env
cmd "/" env = mod2 (vec2 $ fNUM2 (/)) env
cmd a _ = error $ "CMD \"" ++ a ++ "\" not found"

-- convenience

push :: ANY -> ENV -> ENV
push a env@(ENV {stack}) = env {stack = a : stack}

pushs :: [ANY] -> ENV -> ENV
pushs a env@(ENV {stack}) = env {stack = reverse a ++ stack}

argN :: Int -> ([ANY] -> ENV -> b) -> ENV -> b
argN n f env@(ENV {stack}) =
  if isMin n stack
    then
      let (a, b) = splitAt n stack
       in f (reverse a) env {stack = b}
    else error $ "stack length < " ++ show n

modsN :: Int -> ([ANY] -> [ANY]) -> ENV -> ENV
modsN n f = argN n $ pushs . f

modN :: Int -> ([ANY] -> ANY) -> ENV -> ENV
modN n f = argN n $ push . f

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