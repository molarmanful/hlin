module Util where

isMin :: (Eq t, Num t) => t -> [a] -> Bool
isMin 0 _ = True
isMin _ [] = False
isMin n (_ : xs) = isMin (n - 1) xs

pair :: (a, a) -> [a]
pair (a, b) = [a, b]

acb :: (b1 -> c) -> (a -> b) -> (b -> b1) -> a -> c
acb a b c = a . c . b

acb2 :: (a -> b) -> (t1 -> t2) -> (t2 -> t2 -> a) -> t1 -> t1 -> b
acb2 a b c d e = a $ c (b d) (b e)

lsap1 :: (b -> c) -> [b] -> c
lsap1 = (. head)

lsap2 :: (t1 -> t1 -> t2) -> [t1] -> t2
lsap2 f [a, b] = f a b
lsap2 _ _ = undefined

lsap3 :: (t1 -> t1 -> t1 -> t2) -> [t1] -> t2
lsap3 f [a, b, c] = f a b c
lsap3 _ _ = undefined