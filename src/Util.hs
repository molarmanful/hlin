module Util where

import Data.Char (readLitChar)
import qualified Data.Text as T
import Text.ParserCombinators.ReadP (eof, many, readP_to_S, readS_to_P)

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

unescText :: T.Text -> T.Text
unescText = T.pack . unescStr . T.unpack

unescStr :: String -> String
unescStr =
  fst . head . readP_to_S do
    s <- many $ readS_to_P readLitChar
    eof
    return s