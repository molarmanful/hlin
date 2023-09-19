module Util where

import Data.Char (readLitChar)
import Data.Foldable (foldl', toList)
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
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

lsap1 :: (t1 -> t2) -> Seq t1 -> t2
lsap1 f (_ :|> a) = f a
lsap1 _ _ = undefined

lsap2 :: (t1 -> t1 -> t2) -> Seq t1 -> t2
lsap2 f (_ :|> a :|> b) = f a b
lsap2 _ _ = undefined

lsap3 :: (t1 -> t1 -> t1 -> t2) -> Seq t1 -> t2
lsap3 f (_ :|> a :|> b :|> c) = f a b c
lsap3 _ _ = undefined

unescText :: T.Text -> T.Text
unescText = T.pack . unescStr . T.unpack

unescStr :: String -> String
unescStr =
  fst . head . readP_to_S do
    s <- many $ readS_to_P readLitChar
    eof
    return s

seqtovec :: Seq a -> Vector a
seqtovec = V.fromList . toList

vectoseq :: Vector a -> Seq a
vectoseq = foldl' (|>) Seq.empty