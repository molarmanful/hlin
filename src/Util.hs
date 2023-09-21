module Util where

import Data.Char (ord, readLitChar)
import Data.Foldable (foldl', toList)
import Data.Ratio (denominator)
import Data.Sequence (Seq (..), (!?), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.ParserCombinators.ReadP (eof, many, readP_to_S, readS_to_P)

isMin :: (Eq t, Num t) => t -> [a] -> Bool
isMin n = (> GT) . cmpNL n

cmpLN :: (Eq a1, Num a1) => [a2] -> a1 -> Ordering
cmpLN a = compare EQ . flip cmpNL a

cmpNL :: (Eq t, Num t) => t -> [a] -> Ordering
cmpNL 0 [] = EQ
cmpNL 0 _ = LT
cmpNL _ [] = GT
cmpNL n (_ : xs) = cmpNL (n - 1) xs

cmpSN a = compare EQ . flip cmpNS a

cmpNS _ "" = GT
cmpNS n s = compare n $ fromIntegral $ ord $ head s

pair :: (a, a) -> [a]
pair (a, b) = [a, b]

acb :: (b1 -> c) -> (a -> b) -> (b -> b1) -> a -> c
acb a b c = a . c . b

acb2 :: (a -> b) -> (t1 -> t2) -> (t2 -> t2 -> a) -> t1 -> t1 -> b
acb2 a b c d e = a $ c (b d) (b e)

acb3 :: (a -> b) -> (t1 -> t2) -> (t2 -> t2 -> t2 -> a) -> t1 -> t1 -> t1 -> b
acb3 a b c d e f = a $ c (b d) (b e) (b f)

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

canDouble :: Rational -> Bool
canDouble a = a == toRational (fromRational a :: Double)

canInteger :: Rational -> Bool
canInteger = (== 1) . denominator

fromCmp :: Ordering -> Integer
fromCmp = \case
  LT -> -1
  EQ -> 0
  GT -> 1

toCmp :: (Ord a, Num a) => a -> Ordering
toCmp a =
  if
      | a < 0 -> LT
      | a > 0 -> GT
      | otherwise -> EQ

iinv :: Foldable t => Int -> t a -> Int
iinv i s = (-i - 1) `mod` length s