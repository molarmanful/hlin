{-# OPTIONS_GHC -Wno-orphans #-}

module ANY.Func where

import ANY.Base
import Control.Monad (zipWithM)
import Control.Monad.ListM (zipWithM3)
import Data.Align (Semialign (alignWith))
import Data.Text (Text)
import qualified Data.Text as T
import Data.These (fromThese)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Base (stimes)
import Types
import Util

instance Num ANY where
  fromInteger = INT
  signum = fNum1 signum
  abs = fNum1 abs
  (+) = fNum2 (+)
  (-) = fNum2 (-)
  (*) = fNum2 (*)

instance Fractional ANY where
  fromRational = RAT
  NUM a / b = NUM $ a / toNUMW b
  a / b@(NUM _) = toNUM a / b
  BigN a / b = toBigN $ toRATW a / toRATW b
  a / BigN b = toRAT a / b
  a / b = toFrac a / b

instance Real ANY where
  toRational = toRATW

instance Enum ANY where
  toEnum = INT . toInteger
  fromEnum = toInt

instance Integral ANY where
  toInteger = toINTW
  quotRem a b = (acb2 INT toINTW quot a b, acb2 INT toINTW rem a b)

instance RealFrac ANY where
  properFraction (INT a) = (fromInteger a, INT 0)
  properFraction (NUM a) = case properFraction a of (x, y) -> (x, NUM y)
  properFraction a = case properFraction $ toRATW a of (x, y) -> (x, RAT y)

instance Floating ANY where
  pi = NUM pi
  exp = fNUM1 exp
  log = fNUM1 log
  sin = fNUM1 sin
  cos = fNUM1 cos
  asin = fNUM1 asin
  acos = fNUM1 acos
  atan = fNUM1 atan
  sinh = fNUM1 sinh
  cosh = fNUM1 cosh
  asinh = fNUM1 asinh
  acosh = fNUM1 acosh
  atanh = fNUM1 atanh

instance Semigroup ANY where
  FN p a <> b = FN p $ a <> toSEQW b
  a <> b@(FN p _) = toFN p a <> b
  SEQ a <> b = SEQ $ a <> toSEQW b
  a <> b@(SEQ _) = toSEQ a <> b
  MAP a <> MAP b = MAP $ a <> b
  Itr a <> b = ARR $ toARRW a <> toARRW b
  a <> Itr b = toARR a <> b
  STR a <> b = STR $ a <> toSTRW b
  a <> b@(STR _) = toSTR a <> b
  a <> b = SEQ [a, b]

-- functors

amap :: (ANY -> ANY) -> ANY -> ANY
amap f (FN p a) = FN p $ f <$> a
amap f (SEQ a) = SEQ $ f <$> a
amap f a = fARR1 (f <$>) a

azipWith :: (ANY -> ANY -> ANY) -> ANY -> ANY -> ANY
azipWith f a@(FN p _) b@(FN _ _) = toFN p $ azipWith f (toSEQ a) b
azipWith f a@(SEQ _) b = fSEQ2 (zipWith f) a b
azipWith f a b@(SEQ _) = fSEQ2 (zipWith f) a b
azipWith f a b = fARR2 (V.zipWith f) a b

azipWith3 :: (ANY -> ANY -> ANY -> ANY) -> ANY -> ANY -> ANY -> ANY
azipWith3 f a@(FN p _) b@(FN _ _) c@(FN _ _) = toFN p $ azipWith3 f (toSEQ a) b c
azipWith3 f a@(SEQ _) b c = fSEQ3 (zipWith3 f) a b c
azipWith3 f a b@(SEQ _) c = fSEQ3 (zipWith3 f) a b c
azipWith3 f a b c@(SEQ _) = fSEQ3 (zipWith3 f) a b c
azipWith3 f a b c = fARR3 (V.zipWith3 f) a b c

azip :: ANY -> ANY -> ANY
azip = azipWith \a b -> SEQ [a, b]

azipAll :: ANY -> ANY -> ANY -> ANY -> ANY
azipAll da db a@(FN p _) b@(FN _ _) = toFN p $ azipAll da db (toSEQ a) b
azipAll da db a@(SEQ _) b = fSEQ2 (alignWith $ apair . fromThese da db) a b
azipAll da db a b@(SEQ _) = azipAll da db (toSEQ a) b
azipAll da db a b = fARR2 (alignWith (apair . fromThese da db)) a b

-- monads

amapM :: Monad f => (ANY -> f ANY) -> ANY -> f ANY
amapM f (SEQ a) = SEQ <$> mapM f a
amapM f (FN p a) = FN p <$> mapM f a
amapM f a = ARR <$> mapM f (toARRW a)

azipWithM :: Monad f => (ANY -> ANY -> f ANY) -> ANY -> ANY -> f ANY
azipWithM f a@(FN p _) b@(FN _ _) = toFN p <$> azipWithM f (toSEQ a) b
azipWithM f (SEQ a) b = SEQ <$> zipWithM f a (toSEQW b)
azipWithM f a b@(SEQ _) = azipWithM f (toSEQ a) b
azipWithM f a b = ARR <$> V.zipWithM f (toARRW a) (toARRW b)

azipWithM3 :: Monad f => (ANY -> ANY -> ANY -> f ANY) -> ANY -> ANY -> ANY -> f ANY
azipWithM3 f a@(FN p _) b@(FN _ _) c@(FN _ _) = toFN p <$> azipWithM3 f (toSEQ a) b c
azipWithM3 f (SEQ a) b c = SEQ <$> zipWithM3 f a (toSEQW b) (toSEQW c)
azipWithM3 f a b@(SEQ _) c = azipWithM3 f (toSEQ a) b c
azipWithM3 f a b c@(SEQ _) = azipWithM3 f (toSEQ a) b c
azipWithM3 f a b c = toARR <$> azipWithM3 f (toSEQ a) b c

-- vectorizations

vec1 :: (ANY -> ANY) -> ANY -> ANY
vec1 f (Itr a) = amap (vec1 f) a
vec1 f a = f a

vec2 :: (ANY -> ANY -> ANY) -> ANY -> ANY -> ANY
vec2 f (Itr a) (Itr b) = azipWith (vec2 f) a b
vec2 f (Itr a) b = vec1 (`f` b) a
vec2 f a (Itr b) = vec1 (f a) b
vec2 f a b = f a b

vec3 :: (ANY -> ANY -> ANY -> ANY) -> ANY -> ANY -> ANY -> ANY
vec3 f (Itr a) (Itr b) (Itr c) = azipWith3 (vec3 f) a b c
vec3 f (Itr a) (Itr b) c = vec2 (\x y -> f x y c) a b
vec3 f (Itr a) b (Itr c) = vec2 (`f` b) a c
vec3 f a (Itr b) (Itr c) = vec2 (f a) b c
vec3 f (Itr a) b c = vec1 (\x -> f x b c) a
vec3 f a (Itr b) c = vec1 (flip (f a) c) b
vec3 f a b (Itr c) = vec1 (f a b) c
vec3 f a b c = f a b c

vecM1 :: Monad f => (ANY -> f ANY) -> ANY -> f ANY
vecM1 f (Itr a) = amapM (vecM1 f) a
vecM1 f a = f a

vecM2 :: Monad f => (ANY -> ANY -> f ANY) -> ANY -> ANY -> f ANY
vecM2 f (Itr a) (Itr b) = azipWithM (vecM2 f) a b
vecM2 f (Itr a) b = vecM1 (`f` b) a
vecM2 f a (Itr b) = vecM1 (f a) b
vecM2 f a b = f a b

vecM3 :: Monad f => (ANY -> ANY -> ANY -> f ANY) -> ANY -> ANY -> ANY -> f ANY
vecM3 f (Itr a) (Itr b) (Itr c) = azipWithM3 (vecM3 f) a b c
vecM3 f (Itr a) (Itr b) c = vecM2 (\x y -> f x y c) a b
vecM3 f (Itr a) b (Itr c) = vecM2 (`f` b) a c
vecM3 f a (Itr b) (Itr c) = vecM2 (f a) b c
vecM3 f (Itr a) b c = vecM1 (\x -> f x b c) a
vecM3 f a (Itr b) c = vecM1 (flip (f a) c) b
vecM3 f a b (Itr c) = vecM1 (f a b) c
vecM3 f a b c = f a b c

-- convenience

fNum1 :: (forall a. Num a => a -> a) -> ANY -> ANY
fNum1 f (RAT a) = RAT $ f a
fNum1 f (INT a) = INT $ f a
fNum1 f (NUM a) = NUM $ f a
fNum1 f a = fNum1 f $ toNum a

fNum2 :: (forall a. Num a => a -> a -> a) -> ANY -> ANY -> ANY
fNum2 f (NUM a) b = NUM $ f a (toNUMW b)
fNum2 f a b@(NUM _) = fNum2 f (toNUM a) b
fNum2 f (RAT a) b = RAT $ f a (toRATW b)
fNum2 f a b@(RAT _) = fNum2 f (toRAT a) b
fNum2 f (INT a) b = INT $ f a (toINTW b)
fNum2 f a b@(INT _) = fNum2 f (toINT a) b
fNum2 f a b = fNum2 f (toNum a) b

fNUM1 :: (Double -> Double) -> ANY -> ANY
fNUM1 = acb NUM toNUMW

fNUM2 :: (Double -> Double -> Double) -> ANY -> ANY -> ANY
fNUM2 = acb2 NUM toNUMW

fSTR1 :: (Text -> Text) -> ANY -> ANY
fSTR1 = acb STR toSTRW

fSTR2 :: (Text -> Text -> Text) -> ANY -> ANY -> ANY
fSTR2 = acb2 STR toSTRW

fSTR3 :: (Text -> Text -> Text -> Text) -> ANY -> ANY -> ANY -> ANY
fSTR3 = acb3 STR toSTRW

fSEQ1 :: ([ANY] -> [ANY]) -> ANY -> ANY
fSEQ1 = acb SEQ toSEQW

fSEQ2 :: ([ANY] -> [ANY] -> [ANY]) -> ANY -> ANY -> ANY
fSEQ2 = acb2 SEQ toSEQW

fSEQ3 :: ([ANY] -> [ANY] -> [ANY] -> [ANY]) -> ANY -> ANY -> ANY -> ANY
fSEQ3 = acb3 SEQ toSEQW

fARR1 :: (Vector ANY -> Vector ANY) -> ANY -> ANY
fARR1 = acb ARR toARRW

fARR2 :: (Vector ANY -> Vector ANY -> Vector ANY) -> ANY -> ANY -> ANY
fARR2 = acb2 ARR toARRW

fARR3 :: (Vector ANY -> Vector ANY -> Vector ANY -> Vector ANY) -> ANY -> ANY -> ANY -> ANY
fARR3 = acb3 ARR toARRW

fTF1 :: (Bool -> Bool) -> ANY -> ANY
fTF1 = acb TF toTFW

fTF2 :: (Bool -> Bool -> Bool) -> ANY -> ANY -> ANY
fTF2 = acb2 TF toTFW

fTF1' :: (a -> Bool) -> a -> ANY
fTF1' = (TF .)

fTF2' :: (t1 -> t2 -> Bool) -> t1 -> t2 -> ANY
fTF2' f a b = TF $ f a b

apair :: (ANY, ANY) -> ANY
apair = SEQ . pair

apow :: ANY -> ANY -> ANY
apow (INT a) (INT b)
  | b < 0 = RAT (toRational a ^^ b)
  | otherwise = INT (a ^ b)
apow (NUM a) (INT b) = NUM a ^^ b
apow a (INT b) = RAT (toRATW a ^^ b)
apow (Num a) (Num b) = fNUM2 (**) a b
apow a b = apow (toNum a) $ toNum b

afmod :: RealFrac a => a -> a -> a
afmod a b
  | m < 0 = m + b
  | otherwise = m
  where
    m = afrem a b

afrem :: RealFrac a => a -> a -> a
afrem a b = a - b * fromInteger (truncate (a / b))

atimes :: Integral b => b -> ANY -> ANY
atimes n
  | n <= 0 = (`matchT` UN)
  | otherwise = stimes n

atake :: Int -> ANY -> ANY
atake n a | n < 0 = adrop (n + alength a) a
atake n (STR a) = STR $ T.take n a
atake n a@(Listy x) = matchT a $ SEQ $ take n x
atake n (ARR a) = ARR $ V.take n a
atake n a = matchT a $ fSEQ1 (take n) a

adrop :: Int -> ANY -> ANY
adrop n a | n < 0 = atake (n + alength a) a
adrop n (STR a) = STR $ T.drop n a
adrop n a@(Listy x) = matchT a $ SEQ $ drop n x
adrop n (ARR a) = ARR $ V.drop n a
adrop n a = matchT a $ fSEQ1 (drop n) a