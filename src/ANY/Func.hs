{-# OPTIONS_GHC -Wno-orphans #-}

module ANY.Func where

import ANY.Base
import Control.Applicative (Applicative (liftA2))
import Control.Monad (zipWithM)
import Control.Monad.ListM (zipWithM3)
import Data.Align (Semialign (alignWith))
import Data.Foldable (Foldable (toList))
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (Text)
import Data.These (fromThese)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Real (Ratio ((:%)))
import Types
import Util

instance Show ANY where
  show :: ANY -> String
  show UN = "UN"
  show (TF a)
    | a = "$T"
    | otherwise = "$F"
  show (STR a) = show a
  show (CMD a) = a
  show (NUM a) = show a
  show (RAT (n :% d)) = show n ++ "%" ++ show d
  show (INT a) = show a
  show (FN _ a) = "( " ++ unwords (show <$> a) ++ " )"
  show (SEQ a) = "[ " ++ unwords (show <$> a) ++ " ]`"
  show (ARR a) = "[ " ++ unwords (toList $ show <$> a) ++ " ]"
  show (MAP a) =
    "{ "
      ++ unwords
        ( L.intercalate " => " . map show . pair
            <$> M.toList a
        )
      ++ " }"

instance Num ANY where
  fromInteger :: Integer -> ANY
  fromInteger = INT

  signum :: ANY -> ANY
  signum = fNum1 signum

  abs :: ANY -> ANY
  abs = fNum1 abs

  (+) :: ANY -> ANY -> ANY
  (+) = fNum2 (+)

  (-) :: ANY -> ANY -> ANY
  (-) = fNum2 (-)

  (*) :: ANY -> ANY -> ANY
  (*) = fNum2 (*)

instance Fractional ANY where
  fromRational :: Rational -> ANY
  fromRational = RAT

  (/) :: ANY -> ANY -> ANY
  NUM a / b = NUM $ a / toNUMW b
  a / b@(NUM _) = toNUM a / b
  BigN a / b = RAT $ toRATW a / toRATW b
  a / BigN b = toRAT a / b
  a / b = toFrac a / b

instance Real ANY where
  toRational :: ANY -> Rational
  toRational = toRATW

instance Enum ANY where
  toEnum :: Int -> ANY
  toEnum = INT . toInteger

  fromEnum :: ANY -> Int
  fromEnum = fromInteger . toINTW

instance Integral ANY where
  toInteger :: ANY -> Integer
  toInteger = toINTW

  quotRem :: ANY -> ANY -> (ANY, ANY)
  quotRem a b = (acb2 INT toINTW quot a b, acb2 INT toINTW rem a b)

instance RealFrac ANY where
  properFraction :: Integral b => ANY -> (b, ANY)
  properFraction (INT a) = (fromInteger a, INT 0)
  properFraction (NUM a) = case properFraction a of (x, y) -> (x, NUM y)
  properFraction a = case properFraction $ toRATW a of (x, y) -> (x, RAT y)

instance Semigroup ANY where
  (<>) :: ANY -> ANY -> ANY
  SEQ a <> b = SEQ $ a <> toSEQW b
  a <> b@(SEQ _) = toSEQ a <> b
  FN p a <> b = FN p $ a <> toSEQW b
  a <> b@(FN p _) = toFN p a <> b
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
azipWith f a@(SEQ _) b = fSEQ2 (liftA2 f) a b
azipWith f a b@(SEQ _) = fSEQ2 (liftA2 f) a b
azipWith f a b = fARR2 (liftA2 f) a b

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

apair :: (ANY, ANY) -> ANY
apair = SEQ . pair