{-# OPTIONS_GHC -Wno-orphans #-}

module ANY.Func where

import ANY.Base
import Control.Applicative (Applicative (liftA2))
import Data.Align (Semialign (alignWith))
import Data.Foldable (Foldable (toList))
import qualified Data.List as L
import qualified Data.Map as M
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Data.These (fromThese)
import Data.Vector (Vector)
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
  show (RAT a) = show (numerator a) ++ "%" ++ show (denominator a)
  show (INT a) = show a
  show (NUM a) = show a
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
  BigN a / b = RAT $ toRATW a / toRATW b
  a / BigN b = toRAT a / b
  NUM a / b = NUM $ a / toNUMW b
  a / b = toFrac a / b

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
azipWith f a@(FN p _) b = toFN p $ azipWith f (toSEQ a) b
azipWith f a@(SEQ _) b = fSEQ2 (liftA2 f) a b
azipWith f a b@(SEQ _) = azipWith f (toSEQ a) b
azipWith f a b = fARR2 (liftA2 f) a b

azip :: ANY -> ANY -> ANY
azip = azipWith \a b -> SEQ [a, b]

azipAll :: ANY -> ANY -> ANY -> ANY -> ANY
azipAll da db a@(FN p _) b = toFN p $ azipAll da db (toSEQ a) b
azipAll da db a@(SEQ _) b = fSEQ2 (alignWith $ apair . fromThese da db) a b
azipAll da db a b@(SEQ _) = azipAll da db (toSEQ a) b
azipAll da db a b = fARR2 (alignWith (apair . fromThese da db)) a b

-- vectorizations

vec1 :: (ANY -> ANY) -> ANY -> ANY
vec1 f (Itr a) = amap (vec1 f) a
vec1 f a = f a

vec2 :: (ANY -> ANY -> ANY) -> ANY -> ANY -> ANY
vec2 f (Itr a) (Itr b) = azipWith (vec2 f) a b
vec2 f (Itr a) b = vec1 (`f` b) a
vec2 f a (Itr b) = vec1 (f a) b
vec2 f a b = f a b

-- convenience

fNum1 :: (forall a. Num a => a -> a) -> ANY -> ANY
fNum1 f (RAT a) = RAT $ f a
fNum1 f (INT a) = INT $ f a
fNum1 f (NUM a) = NUM $ f a
fNum1 f a = fNum1 f $ toNum a

fNum2 :: (forall a. Num a => a -> a -> a) -> ANY -> ANY -> ANY
fNum2 f (RAT a) b = RAT $ f a (toRATW b)
fNum2 f a b@(RAT _) = fNum2 f (toRAT a) b
fNum2 f (INT a) b = INT $ f a (toINTW b)
fNum2 f a b@(INT _) = fNum2 f (toINT a) b
fNum2 f (NUM a) b = NUM $ f a (toNUMW b)
fNum2 f a b@(NUM _) = fNum2 f (toNUM a) b
fNum2 f a b = fNum2 f (toNum a) b

fSTR1 :: (Text -> Text) -> ANY -> ANY
fSTR1 = acb STR toSTRW

fSTR2 :: (Text -> Text -> Text) -> ANY -> ANY -> ANY
fSTR2 = acb2 STR toSTRW

fSEQ1 :: ([ANY] -> [ANY]) -> ANY -> ANY
fSEQ1 = acb SEQ toSEQW

fSEQ2 :: ([ANY] -> [ANY] -> [ANY]) -> ANY -> ANY -> ANY
fSEQ2 = acb2 SEQ toSEQW

fARR1 :: (Vector ANY -> Vector ANY) -> ANY -> ANY
fARR1 = acb ARR toARRW

fARR2 :: (Vector ANY -> Vector ANY -> Vector ANY) -> ANY -> ANY -> ANY
fARR2 = acb2 ARR toARRW

apair :: (ANY, ANY) -> ANY
apair = SEQ . pair