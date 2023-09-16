module ANY.Func where

import ANY.Base
import Control.Applicative (Applicative (liftA2))
import Data.Align (Semialign (alignWith))
import Data.Number.CReal
import Data.These (fromThese)
import Data.Vector (Vector)
import Types
import Util

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

fNUM1 :: (CReal -> CReal) -> ANY -> ANY
fNUM1 f a = NUM $ f $ toNUMW a

fNUM2 :: (CReal -> CReal -> CReal) -> ANY -> ANY -> ANY
fNUM2 = acb2 NUM toNUMW

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