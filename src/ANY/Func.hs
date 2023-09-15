module ANY.Func where

import ANY.Base
import Control.Applicative (Applicative (liftA2))
import Data.Align (Semialign (alignWith))
import qualified Data.Map as M
import Data.Number.CReal
import qualified Data.Text as T
import Data.These (fromThese)
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V
import Types

-- functors

amap :: (ANY -> ANY) -> ANY -> ANY
amap f (FN p a) = FN p $ f <$> a
amap f (SEQ a) = SEQ $ f <$> a
amap f a = fARR (f <$>) a

azipWith :: (ANY -> ANY -> ANY) -> ANY -> ANY -> ANY
azipWith f a@(FN p _) b = toFN p $ azipWith f (toSEQ a) b
azipWith f a@(SEQ _) b = fSEQ2 (liftA2 f) a b
azipWith f a b@(SEQ _) = azipWith f (toSEQ a) b
azipWith f a b = fARR2 (liftA2 f) a b

azip :: ANY -> ANY -> ANY
azip = azipWith $ \a b -> SEQ [a, b]

azipAll :: ANY -> ANY -> ANY -> ANY -> ANY
azipAll da db a@(FN p _) b = toFN p $ azipAll da db (toSEQ a) b
azipAll da db a@(SEQ _) b = fSEQ2 (alignWith $ apair . fromThese da db) a b
azipAll da db a b@(SEQ _) = azipAll da db (toSEQ a) b
azipAll da db a b = fARR2 (alignWith (apair . fromThese da db)) a b

-- vectorizations

vec :: (ANY -> ANY) -> ANY -> ANY
vec f (Itr a) = amap (vec f) a
vec f a = f a

vec2 :: (ANY -> ANY -> ANY) -> ANY -> ANY -> ANY
vec2 f (Itr a) (Itr b) = azipWith (vec2 f) a b
vec2 f (Itr a) b = vec (`f` b) a
vec2 f a (Itr b) = vec (f a) b
vec2 f a b = f a b

-- convenience

fNUM :: (CReal -> CReal) -> ANY -> ANY
fNUM f a = NUM $ f $ toNUMW a

fNUM2 :: (CReal -> CReal -> CReal) -> ANY -> ANY -> ANY
fNUM2 = acb2 NUM toNUMW

fSEQ :: ([ANY] -> [ANY]) -> ANY -> ANY
fSEQ = acb SEQ toSEQW

fSEQ2 :: ([ANY] -> [ANY] -> [ANY]) -> ANY -> ANY -> ANY
fSEQ2 = acb2 SEQ toSEQW

fARR :: (Vector ANY -> Vector ANY) -> ANY -> ANY
fARR = acb ARR toARRW

fARR2 :: (Vector ANY -> Vector ANY -> Vector ANY) -> ANY -> ANY -> ANY
fARR2 = acb2 ARR toARRW

apair :: (ANY, ANY) -> ANY
apair = SEQ . pair

pair :: (a, a) -> [a]
pair (a, b) = [a, b]

acb :: (b1 -> c) -> (a -> b) -> (b -> b1) -> a -> c
acb a b c = a . c . b

acb2 :: (a -> b) -> (t1 -> t2) -> (t2 -> t2 -> a) -> t1 -> t1 -> b
acb2 a b c d e = a $ c (b d) (b e)