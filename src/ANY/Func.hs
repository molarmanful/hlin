{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ANY.Func where

import ANY.Base
import Control.Applicative
import Data.Align
import qualified Data.Map as M
import Data.Number.CReal
import qualified Data.Text as T
import Data.These
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V
import Types

-- functors

amap :: (ANY -> ANY) -> ANY -> ANY
amap f (FN p a) = FN p $ f <$> a
amap f (SEQ a) = SEQ $ f <$> a
amap f (ARR a) = ARR $ f <$> a
amap f a = f $ toARR a

azipWith :: (ANY -> ANY -> ANY) -> ANY -> ANY -> ANY
azipWith f a@(FN p _) b = toFN p $ azipWith f (toSEQ a) b
azipWith f (SEQ a) b = SEQ $ liftA2 f a $ toSEQW b
azipWith f a b@(SEQ _) = azipWith f (toSEQ a) b
azipWith f a b = ARR $ liftA2 f (toARRW a) (toARRW b)

azip :: ANY -> ANY -> ANY
azip = azipWith $ \a b -> SEQ [a, b]

azipAll :: ANY -> ANY -> ANY -> ANY -> ANY
azipAll da db a@(FN p _) b = toFN p $ azipAll da db (toSEQ a) b
azipAll da db (SEQ a) b = SEQ $ alignWith (apair . fromThese da db) a $ toSEQW b
azipAll da db a b@(SEQ _) = azipAll da db (toSEQ a) b
azipAll da db a b = ARR $ alignWith (apair . fromThese da db) (toARRW a) (toARRW b)

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
fNUM2 f a b = NUM $ f (toNUMW a) (toNUMW b)

apair :: (ANY, ANY) -> ANY
apair = SEQ . pair

pair :: (a, a) -> [a]
pair (a, b) = [a, b]