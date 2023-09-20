{-# LANGUAGE ViewPatterns #-}

module ANY.Base where

import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Parser (parse)
import Text.Read (readMaybe)
import Types
import Util

-- conversions

toTF :: ANY -> ANY
toTF a = case a of
  TF _ -> a
  _ -> TF $ toTFW a

toSTR :: ANY -> ANY
toSTR a = case a of
  STR _ -> a
  _ -> STR $ toSTRW a

toRAT :: ANY -> ANY
toRAT a = case a of
  RAT _ -> a
  _ -> RAT $ toRATW a

toINT :: ANY -> ANY
toINT a = case a of
  INT _ -> a
  _ -> INT $ toINTW a

toNUM :: ANY -> ANY
toNUM a = case a of
  NUM _ -> a
  _ -> NUM $ toNUMW a

toFN :: PATH -> ANY -> ANY
toFN p a = case a of
  FN _ _ -> a
  _ -> FN p $ toFNW a

toSEQ :: ANY -> ANY
toSEQ a = case a of
  SEQ _ -> a
  _ -> SEQ $ toSEQW a

toARR :: ANY -> ANY
toARR a = case a of
  ARR _ -> a
  _ -> ARR $ toARRW a

toMAP :: ANY -> ANY
toMAP a = case a of
  (MAP _) -> a
  _ -> MAP $ toMAPW a

toNum :: ANY -> ANY
toNum a = ratNum (txtRat $ toSTRW a :: Rational)

toFrac :: ANY -> ANY
toFrac a@(Frac _) = a
toFrac a@(INT _) = toRAT a
toFrac a = toRAT a

ratNum :: Rational -> ANY
ratNum n =
  if
      | canInteger n -> INT $ truncate n
      | canDouble n -> NUM $ realToFrac n
      | otherwise -> RAT n

toTFW :: ANY -> Bool
toTFW UN = False
toTFW (TF a) = a
toTFW (RAT a) = a == 0
toTFW (INT a) = a == 0
toTFW (NUM a) = a == 0
toTFW (STR a) = not $ T.null a
toTFW (CMD a) = not $ null a
toTFW (FN _ a) = not $ null a
toTFW (SEQ a) = not $ null a
toTFW (ARR a) = not $ null a
toTFW (MAP a) = not $ null a

toSTRW :: ANY -> Text
toSTRW UN = T.empty
toSTRW (TF a) = T.pack $ show $ fromEnum a
toSTRW (RAT a) = T.pack $ show a
toSTRW (INT a) = T.pack $ show a
toSTRW (NUM a) = T.pack $ show a
toSTRW (STR a) = a
toSTRW (CMD a) = T.pack a
toSTRW (FN _ a) = T.intercalate " " $ toSTRW <$> a
toSTRW (SEQ a) = mconcat $ toSTRW <$> a
toSTRW a@(ARR _) = toSTRW $ toSEQ a
toSTRW (MAP a) =
  T.intercalate "\n" $
    T.intercalate " " . map toSTRW . pair
      <$> M.toList a

toStr :: ANY -> String
toStr (CMD a) = a
toStr a = T.unpack $ toSTRW a

toNUMW :: ANY -> Double
toNUMW (NUM a) = a
toNUMW a = realToFrac $ toRATW a

toINTW :: ANY -> Integer
toINTW (INT a) = a
toINTW a = truncate $ toRATW a

toRATW :: ANY -> Rational
toRATW UN = 0
toRATW (TF a) = fromIntegral $ fromEnum a
toRATW (RAT a) = a
toRATW (INT a) = fromIntegral a
toRATW (NUM a) = realToFrac a
toRATW (STR a) = txtRat a
toRATW (FN _ a) = toRATW $ SEQ a
toRATW a = toRATW $ toSTR a

toInt :: ANY -> Int
toInt = truncate . toNUMW

toFNW :: ANY -> [ANY]
toFNW (STR a) = parse [T.unpack a]
toFNW (FN _ a) = a
toFNW (SEQ a) = a
toFNW a = [a]

toSEQW :: ANY -> [ANY]
toSEQW UN = []
toSEQW (STR a) = STR . T.singleton <$> T.unpack a
toSEQW (FN _ a) = a
toSEQW (SEQ a) = a
toSEQW (ARR a) = toList a
toSEQW a = [a]

toARRW :: ANY -> Vector ANY
toARRW UN = V.empty
toARRW (ARR a) = a
toARRW a = V.fromList $ toSEQW a

toMAPW :: ANY -> Map ANY ANY
toMAPW UN = M.empty
toMAPW (MAP a) = a
toMAPW a = M.fromList $ f . toSEQW =<< toSEQW a
  where
    f [] = []
    f (x : y : _) = [(x, y)]
    f (x : _) = [(x, UN)]

seqtoARR :: Seq ANY -> ANY
seqtoARR = ARR . seqtovec

seqfromARR :: ANY -> Seq ANY
seqfromARR = vectoseq . toARRW

txtRat :: Text -> Rational
txtRat a = case readMaybe $ T.unpack a of
  Nothing -> error "bad num"
  Just x -> x

-- patterns

pattern Num :: ANY -> ANY
pattern Num a <- (getNum -> Just a)

getNum :: ANY -> Maybe ANY
getNum a@(Frac _) = Just a
getNum a@(NUM _) = Just a
getNum _ = Nothing

pattern Frac :: ANY -> ANY
pattern Frac a <- (getFrac -> Just a)

getFrac :: ANY -> Maybe ANY
getFrac a@(RAT _) = Just a
getFrac a@(NUM _) = Just a
getFrac _ = Nothing

pattern BigN :: ANY -> ANY
pattern BigN a <- (getFrac -> Just a)

getBigN :: ANY -> Maybe ANY
getBigN a@(INT _) = Just a
getBigN a@(RAT _) = Just a
getBigN _ = Nothing

pattern Itr :: ANY -> ANY
pattern Itr a <- (getItr -> Just a)

getItr :: ANY -> Maybe ANY
getItr a@(SEQ _) = Just a
getItr a@(ARR _) = Just a
getItr _ = Nothing