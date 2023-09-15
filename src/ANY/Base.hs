{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ANY.Base where

-- import Data.Align

import Data.Foldable (Foldable (toList))
import qualified Data.Map as M
import Data.Number.CReal
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Parser (parse)
import Types

instance Show ANY where
  show UN = "UN"
  show (TF a)
    | a = "$T"
    | otherwise = "$F"
  show (STR a) = show a
  show (CMD a) = a
  show (NUM a)
    | a < 0 = show (abs a) ++ "_"
    | otherwise = show a
  show (FN _ a) = "( " ++ unwords (show <$> a) ++ " )"
  show (SEQ a) = "[ " ++ unwords (show <$> a) ++ " ]"
  show a = show $ toSEQ a

-- conversions

toTF :: ANY -> ANY
toTF a = case a of
  TF _ -> a
  _ -> TF $ toTFW a

toSTR :: ANY -> ANY
toSTR a = case a of
  STR _ -> a
  _ -> STR $ toSTRW a

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

toTFW :: ANY -> Bool
toTFW UN = False
toTFW (TF a) = a
toTFW (NUM a) = a == 0
toTFW (STR a) = not $ T.null a
toTFW (CMD a) = not $ null a
toTFW (FN _ a) = not $ null a
toTFW (SEQ a) = not $ null a
toTFW (ARR a) = not $ null a

toSTRW :: ANY -> T.Text
toSTRW UN = T.empty
toSTRW (TF a) = T.pack $ show $ fromEnum a
toSTRW (NUM a) = T.pack $ show a
toSTRW (STR a) = a
toSTRW (CMD a) = T.pack a
toSTRW (FN _ a) = T.intercalate (T.singleton ' ') $ toSTRW <$> a
toSTRW (SEQ a) = mconcat $ toSTRW <$> a
toSTRW a@(ARR _) = toSTRW $ toSEQ a

toNUMW :: ANY -> CReal
toNUMW UN = 0
toNUMW (TF a) = toEnum $ fromEnum a
toNUMW (NUM a) = a
toNUMW (STR a) = read $ T.unpack a
toNUMW (FN _ a) = toNUMW $ SEQ a
toNUMW a = toNUMW $ toSTR a

toFNW :: ANY -> [ANY]
toFNW (STR a) = parse $ T.unpack a
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

-- patterns

pattern Itr :: ANY -> ANY
pattern Itr a <- (getItr -> Just a)

getItr :: ANY -> Maybe ANY
getItr a@(SEQ _) = Just a
getItr a@(ARR _) = Just a
getItr _ = Nothing