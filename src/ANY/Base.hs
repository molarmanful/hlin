{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ANY.Base where

import Data.Foldable (Foldable (toList))
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Real (Ratio ((:%)))
import Parser (parse)
import Types
import Util

instance Show ANY where
  show UN = "UN"
  show (TF a)
    | a = "$T"
    | otherwise = "$F"
  show (STR a) = show a
  show (CMD a) = a
  show (NUM a) = show a
  show (RAT (n :% d)) = show n ++ "%" ++ show d
  show (INT a) = show a
  show (FN _ a) = case a of
    [] -> "()"
    _ -> "( " ++ unwords (show <$> a) ++ " )"
  show (SEQ a) = case a of
    [] -> "[]`"
    _ -> "[ " ++ unwords (show <$> a) ++ " ]`"
  show (ARR a) =
    if null a
      then "[]"
      else "[ " ++ unwords (toList $ show <$> a) ++ " ]"
  show (MAP a) =
    "{ "
      ++ unwords
        ( L.intercalate " => " . map show . pair
            <$> M.toList a
        )
      ++ " }"

instance Eq ANY where
  UN == UN = True
  TF a == TF b = a == b
  NUM a == Num b = a == toNUMW b
  Num a == NUM b = toNUMW a == b
  RAT a == Num b = a == toRATW b
  Num a == RAT b = toRATW a == b
  INT a == Num b = a == toINTW b
  Num a == INT b = toINTW a == b
  STR a == STR b = a == b
  Str a == Str b = toSTRW a == toSTRW b
  FN _ a == FN _ b = a == b
  SEQ a == SEQ b = a == b
  ARR a == ARR b = a == b
  MAP a == MAP b = a == b
  _ == _ = False

-- what the fuck
instance Ord ANY where
  compare a b | a == b = EQ
  compare UN _ = LT
  compare _ UN = GT
  compare (TF False) _ = LT
  compare _ (TF False) = GT
  compare (TF True) _ = GT
  compare _ (TF True) = LT
  compare (NUM a) (Num b) = compare a $ toNUMW b
  compare (Num a) (NUM b) = compare (toNUMW a) b
  compare (INT a) (Num b) = compare a $ toINTW b
  compare (Num a) (INT b) = compare (toINTW a) b
  compare (RAT a) (Num b) = compare a $ toRATW b
  compare (Num a) (RAT b) = compare (toRATW a) b
  compare (NUM a) (Listy b) = cmpNL a b
  compare (Listy a) (NUM b) = cmpLN a b
  compare (INT a) (Listy b) = cmpNL a b
  compare (Listy a) (INT b) = cmpLN a b
  compare (RAT a) (Listy b) = cmpNL a b
  compare (Listy a) (RAT b) = cmpLN a b
  compare (NUM a) (Itr b) = compare a $ fromIntegral $ alength b
  compare (Itr a) (NUM b) = compare (fromIntegral $ alength a) b
  compare (INT a) (Itr b) = compare a $ fromIntegral $ alength b
  compare (Itr a) (INT b) = compare (fromIntegral $ alength a) b
  compare (RAT a) (Itr b) = compare a $ fromIntegral $ alength b
  compare (Itr a) (RAT b) = compare (fromIntegral $ alength a) b
  compare (NUM a) (Str b) = cmpNS a $ toStr b
  compare (Str a) (NUM b) = cmpSN (toStr a) b
  compare (INT a) (Str b) = cmpNS a $ toStr b
  compare (Str a) (INT b) = cmpSN (toStr a) b
  compare (RAT a) (Str b) = cmpNS a $ toStr b
  compare (Str a) (RAT b) = cmpSN (toStr a) b
  compare (Str a) (Str b) = compare (toSTRW a) $ toSTRW b
  compare (MAP a) (MAP b) = compare a b
  compare (Listy a) (Itr b) = compare a $ toSEQW b
  compare (Itr a) (Listy b) = compare (toSEQW a) b
  compare a b = compare (toARRW a) $ toARRW b

-- conversions

matchT :: ANY -> ANY -> ANY
matchT UN = const UN
matchT (TF _) = toTF
matchT (RAT _) = toRAT
matchT (INT _) = toINT
matchT (NUM _) = toNUM
matchT (STR _) = toSTR
matchT (CMD _) = toCMD
matchT (FN p _) = toFN p
matchT (SEQ _) = toSEQ
matchT (ARR _) = toARR
matchT (MAP _) = toMAP

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
  RAT n -> toBigN n
  _ -> RAT $ toRATW a

toINT :: ANY -> ANY
toINT a = case a of
  INT _ -> a
  _ -> INT $ toINTW a

toNUM :: ANY -> ANY
toNUM a = case a of
  NUM _ -> a
  _ -> NUM $ toNUMW a

toCMD :: ANY -> ANY
toCMD a = case a of
  CMD _ -> a
  _ -> CMD $ toStr a

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

toBigN :: Rational -> ANY
toBigN (a :% 1) = INT a
toBigN a = RAT a

toTFW :: ANY -> Bool
toTFW UN = False
toTFW (TF a) = a
toTFW (RAT a) = a /= 0
toTFW (INT a) = a /= 0
toTFW (NUM a) = a /= 0
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
toFNW UN = []
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
txtRat a = case T.signed T.rational a of
  Left _ -> error "bad num"
  Right (x, _) -> x

-- convenience
alength :: ANY -> Int
alength (STR a) = T.length a
alength (CMD a) = length a
alength (Listy a) = length a
alength (ARR a) = length a
alength (MAP a) = length a
alength _ = -1

-- patterns

pattern Num :: ANY -> ANY
pattern Num a <- (getNum -> Just a)

pattern Frac :: ANY -> ANY
pattern Frac a <- (getFrac -> Just a)

pattern BigN :: ANY -> ANY
pattern BigN a <- (getFrac -> Just a)

pattern Str :: ANY -> ANY
pattern Str a <- (getStr -> Just a)

pattern Itr :: ANY -> ANY
pattern Itr a <- (getItr -> Just a)

pattern Listy :: [ANY] -> ANY
pattern Listy a <- (getListy -> Just a)

getNum :: ANY -> Maybe ANY
getNum a@(Frac _) = Just a
getNum a@(INT _) = Just a
getNum _ = Nothing

getFrac :: ANY -> Maybe ANY
getFrac a@(RAT _) = Just a
getFrac a@(NUM _) = Just a
getFrac _ = Nothing

getBigN :: ANY -> Maybe ANY
getBigN a@(INT _) = Just a
getBigN a@(RAT _) = Just a
getBigN _ = Nothing

getStr :: ANY -> Maybe ANY
getStr a@(STR _) = Just a
getStr a@(CMD _) = Just a
getStr _ = Nothing

getItr :: ANY -> Maybe ANY
getItr a@(SEQ _) = Just a
getItr a@(ARR _) = Just a
getItr _ = Nothing

getListy :: ANY -> Maybe [ANY]
getListy (FN _ a) = Just a
getListy (SEQ a) = Just a
getListy _ = Nothing