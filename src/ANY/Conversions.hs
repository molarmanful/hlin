module ANY.Conversions where

import Data.Number.CReal
import qualified Data.Text as T
import qualified Data.Vector as V
import Parser (parse)
import Types

toTF :: ANY -> ANY
toTF = TF . toBool

toSTR :: ANY -> ANY
toSTR = STR . toText

toNUM :: ANY -> ANY
toNUM = NUM . toCReal

toFN :: ANY -> PATH -> ANY
toFN = flip FN . toParsed

toSEQ :: ANY -> ANY
toSEQ = SEQ . toList

toForm :: ANY -> String
toForm UN = "UN"
toForm (TF False) = "$F"
toForm (TF True) = "$T"
toForm (STR a) = show a
toForm (CMD a) = a
toForm (NUM a) = if a < 0 then show (abs a) ++ "_" else show a
toForm (FN _ a) = "( " ++ unwords (map toForm a) ++ " )"
toForm (SEQ a) = "[ " ++ unwords (map toForm a) ++ " ]"
toForm a = toForm $ toSEQ a

toBool :: ANY -> Bool
toBool UN = False
toBool (TF a) = a
toBool (NUM a) = a == 0
toBool (STR a) = not $ T.null a
toBool (CMD a) = not $ null a
toBool (FN _ a) = not $ null a
toBool (SEQ a) = not $ null a
toBool (ARR a) = not $ V.null a

toText :: ANY -> T.Text
toText UN = T.empty
toText (TF a) = T.pack $ show $ fromEnum a
toText (NUM a) = T.pack $ show a
toText (STR a) = a
toText (CMD a) = T.pack a
toText (FN _ a) = T.intercalate (T.singleton ' ') $ map toText a
toText (SEQ a) = mconcat $ map toText a
toText (ARR a) = toText $ SEQ $ V.toList a

toCReal :: ANY -> CReal
toCReal UN = 0
toCReal (TF a) = toEnum $ fromEnum a
toCReal (NUM a) = a
toCReal (STR a) = read $ T.unpack a
toCReal (FN _ a) = toCReal $ SEQ a
toCReal a = toCReal $ toSTR a

toParsed :: ANY -> [ANY]
toParsed (STR a) = parse $ T.unpack a
toParsed (FN _ a) = a
toParsed (SEQ a) = a
toParsed a = [a]

toList :: ANY -> [ANY]
toList UN = []
toList (STR a) = map (STR . T.singleton) $ T.unpack a
toList (SEQ a) = a
toList a = [a]