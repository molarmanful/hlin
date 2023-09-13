module ANY where

import Data.Number.CReal

data ANY a
  = UN
  | TF Bool
  | NUM CReal
  | STR String
  | CMD String
  | FN ((String, Int), [ANY a])
  | SEQ [ANY a]
  deriving (Show)

toForm :: ANY a -> String
toForm UN = "UN"
toForm (TF False) = "$F"
toForm (TF True) = "$T"
toForm (STR a) = show a
toForm (CMD a) = a
toForm (NUM a) = if a < 0 then show (abs a) ++ "_" else show a
toForm (FN (_, a)) = "( " ++ unwords (map toForm a) ++ " )"
toForm (SEQ a) = "[ " ++ unwords (map toForm a) ++ " ]"

toSTR :: ANY a1 -> ANY a2
toSTR = STR . toString

toNUM :: ANY a1 -> ANY a2
toNUM = NUM . toCReal

toTF :: ANY a1 -> ANY a2
toTF = TF . toBool

toSEQ :: ANY a -> ANY a
toSEQ = SEQ . toList

toBool :: ANY a -> Bool
toBool UN = False
toBool (TF a) = a
toBool (NUM 0) = False
toBool (NUM _) = True
toBool (STR "") = False
toBool (STR _) = True
toBool (CMD "") = False
toBool (CMD _) = True
toBool (FN (_, [])) = False
toBool (FN _) = True
toBool (SEQ []) = False
toBool (SEQ _) = True

toString :: ANY a -> [Char]
toString UN = ""
toString (TF a) = show $ fromEnum a
toString (NUM a) = show a
toString (STR a) = a
toString (CMD a) = a
toString (FN (_, a)) = toString =<< a
toString (SEQ a) = toString =<< a

toCReal :: ANY a -> CReal
toCReal UN = 0
toCReal (TF a) = toEnum $ fromEnum a
toCReal (NUM a) = a
toCReal (STR a) = read a
toCReal a = read $ show a

toList :: ANY a -> [ANY a]
toList UN = []
toList (STR a) = map (STR . show) a
toList (SEQ a) = a
toList a = [a]