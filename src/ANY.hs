module ANY where

import Data.Number.CReal
import Data.Vector.Unboxed

-- data ANY a where
--   TF :: Bool -> ANY Bool
--   NUM :: CReal -> ANY CReal
--   STR :: String -> ANY String
--   SEQ :: [ANY a] -> ANY [a]
--   VEC :: Vector (ANY a) -> ANY (Vector a)

data ANY a
  = UN
  | TF Bool
  | NUM CReal
  | STR String
  -- | SEQ [ANY a]
  -- | VEC [Vector (ANY a)]

instance Show a => Show (ANY a) where
  show :: ANY a -> String
  show UN = ""
  show a@(TF _) = show $ toCReal a
  show (NUM a) = show a
  show (STR a) = a

toCReal :: ANY a -> CReal
toCReal (NUM a) = a
toCReal UN = 0
toCReal (TF a) = toEnum $ fromEnum a
toCReal (STR a) = read a
