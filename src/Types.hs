{-# LANGUAGE GADTs #-}

module Types where

import qualified Data.Map as M
import Data.Number.CReal
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V
import qualified StmContainers.Map as C

data ENV = ENV
  { lines :: C.Map PATH ANY,
    code :: [ANY],
    path :: PATH,
    stack :: V.Vector ANY,
    scope :: M.Map String ANY,
    gscope :: C.Map String ANY,
    ids :: M.Map String PATH,
    gids :: C.Map String PATH,
    arr :: [V.Vector ANY]
  }

data ANY where
  UN :: ANY
  TF :: Bool -> ANY
  NUM :: CReal -> ANY
  STR :: T.Text -> ANY
  CMD :: String -> ANY
  FN :: PATH -> [ANY] -> ANY
  SEQ :: [ANY] -> ANY

instance Show ANY where
  show = toForm

toForm :: ANY -> String
toForm UN = "UN"
toForm (TF False) = "$F"
toForm (TF True) = "$T"
toForm (STR a) = show a
toForm (CMD a) = a
toForm (NUM a) = if a < 0 then show (abs a) ++ "_" else show a
toForm (FN _ a) = "( " ++ unwords (map toForm a) ++ " )"
toForm (SEQ a) = "[ " ++ unwords (map toForm a) ++ " ]"

newtype PATH = PATH (FilePath, Int) deriving (Show)