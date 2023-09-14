{-# LANGUAGE GADTs #-}

module Types where

import qualified Data.Map as M
import Data.Number.CReal
import qualified Data.Text as T
import qualified Data.Vector as V
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
  ARR :: V.Vector ANY -> ANY

newtype PATH = PATH (FilePath, Int) deriving (Show)