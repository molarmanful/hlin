{-# LANGUAGE GADTs #-}

module Types where

import qualified Data.Map as M
import Data.Number.CReal (CReal)
import qualified Data.Text as T
import Data.Vector (Vector)

data ANY
  = UN
  | TF Bool
  | NUM CReal
  | STR T.Text
  | CMD String
  | FN PATH [ANY]
  | SEQ [ANY]
  | ARR (Vector ANY)
  deriving (Eq)

newtype PATH = PATH (FilePath, Int) deriving (Show, Eq)