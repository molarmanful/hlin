module Types where

import qualified Data.Map as M
import Data.Number.CReal (CReal)
import qualified Data.Text as T
import Data.Vector (Vector)
import GHC.Conc (STM)
import qualified StmContainers.Map as CM

data ENV = ENV
  { lns :: STM (CM.Map PATH (String, ANY)),
    code :: [ANY],
    path :: PATH,
    stack :: [ANY],
    scope :: M.Map String ANY,
    gscope :: STM (CM.Map String ANY),
    ids :: M.Map String PATH,
    gids :: STM (CM.Map String PATH),
    arr :: [Vector ANY]
  }

dENV :: ENV
dENV =
  ENV
    { lns = CM.new,
      code = [],
      path = PATH ("", 0),
      stack = [],
      scope = M.empty,
      gscope = CM.new,
      ids = M.empty,
      gids = CM.new,
      arr = []
    }

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
