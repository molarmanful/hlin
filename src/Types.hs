module Types where

import qualified Data.Map as M
import Data.Number.CReal (CReal)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified StmContainers.Map as CM

data ENV = ENV
  { lns :: IO (CM.Map PATH (String, ANY)),
    code :: [ANY],
    path :: PATH,
    stack :: [ANY],
    scope :: M.Map String ANY,
    gscope :: IO (CM.Map String ANY),
    ids :: M.Map String PATH,
    gids :: IO (CM.Map String PATH),
    arr :: [Vector ANY]
  }

dENV :: ENV
dENV =
  ENV
    { lns = CM.newIO,
      code = [],
      path = PATH ("", 0),
      stack = [],
      scope = M.empty,
      gscope = CM.newIO,
      ids = M.empty,
      gids = CM.newIO,
      arr = []
    }

-- TODO: add MAP
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
