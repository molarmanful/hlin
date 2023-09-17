module Types where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Number.CReal (CReal)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified StmContainers.Map as CM

type ENVS a = ExceptT String (StateT ENV IO) a

data ENV = ENV
  { lns :: CM.Map PATH (String, ANY),
    code :: [ANY],
    path :: PATH,
    stack :: [ANY],
    scope :: Map String ANY,
    gscope :: CM.Map String ANY,
    ids :: Map String PATH,
    gids :: CM.Map String PATH,
    arr :: [Vector ANY]
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
  | MAP (Map ANY ANY)
  deriving (Eq, Ord)

newtype PATH = PATH (FilePath, Int) deriving (Show, Eq, Ord, Hashable)
