module Types where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Data.HashMap.Lazy (HashMap)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Sequence (Seq)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified StmContainers.Map as CM

type ENVS a = ExceptT String (StateT ENV IO) a

data ENV = ENV
  { lns :: CM.Map PATH LINE,
    code :: [ANY],
    path :: PATH,
    stack :: Seq ANY,
    scope :: HashMap String ANY,
    gscope :: CM.Map String ANY,
    ids :: HashMap String PATH,
    gids :: CM.Map String PATH,
    arr :: [Vector ANY]
  }

-- TODO: add MAP
data ANY
  = UN
  | TF Bool
  | RAT Rational
  | INT Integer
  | NUM Double
  | STR T.Text
  | CMD String
  | FN PATH [ANY]
  | SEQ [ANY]
  | ARR (Vector ANY)
  | MAP (Map ANY ANY)

newtype PATH = PATH (FilePath, Int) deriving (Show, Eq, Ord, Hashable)

newtype LINE = LINE (String, Maybe ANY)