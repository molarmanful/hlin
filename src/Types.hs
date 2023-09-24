module Types where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (State, StateT)
import Data.HashMap.Lazy (HashMap)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Sequence (Seq)
import qualified Data.Text as T
import Data.Vector (Vector)
import GHC.Generics (Generic)
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
    arr :: [Seq ANY]
  }
  deriving (Generic)

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

type Parser a = State ParserS a

data ParserS = ParserS
  { xs :: [ANY],
    x :: String,
    t :: PFlag
  }
  deriving (Generic)

data PFlag = T_UN | T_NUM | T_STR | T_CMD | T_ESC | T_DEC deriving (Eq)

dParser :: ParserS
dParser = ParserS {xs = [], x = "", t = T_UN}

newtype PATH = PATH (FilePath, Int) deriving (Generic, Show, Eq, Ord, Hashable)

newtype LINE = LINE (String, Maybe ANY)