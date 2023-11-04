module Types where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (State, StateT)
import Data.HashMap.Lazy (HashMap)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.MonoTraversable (Element)
import Data.Sequence (Seq)
import Data.Text (Text)
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

data ANY
  = UN
  | TF Bool
  | RAT Rational
  | INT Integer
  | NUM Double
  | STR Text
  | CMD String
  | FN PATH [ANY]
  | SEQ [ANY]
  | ARR (Vector ANY)
  | MAP (Map ANY ANY)

type instance Element ANY = ANY

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

type LoopFN a = State LoopFNS a

data LoopFNS = LoopFN
  { xs :: [ANY],
    ys :: [ANY],
    n :: Int
  }
  deriving (Generic)

dLoopFN :: LoopFNS
dLoopFN = LoopFN {xs = [], ys = [], n = 1}

newtype PATH = PATH (FilePath, Int) deriving (Show, Eq, Ord, Hashable)

newtype LINE = LINE (String, Maybe ANY)
