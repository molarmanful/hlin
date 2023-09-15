module ENV where

import ANY
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified StmContainers.Map as CM
import Types

data ENV = ENV
  { lines :: CM.Map PATH ANY,
    code :: [ANY],
    path :: PATH,
    stack :: [ANY],
    scope :: M.Map String ANY,
    gscope :: CM.Map String ANY,
    ids :: M.Map String PATH,
    gids :: CM.Map String PATH,
    arr :: [V.Vector ANY]
  }