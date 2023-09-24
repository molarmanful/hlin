module Main (main) where

import Data.Foldable (toList)
import ENV (run)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs
    >>= readFile . head
    >>= run
    >>= putStr . show