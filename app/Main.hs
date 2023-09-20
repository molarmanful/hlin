module Main (main) where

import Data.Foldable (toList)
import ENV
import System.Environment (getArgs)
import Text.Read

main :: IO ()
main =
  getArgs
    >>= readFile . head
    >>= run
    >>= putStr . unlines . map show . toList . stack