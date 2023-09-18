module Main (main) where

import ENV
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs
    >>= readFile . head
    >>= run
    >>= putStr . unlines . map show . reverse . stack