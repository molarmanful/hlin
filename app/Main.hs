module Main (main) where

import ANY
import ENV
import Parser (parse)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs
    >>= readFile . head
    >>= run
    >>= putStr . unlines . map show . reverse . stack