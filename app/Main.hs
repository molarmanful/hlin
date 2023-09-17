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
    >>= print . reverse . stack