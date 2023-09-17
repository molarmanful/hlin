module Main (main) where

import ANY
import ENV
import Parser (parse)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs
    >>= run . head
    >>= print . reverse . stack