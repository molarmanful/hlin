module Main (main) where

import ANY
import Parser (parse)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= print . parse . head