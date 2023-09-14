module Main (main) where

import Parser (parse)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= print . parse . head