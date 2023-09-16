module Main (main) where

import ENV
import Parser (parse)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= print . reverse . stack . run . head