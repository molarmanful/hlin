module Main (main) where

import Flow
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= head .> putStrLn
