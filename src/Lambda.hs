module Lambda (loopFN) where

import Control.Monad.State (execState)
import Optics
import Optics.State.Operators ((%=), (.=))
import Types

loopFN :: [ANY] -> LoopFNS
loopFN code = flip execState dLoopFN do
  #xs .= code
  loop

loop :: LoopFN ()
loop = do
  xs <- use #xs
  n <- use #n
  if n <= 0 || null xs
    then return ()
    else do
      let (c : cs) = xs
      #n %= (+) case c of
        CMD x
          | '(' `elem` x -> 1
          | ')' `elem` x -> -1
        _ -> 0
      #xs .= cs
      #ys %= (|> c)
      loop