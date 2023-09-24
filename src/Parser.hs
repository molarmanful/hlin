module Parser (parse) where

import Control.Monad.State (execState, put, unless)
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import qualified Data.Ratio as R
import qualified Data.Text as T
import GHC.Real (Ratio ((:%)))
import Optics
import Optics.State.Operators ((%=), (.=))
import Types

parse :: [String] -> [ANY]
parse ls = case ls of
  l : _ -> pline l
  _ -> []

pline :: String -> [ANY]
pline l = execState (mapM_ choice l >> clean) dParser ^. #xs

choice :: Char -> Parser ()
choice c =
  use #t >>= \case
    T_ESC -> pesc c
    T_STR -> pstr c
    _ -> case c of
      '"' -> clean >> #t .= T_STR
      '.' -> pdec
      c'
        | isDigit c' -> pnum c
        | c' `elem` (" \t\r\n" :: String) -> clean
      _ -> pcmd c

pesc :: Char -> Parser ()
pesc c = do
  let cs = case c of
        '"' -> [c]
        _ -> ['\\', c]
  #x %= (++ cs)
  #t .= T_STR

pstr :: Char -> Parser ()
pstr c = case c of
  '\\' -> #t .= T_ESC
  '"' -> clean
  _ -> do
    #x %= (++ [c])
    #t .= T_STR

pdec :: Parser ()
pdec = pf (== T_NUM) T_DEC '.'

pnum :: Char -> Parser ()
pnum = pf (`elem` [T_NUM, T_DEC]) T_NUM

pcmd :: Char -> Parser ()
pcmd = pf (== T_CMD) T_CMD

pf :: (PFlag -> Bool) -> PFlag -> Char -> Parser ()
pf b t' c = do
  t <- use #t
  unless (b t) clean
  #x %= (++ [c])
  #t .= t'

clean :: Parser ()
clean = do
  t <- use #t
  x <- use #x
  let x' = case t of
        T_STR -> [STR $ T.pack x]
        T_ESC -> [STR $ T.pack $ x ++ "\\"]
        T_CMD
          | all (`elem` ("()[]{}" :: String)) x -> [CMD [a] | a <- x]
          | otherwise -> [CMD x]
        T_DEC
          | x == "." -> [CMD x]
          | last x == '.' -> [INT $ read $ init x, CMD "."]
          | otherwise -> [NUM $ read $ '0' : x]
        T_NUM
          | '.' `elem` x -> [mkRAT x]
          | otherwise -> [INT $ read x]
        _ -> []
  #xs %= (++ x')
  #x .= ""
  #t .= T_UN

mkRAT :: String -> ANY
mkRAT r = f $ n R.% (10 ^ d)
  where
    s = splitOn "." r
    n :: Integer = read $ concat s
    d = toInteger $ length $ last s
    f (a :% 1) = INT a
    f a = RAT a