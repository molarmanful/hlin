{-# LANGUAGE NamedFieldPuns #-}

module Parser (parse) where

import Data.Char (isDigit)
import qualified Data.Text as T
import Types (ANY (..))

data Parser = Parser {xs :: [ANY], x :: String, t :: PFlag}

data PFlag = T_UN | T_NUM | T_STR | T_CMD | T_ESC | T_DEC deriving (Eq)

dParser :: Parser
dParser = Parser {xs = [], x = "", t = T_UN}

parse :: String -> [ANY]
parse ls = case lines ls of
  l : _ -> pline l
  _ -> []

pline :: String -> [ANY]
pline = xs . clean . foldl choice dParser

choice :: Parser -> Char -> Parser
choice p@(Parser {t}) c = case t of
  T_ESC -> pesc p c
  T_STR -> pstr p c
  _ -> case c of
    '"' -> p {t = T_STR}
    '.' -> pdec p
    c' | isDigit c' -> pnum p c
    c' | c' `elem` (" \t\r\n" :: String) -> clean p
    _ -> pcmd p c

pesc :: Parser -> Char -> Parser
pesc p@(Parser {x}) c = p {x = x ++ cs, t = T_STR}
  where
    cs = case c of
      '"' -> [c]
      _ -> ['\\', c]

pstr :: Parser -> Char -> Parser
pstr p@(Parser {x}) c = case c of
  '\\' -> p {t = T_ESC}
  '"' -> clean p
  _ -> p {x = x ++ [c], t = T_STR}

pdec :: Parser -> Parser
pdec = flip (pf (== T_NUM) T_DEC) '.'

pnum :: Parser -> Char -> Parser
pnum = pf (`elem` [T_NUM, T_DEC]) T_NUM

pcmd :: Parser -> Char -> Parser
pcmd = pf (== T_CMD) T_CMD

pf :: (PFlag -> Bool) -> PFlag -> Parser -> Char -> Parser
pf b t1 p@(Parser {t}) c = f $ if b t then p else clean p
  where
    f q@(Parser {x}) = q {x = x ++ [c], t = t1}

clean :: Parser -> Parser
clean (Parser {xs, x, t}) = dParser {xs = xs ++ x'}
  where
    x' = case t of
      T_STR -> [STR $ T.pack x]
      T_ESC -> [STR $ T.pack $ x ++ "\\"]
      T_CMD ->
        if all (`elem` ("()[]{}" :: String)) x
          then [CMD [a] | a <- x]
          else [CMD x]
      T_DEC ->
        if
            | x == "." -> [CMD x]
            | last x == '.' -> [NUM $ read $ init x, CMD "."]
            | otherwise -> [NUM $ read x]
      T_NUM -> [NUM $ read x]
      _ -> []