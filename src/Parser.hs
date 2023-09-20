module Parser (parse) where

import Control.Monad.State
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Ratio ((%))
import qualified Data.Text as T
import Types (ANY (..))

type Parser a = State ParserS a

data ParserS = ParserS {xs :: [ANY], x :: String, t :: PFlag}

data PFlag = T_UN | T_NUM | T_STR | T_CMD | T_ESC | T_DEC deriving (Eq)

dParser :: ParserS
dParser = ParserS {xs = [], x = "", t = T_UN}

parse :: [String] -> [ANY]
parse ls = case ls of
  l : _ -> pline l
  _ -> []

pline :: String -> [ANY]
pline l = xs $ execState (mapM_ choice l >> clean) dParser

choice :: Char -> Parser ()
choice c = do
  p@ParserS {t} <- get
  case t of
    T_ESC -> pesc c
    T_STR -> pstr c
    _ -> case c of
      '"' -> clean >> modify \q -> q {t = T_STR}
      '.' -> pdec
      c'
        | isDigit c' -> pnum c
        | c' `elem` (" \t\r\n" :: String) -> clean
      _ -> pcmd c

pesc :: Char -> Parser ()
pesc c = modify \p -> p {x = x p ++ cs, t = T_STR}
  where
    cs = case c of
      '"' -> [c]
      _ -> ['\\', c]

pstr :: Char -> Parser ()
pstr c = do
  p <- get
  case c of
    '\\' -> put p {t = T_ESC}
    '"' -> clean
    _ -> put p {x = x p ++ [c], t = T_STR}

pdec :: Parser ()
pdec = pf (== T_NUM) T_DEC '.'

pnum :: Char -> Parser ()
pnum = pf (`elem` [T_NUM, T_DEC]) T_NUM

pcmd :: Char -> Parser ()
pcmd = pf (== T_CMD) T_CMD

pf :: (PFlag -> Bool) -> PFlag -> Char -> Parser ()
pf b t' c = do
  ParserS {t} <- get
  unless (b t) clean
  modify \p -> p {x = x p ++ [c], t = t'}

clean :: Parser ()
clean = modify \ParserS {xs, x, t} -> dParser {xs = xs ++ f x t}
  where
    f x = \case
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
        | '.' `elem` x ->
            let s = splitOn "." x
                n :: Integer = read $ concat s
                d = toInteger $ length $ last s
             in [RAT $ n % (10 ^ d)]
        | otherwise -> [INT $ read x]
      _ -> []