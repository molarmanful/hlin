{-# LANGUAGE NamedFieldPuns #-}

module Parser where

import ANY

data Parser a = Parser {xs :: [ANY a], x :: String, t :: Types}

data Types = T_UN | T_NUM | T_STR | T_CMD | T_ESC | T_DEC deriving (Eq)

parse :: String -> Parser a
parse = pline . head . lines

pline :: String -> Parser a
pline = foldl choice Parser {xs = [], x = "", t = T_UN}

choice :: Parser a -> Char -> Parser a
choice p@(Parser {x, t = T_ESC}) '"' = p {x = x ++ "\"", t = T_STR}
choice p@(Parser {x, t = T_ESC}) c = p {x = x ++ ['\\', c], t = T_STR}
choice p@(Parser {t = T_STR}) '\\' = p {t = T_ESC}
choice p@(Parser {t = T_STR}) '"' = p {t = T_UN} -- TODO: add clean fn
choice p@(Parser {x, t = T_STR}) c = p {x = x ++ [c], t = T_STR}