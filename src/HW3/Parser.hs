{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser where

import Data.Scientific (toRealFloat)
import Data.String
import Data.Text hiding (foldr, map)
import Data.Void
import HW3.Base
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

spaced :: Text -> Parser ()
spaced s = do
  string s
  C.space
  return ()

spacedStr :: Text -> Parser Text
spacedStr s = do
  x <- string s
  C.space
  return x

negScientific = do
  spaced "-"
  n <- scientific
  return (- n)

pEmpty :: Parser a
pEmpty = do
  fail "empty parser"

number :: Parser HiExpr
number = do
  n <- (negScientific <|> scientific)
  C.space
  return $ HiExprValue $ HiValueNumber (toRational n)

bool :: Parser HiExpr
bool = do
  b <- bool'
  return $ HiExprValue (HiValueBool b)

bool' :: Parser Bool
bool' =
  do string "true"; return True
    <|> do string "false"; return False

valFunc :: Text -> HiFun -> Parser HiExpr
valFunc s n = do
  spaced s
  return $ HiExprValue (HiValueFunction n)

funcName :: Parser HiExpr
funcName =
  foldr
    (\v acc -> valFunc (funcStr v) v <|> acc)
    pEmpty
    funcs

func :: HiExpr -> Parser HiExpr
func head = do
  body <- args
  return $ HiExprApply head body

args :: Parser [HiExpr]
args = between (spaced "(") (spaced ")") (sepBy expr (spaced ","))

simpleExpr :: Parser HiExpr
simpleExpr = number <|> funcName <|> bool

expr :: Parser HiExpr
expr =
  do
    x <- simpleExpr
    expr' x
  where
    expr' head = do { y <- func head; expr' y } <|> return head

oprExpr :: Int -> Parser HiExpr
oprExpr 10 = expr
oprExpr y = do
  x <- oprExpr (y + 1)
  lamb <- oprExpr'
  return $ lamb x
  where
    ops = getOperators y
    parseOps :: Parser Text
    parseOps = foldr (\x acc -> spacedStr x <|> acc) pEmpty ops
    oprExpr' :: Parser (HiExpr -> HiExpr)
    oprExprNext =
      do
        f <- parseOps
        ex <- oprExpr (y + 1)
        lamb <- oprExpr'
        return $ \outer ->
          lamb $
            HiExprApply
              (HiExprValue . HiValueFunction . opToFun $ f)
              [outer, ex]
    oprExpr' = try oprExprNext <|> return id
