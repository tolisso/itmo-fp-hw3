{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser where

import Control.Monad (join)
import Data.Scientific (Scientific, toRealFloat)
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

negScientific :: Parser Scientific
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
  do spaced "true"; return True
    <|> do spaced "false"; return False

pNull :: Parser HiExpr
pNull = do
  spaced "null"
  return . HiExprValue $ HiValueNull

pString :: Parser HiExpr
pString = do
  string "\""
  x <- manyTill charLiteral (spaced "\"")
  return . HiExprValue . HiValueString . pack $ x

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
args = between (spaced "(") (spaced ")") (sepBy mainExpr (spaced ","))

simpleExpr :: Parser HiExpr
simpleExpr = number <|> funcName <|> bool <|> pString <|> pNull

expr :: Parser HiExpr
expr =
  bracketMainExpr
    <|> do
      x <- simpleExpr
      expr' x
  where
    expr' head = do { y <- func head; expr' y } <|> return head

oprExpr :: Int -> Parser HiExpr
oprExpr 10 = expr
oprExpr y = do
  x <- termParser
  lamb <- oprExpr'
  return $ lamb x
  where
    termParser = oprExpr (y + 1)
    -- operators on current level
    ops = getOperators y
    -- helper function to convert `Text` to function
    textToFun f = (HiExprValue . HiValueFunction . opToFun $ f)
    -- recursive right side parser
    oprExprNext :: Parser (HiExpr -> HiExpr)
    oprExprNext =
      do
        f <- parseOps y
        ex <- termParser
        lamb <- oprExpr'
        if (isLeftAssoc y)
          then return $ \outer ->
            lamb $
              HiExprApply
                (textToFun f)
                [outer, ex]
          else return $ \outer ->
            HiExprApply
              (textToFun f)
              ([outer, lamb ex])
    -- parser's main body
    oprExpr' :: Parser (HiExpr -> HiExpr)
    oprExpr' = try oprExprNext <|> return id

parseOps :: Int -> Parser Text
parseOps y = foldr (\x acc -> spacedStr x <|> acc) pEmpty (getOperators y)

mainExpr :: Parser HiExpr
mainExpr = oprExpr 0

bracketMainExpr :: Parser HiExpr
bracketMainExpr = between (spaced "(") (spaced ")") mainExpr