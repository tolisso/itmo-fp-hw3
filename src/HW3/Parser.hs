{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser where

import Data.Scientific (toRealFloat)
import Data.String
import Data.Void
import HW3.Base
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as L
import Data.Text hiding (foldr)

type Parser = Parsec Void Text

spaced :: Text -> Parser ()
spaced s = do
  string s
  C.space
  return ()
  
negScientific = do
  spaced "-"
  n <- scientific
  return (-n)

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
  do {string "true"; return True} 
  <|>
  do {string "false"; return False}

valFunc :: Text -> HiFun -> Parser HiExpr
valFunc s n = do
  spaced s
  return $ HiExprValue (HiValueFunction n)

funcName :: Parser HiExpr
funcName =
  foldr 
    (\v acc -> valFunc (snd v) (fst v) <|> acc) 
    pEmpty
    funcInfo 

func :: HiExpr -> Parser HiExpr
func head = do
  body <- args
  return $ HiExprApply head body

args :: Parser [HiExpr]
args = do
  spaced "("
  emptyP <|> nonemptyP
  
  where
    emptyP :: Parser [HiExpr]
    emptyP = do 
      spaced ")"
      return []
    nonemptyP :: Parser [HiExpr]
    nonemptyP = do
      v <- expr
      x <- many $ do
        spaced ","
        expr
      spaced ")"
      return $ [v] ++ x

simpleExpr :: Parser HiExpr
simpleExpr = number <|> funcName <|> bool

expr :: Parser HiExpr
expr =
  do
    x <- simpleExpr
    expr' x
  where
    expr' head = do {y <- func head; expr' y} <|> return head