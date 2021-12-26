{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser where

import Data.Scientific (toRealFloat)
import Data.String
import Data.Void
import HW3.Base
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as L
import Data.Text

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

number :: Parser HiExpr
number = do
  n <- (negScientific <|> scientific)
  C.space
  return $ HiExprValue $ HiValueNumber (toRational n)

valFunc :: Text -> HiFun -> Parser HiExpr
valFunc s n = do
  spaced s
  return $ HiExprValue (HiValueFunction n)

funcName :: Parser HiExpr
funcName =
  valFunc "add" HiFunAdd
    <|> valFunc "sub" HiFunSub
    <|> valFunc "mul" HiFunMul
    <|> valFunc "div" HiFunDiv

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
simpleExpr = number <|> funcName

expr :: Parser HiExpr
expr =
  do
    x <- simpleExpr <|> expr
    func x <|> return x