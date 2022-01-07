{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser where

import Control.Monad (join)
import Control.Monad.Combinators.Expr
import qualified Data.ByteString as B
import Data.Char (digitToInt, isAlpha, isAlphaNum)
import Data.Foldable (Foldable (fold))
import qualified Data.List as List
import Data.Scientific (Scientific, toRealFloat)
import Data.String
import Data.Text hiding (foldr, map)
import Data.Void
import qualified Data.Word as W
import HW3.Base
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

spaced :: String -> Parser ()
spaced s = do
  string s
  C.space
  return ()

spacedStr :: String -> Parser String
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

pList :: Parser HiExpr
pList = do
  x <- between (spaced "[") (spaced "]") (sepBy oprExpr (spaced ","))
  return $ HiExprApply (HiExprValue . HiValueFunction $ HiFunList) x

pCwd :: Parser HiExpr
pCwd = do
  spaced "cwd"
  return . HiExprValue . HiValueAction $ HiActionCwd

pNow :: Parser HiExpr
pNow = do
  spaced "now"
  return . HiExprValue . HiValueAction $ HiActionNow

hexnumber :: Parser W.Word8
hexnumber = do
  x <- hexDigitChar
  y <- hexDigitChar
  spaced ""
  return . fromIntegral $ (digitToInt x) * 16 + (digitToInt y)

pBytes :: Parser HiExpr
pBytes = do
  x <- between (spaced "[#") (spaced "#]") (many hexnumber)
  return . HiExprValue . HiValueBytes . B.pack $ x

valFunc :: String -> HiFun -> Parser HiExpr
valFunc s n = do
  spaced s
  return $ HiExprValue (HiValueFunction n)

funcName :: Parser HiExpr
funcName =
  choice $
    map
      (\v -> valFunc (funcStr v) v)
      funcs

func :: HiExpr -> Parser HiExpr
func head = do
  body <- args
  return $ HiExprApply head body

args :: Parser [HiExpr]
args =
  between (spaced "(") (spaced ")") (sepBy oprExpr (spaced ","))
    <|> parseDotKey

pMapTerm :: Parser (HiExpr, HiExpr)
pMapTerm = do
  x <- oprExpr
  spaced ":"
  y <- oprExpr
  return (x, y)

pMap :: Parser HiExpr
pMap = do
  vals <- between (spaced "{") (spaced "}") (sepBy pMapTerm (spaced ","))
  return . HiExprDict $ vals

parseDotKey = do
  spaced "."
  x <- ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy` char '-'
  return [HiExprValue . HiValueString . pack . fold . List.intersperse ['-'] $ x]

simpleExpr :: Parser HiExpr
simpleExpr =
  number
    <|> funcName
    <|> bool
    <|> pString
    <|> pNull
    <|> pBytes
    <|> pList
    <|> pCwd
    <|> pNow
    <|> pMap

expr :: Parser HiExpr
expr = do
  x <- simpleExpr <|> bracketOprExpr
  expr' x
  where
    expr' head = do { y <- func head; expr' y } <|> return head

mkBin :: HiFun -> HiExpr -> HiExpr -> HiExpr
mkBin f = \x y -> HiExprApply (HiExprValue . HiValueFunction $ f) [x, y]

mkBinaryNotEnding inf name f ch = inf (mkBin f <$ try (spaced name <* notFollowedBy ch))

mkBinary inf name f = inf (mkBin f <$ spaced name)

action = Postfix ((\s -> HiExprRun s) <$ spaced "!")

table :: [[Operator Parser HiExpr]]
table =
  [ [action],
    [ mkBinary InfixL "*" HiFunMul,
      mkBinaryNotEnding InfixL "/" HiFunDiv "="
    ],
    [ mkBinary InfixL "+" HiFunAdd,
      mkBinary InfixL "-" HiFunSub
    ],
    [ mkBinary InfixN "==" HiFunEquals,
      mkBinary InfixN ">=" HiFunNotLessThan,
      mkBinary InfixN "<=" HiFunNotGreaterThan,
      mkBinary InfixN "/=" HiFunNotEquals,
      mkBinary InfixN ">" HiFunGreaterThan,
      mkBinary InfixN "<" HiFunLessThan
    ],
    [ mkBinary InfixR "&&" HiFunAnd
    ],
    [ mkBinary InfixR "||" HiFunOr
    ]
  ]

oprExpr :: Parser HiExpr
oprExpr = makeExprParser expr table

bracketOprExpr :: Parser HiExpr
bracketOprExpr = between (spaced "(") (spaced ")") oprExpr

parse' :: Parser HiExpr
parse' = do
  spaced ""
  e <- oprExpr
  eof
  return e

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse input = Text.Megaparsec.parse parse' "" input