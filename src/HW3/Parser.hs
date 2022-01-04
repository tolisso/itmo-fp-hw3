{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser where

import Control.Monad (join)
import Control.Monad.Combinators.Expr
import qualified Data.ByteString as B
import Data.Char (digitToInt)
import Data.Scientific (Scientific, toRealFloat)
import Data.String
import Data.Text hiding (foldr, map)
import Data.Void
import qualified Data.Word as W
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

pList :: Parser HiExpr
pList = do
  x <- between (spaced "[") (spaced "]") (sepBy oprExpr (spaced ","))
  return $ HiExprApply (HiExprValue . HiValueFunction $ HiFunList) x

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
args = between (spaced "(") (spaced ")") (sepBy oprExpr (spaced ","))

simpleExpr :: Parser HiExpr
simpleExpr =
  number
    <|> funcName
    <|> bool
    <|> pString
    <|> pNull
    <|> pBytes
    <|> pList

expr :: Parser HiExpr
expr = do
  x <- simpleExpr <|> bracketOprExpr
  expr' x
  where
    expr' head = do { y <- func head; expr' y } <|> return head

mkBin :: HiFun -> HiExpr -> HiExpr -> HiExpr
mkBin f = \x y -> HiExprApply (HiExprValue . HiValueFunction $ f) [x, y]

mkBinaryX inf name f = inf (mkBin f <$ spaced name)

binaryL = mkBinaryX InfixL

binaryR = mkBinaryX InfixR

action = Postfix ((\s -> HiExprRun s) <$ spaced "!")

table :: [[Operator Parser HiExpr]]
table =
  [ [action],
    [ binaryL "*" HiFunMul,
      binaryL "/" HiFunDiv
    ],
    [ binaryL "+" HiFunAdd,
      binaryL "-" HiFunSub
    ],
    [ binaryL "==" HiFunEquals,
      binaryL ">=" HiFunNotLessThan,
      binaryL "<=" HiFunGreaterThan,
      binaryL "/=" HiFunNotEquals,
      binaryL ">" HiFunGreaterThan,
      binaryL "<" HiFunLessThan
    ],
    [ binaryR "&&" HiFunAnd
    ],
    [ binaryR "||" HiFunOr
    ]
  ]

oprExpr :: Parser HiExpr
oprExpr = makeExprParser expr table

bracketOprExpr :: Parser HiExpr
bracketOprExpr = between (spaced "(") (spaced ")") oprExpr