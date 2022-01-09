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

-- parse string followed by any number of whitespaces
spaced :: String -> Parser ()
spaced s = do
  string s
  C.space
  return ()

-- spaced, but returning parsed string
-- without whithout following whitespaces
spacedStr :: String -> Parser String
spacedStr s = do
  x <- string s
  C.space
  return x

-- parse negative scientific
negScientific :: Parser Scientific
negScientific = do
  spaced "-"
  n <- scientific
  return (- n)

-- number parser
number :: Parser HiExpr
number = do
  n <- (negScientific <|> scientific)
  C.space
  return $ HiExprValue $ HiValueNumber (toRational n)

-- bool parser
bool :: Parser HiExpr
bool = do
  b <- bool'
  return $ HiExprValue (HiValueBool b)

-- bool parser without HiExpr wrapper
bool' :: Parser Bool
bool' =
  do spaced "true"; return True
    <|> do spaced "false"; return False

-- bull parser
pNull :: Parser HiExpr
pNull = do
  spaced "null"
  return . HiExprValue $ HiValueNull

-- string literal parser
pString :: Parser HiExpr
pString = do
  string "\""
  x <- manyTill charLiteral (try $ spaced "\"")
  return . HiExprValue . HiValueString . pack $ x

pList :: Parser HiExpr
pList = do
  x <- between (spaced "[") (spaced "]") (sepBy oprExpr (spaced ","))
  return $ HiExprApply (HiExprValue . HiValueFunction $ HiFunList) x

-- // the next two parsers can be moved to abstraction
-- // but unnecessary optimizing is the root of evil

-- `cwd` parser
pCwd :: Parser HiExpr
pCwd = do
  spaced "cwd"
  return . HiExprValue . HiValueAction $ HiActionCwd

-- `now` parser
pNow :: Parser HiExpr
pNow = do
  spaced "now"
  return . HiExprValue . HiValueAction $ HiActionNow

-- parse bytestring byte
hexnumber :: Parser W.Word8
hexnumber = do
  x <- hexDigitChar
  y <- hexDigitChar
  return . fromIntegral $ (digitToInt x) * 16 + (digitToInt y)

-- parse ByteString
pBytes :: Parser HiExpr
pBytes = do
  x <-
    between
      (spaced "[#")
      (C.space *> spaced "#]")
      (sepBy hexnumber (try $ spaced " " <* notFollowedBy (string "#]")))
  return . HiExprValue . HiValueBytes . B.pack $ x

-- the function literal parser
valFunc :: String -> HiFun -> Parser HiExpr
valFunc s n = do
  spaced s
  return $ HiExprValue (HiValueFunction n)

-- any function literal parser
funcName :: Parser HiExpr
funcName =
  choice $
    map
      (\v -> valFunc (funcStr v) v)
      funcs

-- get function to apply (head) and return parser of single function apply
-- aka parser of `head(...)`
func :: HiExpr -> Parser HiExpr
func head = do
  body <- args
  return $ HiExprApply head body

-- arguments in brackets or dot literal argument parser
args :: Parser [HiExpr]
args =
  between (spaced "(") (spaced ")") (sepBy (oprExpr <?> "argument") (spaced ","))
    <|> parseDotKey

-- map key-value pair parser
pMapTerm :: Parser (HiExpr, HiExpr)
pMapTerm = do
  x <- oprExpr <?> "map key"
  spaced ":"
  y <- oprExpr <?> "map value"
  return (x, y)

-- map parser
pMap :: Parser HiExpr
pMap = do
  vals <- between (spaced "{") (spaced "}") (sepBy pMapTerm (spaced ","))
  return . HiExprDict $ vals

-- `!` parser
pDoAction :: HiExpr -> Parser HiExpr
pDoAction head = do
  spaced "!"
  return . HiExprRun $ head

-- dot literal argument parser
parseDotKey = do
  spaced "." <?> "dot literal argument"
  x <- ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-' <?> "after dot literal"
  return [HiExprValue . HiValueString . pack . fold . List.intersperse ['-'] $ x]

-- primitives parser
simpleExpr :: Parser HiExpr
simpleExpr =
  choice
    [ number,
      funcName,
      bool,
      pString,
      pNull,
      pBytes,
      pList,
      pCwd,
      pNow,
      pMap
    ]
    <?> "primitive"

-- no operators expression parser
expr :: Parser HiExpr
expr =
  ( do
      x <- simpleExpr <|> bracketOprExpr
      expr' x
  )
    <?> "no operators expression"
  where
    expr' head =
      do { y <- func head <|> pDoAction head; expr' y } <|> return head

-- lambda for HiExprApply creation
mkBinApply :: HiFun -> HiExpr -> HiExpr -> HiExpr
mkBinApply f = \x y -> HiExprApply (HiExprValue . HiValueFunction $ f) [x, y]

-- `opParser` - operator parser
-- `inf` - infix type (associativity)
-- f - function (`HiFun`)
mkBinary' opParser inf f = inf (mkBinApply f <$ (opParser <?> "binary operator"))

-- operator not followed on `ch`
mkBinaryNotFollowedBy ch name = mkBinary' (try (spaced name <* notFollowedBy ch))

mkBinary name = mkBinary' (spaced name)

table :: [[Operator Parser HiExpr]]
table =
  [ [ mkBinary "*" InfixL HiFunMul,
      (mkBinaryNotFollowedBy "=") "/" InfixL HiFunDiv
    ],
    [ mkBinary "+" InfixL HiFunAdd,
      mkBinary "-" InfixL HiFunSub
    ],
    [ mkBinary "==" InfixN HiFunEquals,
      mkBinary ">=" InfixN HiFunNotLessThan,
      mkBinary "<=" InfixN HiFunNotGreaterThan,
      mkBinary "/=" InfixN HiFunNotEquals,
      mkBinary ">" InfixN HiFunGreaterThan,
      mkBinary "<" InfixN HiFunLessThan
    ],
    [ mkBinary "&&" InfixR HiFunAnd
    ],
    [ mkBinary "||" InfixR HiFunOr
    ]
  ]

-- expression with operators parser
oprExpr :: Parser HiExpr
oprExpr = makeExprParser expr table <?> "expression"

-- literally `oprExpr` in brackets parser
bracketOprExpr :: Parser HiExpr
bracketOprExpr = between (spaced "(") (spaced ")") oprExpr

-- internal parse function
parse' :: Parser HiExpr
parse' = do
  spaced ""
  e <- oprExpr
  eof
  return e

-- external parse function
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse input = Text.Megaparsec.parse parse' "" input