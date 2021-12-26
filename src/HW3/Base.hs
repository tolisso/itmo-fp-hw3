{-# LANGUAGE OverloadedStrings #-}

module HW3.Base where
import GHC.Natural
import Data.Text

data HiFun
  -- number
  = HiFunAdd
  | HiFunSub
  | HiFunMul
  | HiFunDiv

  -- bool
  | HiFunNot
  | HiFunAnd
  | HiFunOr

  -- compare
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals

  -- conditional
  | HiFunIf
  deriving (Show)

data HiValue
  = HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueBool Bool
  deriving (Show)

data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  deriving (Show)

data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show)

numArgs :: HiFun -> Int
numArgs HiFunAdd = 2
numArgs HiFunSub = 2
numArgs HiFunMul = 2
numArgs HiFunDiv = 2
numArgs HiFunNot = 1
numArgs HiFunAnd = 2
numArgs HiFunOr = 2
numArgs HiFunLessThan = 2
numArgs HiFunGreaterThan = 2
numArgs HiFunEquals = 2
numArgs HiFunNotLessThan = 2
numArgs HiFunNotGreaterThan = 2
numArgs HiFunNotEquals = 2
numArgs HiFunIf = 3

-- constructor, str-to-parse
funcInfo :: [(HiFun, Text)]
funcInfo = [ 
  (HiFunAdd,            "add"), 
  (HiFunSub,            "sub"), 
  (HiFunMul,            "mul"), 
  (HiFunDiv,            "div"),

  (HiFunNot,            "not"),
  (HiFunAnd,            "and"),
  (HiFunOr,             "or"),

  (HiFunLessThan,       "less-than"),
  (HiFunGreaterThan,    "greater-then"),
  (HiFunEquals,         "equals"),
  (HiFunNotLessThan,    "not-less-then"),
  (HiFunNotGreaterThan, "not-greater-then"),
  (HiFunNotEquals,      "not-equals"),

  (HiFunIf,             "if")
  ]