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
  deriving (Show, Eq, Ord)

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

funcInfo :: HiFun -> (Int, Text)
funcInfo HiFunAdd = (2, "add")
funcInfo HiFunSub = (2, "sub")
funcInfo HiFunMul = (2, "mul")
funcInfo HiFunDiv = (2, "div")
funcInfo HiFunNot = (1, "not")
funcInfo HiFunAnd = (2, "and")
funcInfo HiFunOr = (2, "or")
funcInfo HiFunLessThan = (2, "less-than")
funcInfo HiFunGreaterThan = (2, "greater-then")
funcInfo HiFunEquals = (2, "equals")
funcInfo HiFunNotLessThan = (2, "not-less-then")
funcInfo HiFunNotGreaterThan = (2, "not-greater-then")
funcInfo HiFunNotEquals = (2, "not-equals")
funcInfo HiFunIf = (3, "if")

numArgs :: HiFun -> Int
numArgs = fst . funcInfo

funcStr :: HiFun -> Text
funcStr = snd . funcInfo

funcs :: [HiFun]
funcs = [ 
  HiFunAdd,
  HiFunSub,
  HiFunMul,
  HiFunDiv,

  HiFunNot,
  HiFunAnd,
  HiFunOr,

  HiFunLessThan,
  HiFunGreaterThan,
  HiFunEquals,
  HiFunNotLessThan,
  HiFunNotGreaterThan,
  HiFunNotEquals,

  HiFunIf
  ]