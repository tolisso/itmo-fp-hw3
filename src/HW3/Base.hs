{-# LANGUAGE OverloadedStrings #-}

module HW3.Base where

import Data.Text
import GHC.Natural

data HiFun
  = -- number
    HiFunAdd
  | HiFunSub
  | HiFunMul
  | HiFunDiv
  | -- bool
    HiFunNot
  | HiFunAnd
  | HiFunOr
  | -- compare
    HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | -- conditional
    HiFunIf
  | -- string
    HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  deriving (Show, Eq, Ord)

data HiValue
  = HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNull
  | HiValueString Text
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
funcInfo HiFunLength = (1, "length")
funcInfo HiFunToUpper = (1, "to-upper")
funcInfo HiFunToLower = (1, "to-lower")
funcInfo HiFunReverse = (1, "reverse")
funcInfo HiFunTrim = (1, "trim")

numArgs :: HiFun -> Int
numArgs = fst . funcInfo

funcStr :: HiFun -> Text
funcStr = snd . funcInfo

funcs :: [HiFun]
funcs =
  [ HiFunAdd,
    HiFunSub,
    HiFunMul,
    HiFunDiv,
    HiFunNotLessThan,
    HiFunNotGreaterThan,
    HiFunNotEquals,
    HiFunNot,
    HiFunAnd,
    HiFunOr,
    HiFunLessThan,
    HiFunGreaterThan,
    HiFunEquals,
    HiFunIf,
    HiFunLength,
    HiFunToUpper,
    HiFunToLower,
    HiFunReverse,
    HiFunTrim
  ]

isDifferentValues :: HiValue -> HiValue -> Bool
isDifferentValues (HiValueNumber _) (HiValueNumber _) = False
isDifferentValues (HiValueFunction _) (HiValueFunction _) = False
isDifferentValues (HiValueString _) (HiValueString _) = False
isDifferentValues (HiValueBool _) (HiValueBool _) = False
isDifferentValues (HiValueNull) (HiValueNull) = False
isDifferentValues _ _ = True

valPriority :: HiValue -> Int
valPriority (HiValueString _) = 5
valPriority (HiValueNumber _) = 4
valPriority (HiValueBool _) = 3
valPriority (HiValueFunction _) = 2
valPriority (HiValueNull) = 1

isLeftAssoc :: Int -> Bool
isLeftAssoc 2 = False
isLeftAssoc 3 = False
isLeftAssoc _ = True

getOperators :: Int -> [Text]
getOperators 0 = []
getOperators 1 = []
getOperators 2 = ["||"]
getOperators 3 = ["&&"]
getOperators 4 = ["==", "/=", "<=", ">=", "<", ">"]
getOperators 5 = []
getOperators 6 = ["+", "-"]
getOperators 7 = ["/", "*"]
getOperators 8 = []
getOperators 9 = []
getOperators x = error ("No such operator priority: " ++ show x)

opToFun :: Text -> HiFun
opToFun "+" = HiFunAdd
opToFun "-" = HiFunSub
opToFun "/" = HiFunDiv
opToFun "*" = HiFunMul
opToFun "==" = HiFunEquals
opToFun "/=" = HiFunNotEquals
opToFun ">" = HiFunGreaterThan
opToFun ">=" = HiFunNotLessThan
opToFun "<=" = HiFunNotGreaterThan
opToFun "<" = HiFunLessThan
opToFun "&&" = HiFunAnd
opToFun "||" = HiFunOr
opToFun x = error ("No such operator: " ++ show x)
