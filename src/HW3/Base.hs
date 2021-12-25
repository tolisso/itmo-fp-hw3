module HW3.Base where
import GHC.Natural

data HiFun
  = HiFunAdd
  | HiFunSub
  | HiFunMul
  | HiFunDiv
  deriving (Show)

data HiValue
  = HiValueNumber Rational
  | HiValueFunction HiFun
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