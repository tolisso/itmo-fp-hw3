{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module HW3.Base where

import Codec.Serialise (Serialise)
import Control.Exception (Exception)
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text
import qualified Data.Time as T
import qualified GHC.Base as Control.Monad
import GHC.Generics (Generic)
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
  | -- list
    HiFunList
  | HiFunRange
  | HiFunFold
  | -- bytes
    HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | -- actions
    HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | -- time
    HiFunParseTime
  | -- random
    HiFunRand
  | -- io
    HiFunEcho
  | -- dict
    HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

data HiValue
  = HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes B.ByteString
  | HiValueAction HiAction
  | HiValueTime T.UTCTime
  | HiValueDict (M.Map HiValue HiValue)
  deriving (Show, Ord, Eq)
  deriving stock (Generic)
  deriving anyclass (Serialise)

data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show, Eq)

data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq)

data HiAction
  = HiActionRead FilePath
  | HiActionWrite FilePath B.ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

funcInfo :: HiFun -> (Int, String)
funcInfo HiFunAdd = (2, "add")
funcInfo HiFunSub = (2, "sub")
funcInfo HiFunMul = (2, "mul")
funcInfo HiFunDiv = (2, "div")
funcInfo HiFunNot = (1, "not")
funcInfo HiFunAnd = (2, "and")
funcInfo HiFunOr = (2, "or")
funcInfo HiFunLessThan = (2, "less-than")
funcInfo HiFunGreaterThan = (2, "greater-than")
funcInfo HiFunEquals = (2, "equals")
funcInfo HiFunNotLessThan = (2, "not-less-than")
funcInfo HiFunNotGreaterThan = (2, "not-greater-than")
funcInfo HiFunNotEquals = (2, "not-equals")
funcInfo HiFunIf = (3, "if")
funcInfo HiFunLength = (1, "length")
funcInfo HiFunToUpper = (1, "to-upper")
funcInfo HiFunToLower = (1, "to-lower")
funcInfo HiFunReverse = (1, "reverse")
funcInfo HiFunTrim = (1, "trim")
funcInfo HiFunList = (error "list is varag", "list")
funcInfo HiFunRange = (2, "range")
funcInfo HiFunFold = (2, "fold")
funcInfo HiFunPackBytes = (1, "pack-bytes")
funcInfo HiFunUnpackBytes = (1, "unpack-bytes")
funcInfo HiFunEncodeUtf8 = (1, "encode-utf8")
funcInfo HiFunDecodeUtf8 = (1, "decode-utf8")
funcInfo HiFunZip = (1, "zip")
funcInfo HiFunUnzip = (1, "unzip")
funcInfo HiFunSerialise = (1, "serialise")
funcInfo HiFunDeserialise = (1, "deserialise")
funcInfo HiFunRead = (1, "read")
funcInfo HiFunWrite = (2, "write")
funcInfo HiFunChDir = (1, "cd")
funcInfo HiFunMkDir = (1, "mkdir")
funcInfo HiFunParseTime = (1, "parse-time")
funcInfo HiFunRand = (2, "rand")
funcInfo HiFunEcho = (1, "echo")
funcInfo HiFunCount = (1, "count")
funcInfo HiFunKeys = (1, "keys")
funcInfo HiFunValues = (1, "values")
funcInfo HiFunInvert = (1, "invert")

numArgs :: HiFun -> Int
numArgs = fst . funcInfo

funcStr :: HiFun -> String
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
    HiFunTrim,
    HiFunList,
    HiFunRange,
    HiFunFold,
    HiFunPackBytes,
    HiFunUnpackBytes,
    HiFunEncodeUtf8,
    HiFunDecodeUtf8,
    HiFunZip,
    HiFunUnzip,
    HiFunSerialise,
    HiFunDeserialise,
    HiFunRead,
    HiFunWrite,
    HiFunChDir,
    HiFunMkDir,
    HiFunParseTime,
    HiFunRand,
    HiFunEcho,
    HiFunCount,
    HiFunKeys,
    HiFunValues,
    HiFunInvert
  ]

isDifferentValues :: HiValue -> HiValue -> Bool
isDifferentValues (HiValueBool _) (HiValueBool _) = False
isDifferentValues (HiValueAction _) (HiValueAction _) = False
isDifferentValues (HiValueBytes _) (HiValueBytes _) = False
isDifferentValues (HiValueDict _) (HiValueDict _) = False
isDifferentValues (HiValueFunction _) (HiValueFunction _) = False
isDifferentValues (HiValueList _) (HiValueList _) = False
isDifferentValues (HiValueNull) (HiValueNull) = False
isDifferentValues (HiValueNumber _) (HiValueNumber _) = False
isDifferentValues (HiValueString _) (HiValueString _) = False
isDifferentValues (HiValueTime _) (HiValueTime _) = False
isDifferentValues _ _ = True

valPriority :: HiValue -> Int
valPriority (HiValueDict _) = 10
valPriority (HiValueTime _) = 9
valPriority (HiValueAction _) = 8
valPriority (HiValueBytes _) = 7
valPriority (HiValueList _) = 6
valPriority (HiValueString _) = 5
valPriority (HiValueNumber _) = 4
valPriority (HiValueBool _) = 3
valPriority (HiValueFunction _) = 2
valPriority (HiValueNull) = 1
