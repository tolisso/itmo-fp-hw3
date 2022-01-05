{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module HW3.Base where

import Codec.Serialise (Serialise)
import Control.Exception (Exception)
import qualified Data.ByteString as B
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
  deriving (Show)
  deriving stock (Generic)
  deriving anyclass (Serialise)

data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  deriving (Show)

data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show)

data HiAction
  = HiActionRead FilePath
  | HiActionWrite FilePath B.ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  deriving (Show)
  deriving stock (Generic)
  deriving anyclass (Serialise)

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Ord, Eq)

data PermissionException
  = PermissionRequired HiPermission
  deriving (Show)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

instance Exception PermissionException

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
funcInfo HiFunList = (error "list is varag", "list")
funcInfo HiFunRange = (2, "range")
funcInfo HiFunFold = (2, "fold")
funcInfo HiFunPackBytes = (1, "pack-bytes")
funcInfo HiFunUnpackBytes = (1, "unpack-bytes")
funcInfo HiFunEncodeUtf8 = (1, "encode-utf8")
funcInfo HiFunDecodeUtf8 = (1, "decode-utf8")
funcInfo HiFunZip = (1, "zip")
funcInfo HiFunUnzip = (1, "unzip")
funcInfo HiFunSerialise = (1, "serialize")
funcInfo HiFunDeserialise = (1, "deserialize")
funcInfo HiFunRead = (1, "read")
funcInfo HiFunWrite = (2, "write")
funcInfo HiFunChDir = (1, "cd")
funcInfo HiFunMkDir = (1, "mkdir")
funcInfo HiFunParseTime = (1, "parse-time")
funcInfo HiFunRand = (2, "rand")

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
    HiFunRand
  ]

isDifferentValues :: HiValue -> HiValue -> Bool
isDifferentValues (HiValueNumber _) (HiValueNumber _) = False
isDifferentValues (HiValueFunction _) (HiValueFunction _) = False
isDifferentValues (HiValueString _) (HiValueString _) = False
isDifferentValues (HiValueBool _) (HiValueBool _) = False
isDifferentValues (HiValueNull) (HiValueNull) = False
isDifferentValues _ _ = True

valPriority :: HiValue -> Int
valPriority (HiValueTime _) = 9
valPriority (HiValueAction _) = 8
valPriority (HiValueBytes _) = 7
valPriority (HiValueList _) = 6
valPriority (HiValueString _) = 5
valPriority (HiValueNumber _) = 4
valPriority (HiValueBool _) = 3
valPriority (HiValueFunction _) = 2
valPriority (HiValueNull) = 1
