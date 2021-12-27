module HW3.Evaluator where

import Control.Applicative (liftA2)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans
import HW3.Base

type Status = ExceptT HiError Identity

eval :: HiExpr -> Either HiError HiValue
eval ex = convertM $ evalM ex

convertM :: Status HiValue -> Either HiError HiValue
convertM (ExceptT (Identity v)) = v

-- `eval`'s in-depth realization with
evalM :: HiExpr -> Status HiValue
evalM (HiExprValue v) = return v
evalM (HiExprApply f args) = do
  fr <- evalM f
  if reduced fr
    then do
      values <- traverse (\v -> evalM v) args
      apply fr values
    else applyFull fr args

-- determine in `evalM` what we need to use, `apply` or `applyFull`
reduced :: HiValue -> Bool
reduced (HiValueFunction HiFunIf) = False
reduced (HiValueFunction HiFunAnd) = False
reduced (HiValueFunction HiFunOr) = False
reduced _ = True

isTrue :: HiValue -> Bool
isTrue (HiValueBool True) = True
isTrue (HiValueBool False) = False
isTrue _ = undefined

applyFull :: HiValue -> [HiExpr] -> Status HiValue
-- if
applyFull (HiValueFunction HiFunIf) [condExpr, a, b] = do
  cond <- evalM condExpr
  applyIf cond
  where
    applyIf (HiValueBool True) = evalM a
    applyIf (HiValueBool False) = evalM b
    applyIf _ = throwError HiErrorInvalidArgument
-- bool
applyFull (HiValueFunction HiFunAnd) [a, b] = do
  ra <- evalM a
  if (not . isTrue $ ra)
    then return . HiValueBool $ False
    else do
      rb <- evalM b
      return . HiValueBool . isTrue $ rb
applyFull (HiValueFunction HiFunOr) [a, b] = do
  ra <- evalM a
  if (isTrue $ ra)
    then return . HiValueBool $ True
    else do
      rb <- evalM b
      return . HiValueBool . isTrue $ rb
-- other
applyFull (HiValueFunction f) args = do
  check (length args == numArgs f) HiErrorArityMismatch
  throwError HiErrorInvalidArgument
applyFull _ _ = throwError HiErrorInvalidFunction

apply :: HiValue -> [HiValue] -> Status HiValue
-- number
apply (HiValueFunction HiFunAdd) [(HiValueNumber a), (HiValueNumber b)] =
  return (HiValueNumber (a + b))
apply (HiValueFunction HiFunSub) [(HiValueNumber a), (HiValueNumber b)] =
  return (HiValueNumber (a - b))
apply (HiValueFunction HiFunMul) [(HiValueNumber a), (HiValueNumber b)] =
  return (HiValueNumber (a * b))
apply (HiValueFunction HiFunDiv) [(HiValueNumber a), (HiValueNumber b)] =
  return (HiValueNumber (a / b))
-- bool
apply (HiValueFunction HiFunNot) [(HiValueBool a)] =
  return (HiValueBool (not a))
-- compare
apply (HiValueFunction HiFunEquals) [a, b] =
  return . HiValueBool $ equals a b
apply (HiValueFunction HiFunNotEquals) [a, b] =
  return . HiValueBool $ not (equals a b)
apply (HiValueFunction HiFunLessThan) [a, b] =
  return . HiValueBool $ lz a b
apply (HiValueFunction HiFunNotLessThan) [a, b] =
  return . HiValueBool . not $ lz a b
apply (HiValueFunction HiFunNotGreaterThan) [a, b] =
  return . HiValueBool $ ngz a b
apply (HiValueFunction HiFunGreaterThan) [a, b] =
  return . HiValueBool . not $ ngz a b
-- other
apply (HiValueFunction f) args = do
  check (length args == numArgs f) HiErrorArityMismatch
  throwError HiErrorInvalidArgument
apply _ _ = throwError HiErrorInvalidFunction

-- assertion
check :: Bool -> HiError -> Status ()
check cond err =
  unless cond $ throwError err

-- lower-then
lz :: HiValue -> HiValue -> Bool
lz (HiValueNumber x) (HiValueNumber y) = (x < y)
lz (HiValueBool x) (HiValueBool y) = (x < y)
lz (HiValueFunction x) (HiValueFunction y) = (x < y)
lz (HiValueFunction _) _ = True
lz (HiValueBool _) (HiValueNumber _) = True
lz _ _ = False

equals :: HiValue -> HiValue -> Bool
equals (HiValueNumber x) (HiValueNumber y) = (x == y)
equals (HiValueBool x) (HiValueBool y) = (x == y)
equals (HiValueFunction x) (HiValueFunction y) = (x == y)
equals _ _ = False

-- not-greater-then
ngz :: HiValue -> HiValue -> Bool
ngz a b = (lz a b) || (equals a b)