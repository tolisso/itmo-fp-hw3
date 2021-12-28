module HW3.Evaluator where

import Control.Applicative (liftA2)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Text as T
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
  check (Prelude.length args == numArgs f) HiErrorArityMismatch
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
  HiValueBool <$> equals a b
apply (HiValueFunction HiFunNotEquals) [a, b] =
  HiValueBool . not <$> (equals a b)
apply (HiValueFunction HiFunLessThan) [a, b] =
  HiValueBool <$> lz a b
apply (HiValueFunction HiFunNotLessThan) [a, b] =
  HiValueBool . not <$> lz a b
apply (HiValueFunction HiFunNotGreaterThan) [a, b] =
  HiValueBool <$> ngz a b
apply (HiValueFunction HiFunGreaterThan) [a, b] =
  HiValueBool . not <$> ngz a b
-- string
apply (HiValueFunction HiFunLength) [(HiValueString s)] =
  return . HiValueNumber . toRational $ T.length s
apply (HiValueFunction HiFunToUpper) [(HiValueString s)] =
  return . HiValueString $ T.toUpper s
apply (HiValueFunction HiFunToLower) [(HiValueString s)] =
  return . HiValueString $ T.toLower s
apply (HiValueFunction HiFunReverse) [(HiValueString s)] =
  return . HiValueString $ T.reverse s
apply (HiValueFunction HiFunTrim) [(HiValueString s)] =
  return . HiValueString $ T.strip s
apply (HiValueFunction HiFunToUpper) [(HiValueString s)] =
  return . HiValueString $ T.toUpper s
-- other
apply (HiValueFunction f) args = do
  check (Prelude.length args == numArgs f) HiErrorArityMismatch
  throwError HiErrorInvalidArgument
apply _ _ = throwError HiErrorInvalidFunction

-- assertion
check :: Bool -> HiError -> Status ()
check cond err =
  unless cond $ throwError err

-- lower-then
lz :: HiValue -> HiValue -> Status Bool
lz (HiValueNumber x) (HiValueNumber y) = return (x < y)
lz (HiValueBool x) (HiValueBool y) = return (x < y)
lz (HiValueFunction x) (HiValueFunction y) = return (x < y)
lz (HiValueFunction _) _ = return True
lz (HiValueBool _) (HiValueNumber _) = return True
lz (HiValueString x) (HiValueString y) = return $ x < y
lz (HiValueString _) _ = throwError HiErrorInvalidArgument
lz _ (HiValueString _) = throwError HiErrorInvalidArgument
lz _ HiValueNull = throwError HiErrorInvalidArgument
lz HiValueNull _ = throwError HiErrorInvalidArgument
lz _ _ = return False

equals :: HiValue -> HiValue -> Status Bool
equals (HiValueNumber x) (HiValueNumber y) = return (x == y)
equals (HiValueBool x) (HiValueBool y) = return (x == y)
equals (HiValueFunction x) (HiValueFunction y) = return (x == y)
equals (HiValueString x) (HiValueString y) = return (x == y)
equals (HiValueNull) (HiValueNull) = return True
equals _ _ = return False

-- not-greater-then
ngz :: HiValue -> HiValue -> Status Bool
ngz a b = liftA2 (||) (lz a b) (equals a b)