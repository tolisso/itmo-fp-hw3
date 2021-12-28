{-# LANGUAGE OverloadedStrings #-}

module HW3.Evaluator where

import Control.Applicative (liftA2)
import qualified Control.Monad.Cont as Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Foldable (traverse_)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (Semigroup (stimes))
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
  if b == 0
    then throwError HiErrorDivideByZero
    else return (HiValueNumber (a / b))
-- bool
apply (HiValueFunction HiFunNot) [(HiValueBool a)] =
  return (HiValueBool (not a))
-- compare
apply (HiValueFunction HiFunEquals) [a, b] =
  return . HiValueBool $ equals a b
apply (HiValueFunction HiFunNotEquals) [a, b] =
  return . HiValueBool . not $ (equals a b)
apply (HiValueFunction HiFunLessThan) [a, b] =
  return . HiValueBool $ lz a b
apply (HiValueFunction HiFunNotLessThan) [a, b] =
  return . HiValueBool . not $ lz a b
apply (HiValueFunction HiFunNotGreaterThan) [a, b] =
  return . HiValueBool $ ngz a b
apply (HiValueFunction HiFunGreaterThan) [a, b] =
  return . HiValueBool . not $ ngz a b
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
apply (HiValueFunction HiFunMul) [(HiValueString s), (HiValueNumber n)] =
  nTimes n s
apply (HiValueFunction HiFunMul) [(HiValueNumber n), (HiValueString s)] =
  nTimes n s
apply (HiValueFunction HiFunAdd) [(HiValueString x), (HiValueString y)] =
  return . HiValueString $ x <> y
apply (HiValueFunction HiFunDiv) [(HiValueString x), (HiValueString y)] =
  return . HiValueString $ x <> "/" <> y
-- string slices
apply (HiValueString s) [(HiValueNumber n)] = do
  x <- getInt n
  return $
    if checkBounds' s x
      then HiValueString . pack $ [T.index s x]
      else HiValueNull
apply (HiValueString s) [(HiValueNumber a), (HiValueNumber b)] = do
  x <- getInt a
  y <- getInt b
  let start = if x >= 0 then x else x + T.length s
  let end = if y >= 0 then y else y + T.length s
  let r = range (start, end)
  -- when x == y: r is empty, but we need to check
  Control.Monad.when (x == y) $ checkBounds s x
  -- check if all indeces are in range
  if Prelude.all (checkBounds' s) r
    then return . HiValueString . pack $ Prelude.map (index s) r
    else throwError HiErrorInvalidArgument
apply (HiValueString s) [x, (HiValueNull)] =
  apply (HiValueString s) [x, (HiValueNumber . toRational $ T.length s)]
apply (HiValueString s) [(HiValueNull), y] =
  apply (HiValueString s) [(HiValueNumber 0), y]
apply (HiValueString _) args =
  let sz = Prelude.length args
   in if sz == 1 || sz == 2
        then throwError HiErrorInvalidArgument
        else throwError HiErrorArityMismatch
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
lz :: HiValue -> HiValue -> Bool
lz (HiValueNumber x) (HiValueNumber y) = (x < y)
lz (HiValueBool x) (HiValueBool y) = (x < y)
lz (HiValueFunction x) (HiValueFunction y) = (x < y)
lz (HiValueString x) (HiValueString y) = x < y
lz (HiValueNull) (HiValueNull) = False
lz x y | isDifferentValues x y = (valPriority x) < (valPriority y)
lz x y =
  error $
    "lz didn't initialized for \"" ++ show x
      ++ "\", \""
      ++ show y
      ++ "\""

equals :: HiValue -> HiValue -> Bool
equals (HiValueNumber x) (HiValueNumber y) = (x == y)
equals (HiValueBool x) (HiValueBool y) = (x == y)
equals (HiValueFunction x) (HiValueFunction y) = (x == y)
equals (HiValueString x) (HiValueString y) = (x == y)
equals (HiValueNull) (HiValueNull) = True
equals _ _ = False

-- not-greater-then
ngz :: HiValue -> HiValue -> Bool
ngz a b = (lz a b) || (equals a b)

getInt :: Rational -> Status Int
getInt n | denominator n /= 1 = throwError HiErrorInvalidArgument
getInt n = return . fromIntegral . numerator $ n

nTimes :: Rational -> Text -> Status HiValue
nTimes n s = do
  x <- getInt n
  return
    . HiValueString
    . stimes x
    $ s

checkBounds' :: Text -> Int -> Bool
checkBounds' _ n | n < 0 = False
checkBounds' s n | T.length s <= n = False
checkBounds' _ _ = True

checkBounds :: Text -> Int -> Status ()
checkBounds n s =
  if checkBounds' n s
    then return ()
    else throwError HiErrorInvalidArgument

range :: (Int, Int) -> [Int]
range (x, y) | x < y = [x .. y - 1]
range (x, y) | y < x = Prelude.reverse [y .. x - 1]
range _ = []