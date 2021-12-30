{-# LANGUAGE OverloadedStrings #-}

module HW3.Evaluator where

import Control.Applicative (liftA2)
import qualified Control.Monad.Cont as Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans
import qualified Data.Foldable as F
import Data.Ratio (denominator, numerator)
import Data.Semigroup (Semigroup (stimes))
import qualified Data.Sequence as S
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

toBool' :: HiValue -> Status Bool
toBool' (HiValueBool True) = return True
toBool' (HiValueBool False) = return False
toBool' _ = throwError HiErrorInvalidArgument

toBool :: HiExpr -> Status Bool
toBool x = do
  xr <- evalM x
  toBool' $ xr

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
  ra <- toBool a
  if (not ra)
    then return . HiValueBool $ False
    else evalM b
applyFull (HiValueFunction HiFunOr) [a, b] = do
  ra <- toBool a
  if ra
    then return . HiValueBool $ True
    else evalM b
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
  nTimes n s HiValueString
apply (HiValueFunction HiFunMul) [(HiValueNumber n), (HiValueString s)] =
  nTimes n s HiValueString
apply (HiValueFunction HiFunAdd) [(HiValueString x), (HiValueString y)] =
  return . HiValueString $ x <> y
apply (HiValueFunction HiFunDiv) [(HiValueString x), (HiValueString y)] =
  return . HiValueString $ x <> "/" <> y
-- string slices
apply (HiValueString s) [(HiValueNumber n)] = do
  x <- getInt n
  return $
    if checkBoundsStr s x
      then HiValueString . pack $ [T.index s x]
      else HiValueNull
apply (HiValueString s) [(HiValueNumber a), (HiValueNumber b)] =
  slice s a b (T.length s) $ \str start end ->
    if start <= end
      then do
        checkBoundsStr_ str start
        checkBoundsStr_ str (end - 1)
        return . HiValueString $ substr str start end
      else do
        checkBoundsStr_ s (end + 1)
        checkBoundsStr_ s start
        return . HiValueString $ T.reverse (substr s (end + 1) (start + 1))
apply (HiValueString s) [x, (HiValueNull)] =
  apply (HiValueString s) [x, (HiValueNumber . toRational $ T.length s)]
apply (HiValueString s) [(HiValueNull), y] =
  apply (HiValueString s) [(HiValueNumber 0), y]
apply (HiValueString _) args =
  let sz = Prelude.length args
   in if sz == 1 || sz == 2
        then throwError HiErrorInvalidArgument
        else throwError HiErrorArityMismatch
-- list
apply (HiValueFunction HiFunList) arr =
  return . HiValueList . S.fromList $ arr
apply (HiValueFunction HiFunFold) [(HiValueFunction f), HiValueList arr] = do
  check (not . S.null $ arr) HiErrorInvalidArgument
  Prelude.foldl1
    ( \x y -> do
        xr <- x
        yr <- y
        apply (HiValueFunction f) [xr, yr]
    )
    (Prelude.map return (F.toList arr))
apply (HiValueList arr) [(HiValueNumber i)] = do
  x <- getInt i
  return $
    if checkBoundsSeq arr x
      then HiValueList . S.singleton $ S.index arr x
      else HiValueNull
apply (HiValueList arr) [(HiValueNumber a), (HiValueNumber b)] =
  slice arr a b (S.length arr) $ \s start end ->
    if start <= end
      then do
        checkBoundsSeq_ s start
        checkBoundsSeq_ s (end - 1)
        return . HiValueList $ subseq s start end
      else do
        checkBoundsSeq_ s (end + 1)
        checkBoundsSeq_ s start
        return . HiValueList $ S.reverse (subseq s (end + 1) (start + 1))
apply (HiValueList s) [x, (HiValueNull)] =
  apply (HiValueList s) [x, (HiValueNumber . toRational $ S.length s)]
apply (HiValueList s) [(HiValueNull), y] =
  apply (HiValueList s) [(HiValueNumber 0), y]
apply (HiValueFunction HiFunMul) [(HiValueList s), (HiValueNumber n)] =
  nTimes n s HiValueList
apply (HiValueFunction HiFunMul) [(HiValueNumber n), (HiValueList s)] =
  nTimes n s HiValueList
apply (HiValueFunction HiFunAdd) [(HiValueList x), (HiValueList y)] =
  return . HiValueList $ x S.>< y
apply (HiValueList _) args =
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

nTimes :: Semigroup a => Rational -> a -> (a -> HiValue) -> Status HiValue
nTimes n s val = do
  x <- getInt n
  return
    . val
    . stimes x
    $ s

checkBounds :: Int -> Int -> Bool
checkBounds _ n | n < 0 = False
checkBounds sz n | sz <= n = False
checkBounds _ _ = True

checkBoundsStr :: Text -> Int -> Bool
checkBoundsStr s = checkBounds (T.length s)

checkBoundsStr_ :: Text -> Int -> Status ()
checkBoundsStr_ s i = check (checkBoundsStr s i) HiErrorInvalidArgument

checkBoundsSeq :: S.Seq HiValue -> Int -> Bool
checkBoundsSeq arr = checkBounds $ S.length arr

checkBoundsSeq_ :: S.Seq HiValue -> Int -> Status ()
checkBoundsSeq_ s i = check (checkBoundsSeq s i) HiErrorInvalidArgument

-- abstraction converting `Rational` borders to `Int` and apply last argument function
slice ::
  t -> -- what to slice
  Rational -> -- l rational
  Rational -> -- r rational
  Int -> -- length
  (t -> Int -> Int -> Status HiValue) -> -- func to get slice with int borders
  Status HiValue
slice arr a b len getSlice = do
  x <- getInt a
  y <- getInt b
  let start = if x >= 0 then x else x + len
  let end = if y >= 0 then y else y + len
  getSlice arr start end

substr :: Text -> Int -> Int -> Text
substr t l r = T.drop l . T.take r $ t

subseq :: S.Seq HiValue -> Int -> Int -> S.Seq HiValue
subseq t l r = S.drop l . S.take r $ t