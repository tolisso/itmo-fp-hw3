module HW3.Evaluator where
import HW3.Base
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Except
import Control.Applicative (liftA2)

type Status = ExceptT HiError Identity

eval :: HiExpr -> Either HiError HiValue 
eval ex = convertM $ evalM ex

convertM :: Status HiValue -> Either HiError HiValue
convertM (ExceptT (Identity v)) = v

evalM :: HiExpr -> Status HiValue 
evalM (HiExprValue v) = return v
evalM (HiExprApply f args) = do
    fr <- evalM f
    if reduced fr then do
        values <- traverse (\v -> evalM v) args
        apply fr values
    else
        applyFull fr args

applyFull :: HiValue -> [HiExpr] -> Status HiValue
applyFull (HiValueFunction HiFunIf) [condExpr, a, b] = do
    cond <- evalM condExpr
    applyIf cond where
        applyIf (HiValueBool True) = evalM a
        applyIf (HiValueBool False) = evalM b
        applyIf _ = throwError HiErrorInvalidArgument

applyFull (HiValueFunction f) args = do
    check (length args == numArgs f) HiErrorArityMismatch
    throwError HiErrorInvalidArgument
applyFull _ _ = throwError HiErrorInvalidFunction


reduced :: HiValue -> Bool
reduced (HiValueFunction HiFunIf) = False
reduced _ = True

apply :: HiValue -> [HiValue] -> Status HiValue
apply (HiValueFunction HiFunAdd) [(HiValueNumber a), (HiValueNumber b)] = do
    return (HiValueNumber (a + b))
apply (HiValueFunction HiFunSub) [(HiValueNumber a), (HiValueNumber b)] = do
    return (HiValueNumber (a - b))
apply (HiValueFunction HiFunMul) [(HiValueNumber a), (HiValueNumber b)] = do
    return (HiValueNumber (a * b))
apply (HiValueFunction HiFunDiv) [(HiValueNumber a), (HiValueNumber b)] = do
    return (HiValueNumber (a / b))

apply (HiValueFunction HiFunNot) [(HiValueBool a)] = do
    return (HiValueBool (not a))
apply (HiValueFunction HiFunAnd) [(HiValueBool a), (HiValueBool b)] = do
    return (HiValueBool (a && b))
apply (HiValueFunction HiFunOr) [(HiValueBool a), (HiValueBool b)] = do
    return (HiValueBool (a || b))

apply (HiValueFunction HiFunEquals) [a, b] = do
    return . HiValueBool $ equals a b
        
apply (HiValueFunction HiFunLessThan) [a, b] =
    return . HiValueBool $ lz a b
        
apply (HiValueFunction HiFunNotLessThan) [a, b] = 
    return . HiValueBool . not $ lz a b

apply (HiValueFunction HiFunNotGreaterThan) [a, b] = 
    return . HiValueBool $ ngz a b

apply (HiValueFunction HiFunGreaterThan) [a, b] = 
    return . HiValueBool . not $ ngz a b

apply (HiValueFunction f) args = do
    check (length args == numArgs f) HiErrorArityMismatch
    throwError HiErrorInvalidArgument

apply _ _ = throwError HiErrorInvalidFunction


check :: Bool -> HiError -> Status ()
check cond err = do
    unless cond $ throwError err

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

ngz :: HiValue -> HiValue -> Bool
ngz a b = (lz a b) || (equals a b)