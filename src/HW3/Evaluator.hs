module HW3.Evaluator where
import HW3.Base
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Except

type Status = ExceptT HiError Identity

evalM :: HiExpr -> Status HiValue 
evalM (HiExprValue v) = return v
evalM (HiExprApply f args) = do
    fr <- evalM f
    values <- traverse (\v -> evalM v) args
    apply fr values

apply :: HiValue -> [HiValue] -> Status HiValue
apply (HiValueFunction HiFunAdd) [(HiValueNumber a), (HiValueNumber b)] = do
    return (HiValueNumber (a + b))
apply (HiValueFunction HiFunSub) [(HiValueNumber a), (HiValueNumber b)] = do
    return (HiValueNumber (a - b))
apply (HiValueFunction HiFunMul) [(HiValueNumber a), (HiValueNumber b)] = do
    return (HiValueNumber (a * b))
apply (HiValueFunction HiFunDiv) [(HiValueNumber a), (HiValueNumber b)] = do
    return (HiValueNumber (a / b))
apply (HiValueFunction f) args = do
    check (length args == numArgs f) HiErrorArityMismatch
    throwError HiErrorInvalidArgument
apply _ _ = throwError HiErrorInvalidFunction


check :: Bool -> HiError -> Status ()
check cond err = do
    unless cond $ throwError err