module Main where

import Control.Monad
import Data.Text
import Data.Void
import GHC.IO.Handle
import GHC.IO.Handle.FD
import HW3.Base (HiExpr (HiExprValue), HiValue (HiValueFunction), HiFun (HiFunDiv))
import HW3.Parser
import Lib
import Text.Megaparsec
import HW3.Evaluator (evalM)

parser :: Parser HiExpr
parser = expr

main :: IO ()
main = do
  input <-
    putStr "REPL> "
      >> hFlush stdout
      >> getLine

  unless (input == ":quit") $
    eval (parse parser "unused" (pack input)) >> main
    where
      eval (Left s) = print s
      eval (Right v) = print (evalM v)
