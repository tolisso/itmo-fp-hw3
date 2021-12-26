{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Text as T
import Data.Void
import GHC.IO.Handle
import GHC.IO.Handle.FD
import HW3.Base (HiExpr (HiExprValue), HiValue (HiValueFunction), HiFun (HiFunDiv))
import HW3.Parser
import Lib
import Text.Megaparsec
import HW3.Evaluator
import HW3.Pretty

parser :: Parser HiExpr
parser = do 
  e <- expr
  eof
  return e

main :: IO ()
main = do
  -- putStrLn "as"
  input <-
    putStr "REPL> "
      >> hFlush stdout
      >> getLine

  unless (input == ":quit") $
    eval' (parse parser "aba" (pack input)) >> main
    where
      eval' :: Either (ParseErrorBundle Text Void) HiExpr -> IO() 
      eval' (Left s) = print s
      eval' (Right v) = print (prettyValue <$> eval v)
