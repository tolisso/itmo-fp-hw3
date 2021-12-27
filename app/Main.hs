{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Text as T
import Data.Void
import GHC.IO.Handle
import GHC.IO.Handle.FD
import HW3.Base (HiExpr (HiExprValue), HiFun (HiFunDiv), HiValue (HiValueFunction))
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty
import Lib
import System.Console.Haskeline
import Text.Megaparsec

parser :: Parser HiExpr
parser = do
  e <- mainExpr
  eof
  return e

getResult :: String -> InputT IO ()
getResult input = eval' (parse parser "aba" (pack input))
  where
    eval' :: Either (ParseErrorBundle Text Void) HiExpr -> InputT IO ()
    eval' (Left s) = outputStrLn (show s)
    eval' (Right v) = either printRes printRes (prettyValue <$> eval v)
      where
        printRes v = outputStrLn $ show v

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          getResult input
          loop
