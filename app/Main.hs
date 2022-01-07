{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Except (ExceptT)
import Control.Monad.RWS.Lazy (MonadIO (liftIO))
import Data.Set (fromList)
import Data.Text as T
import Data.Void
import GHC.IO.Handle
import GHC.IO.Handle.FD
import HW3.Action
import HW3.Base
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty
import Lib
import System.Console.Haskeline
import Text.Megaparsec

parser :: Parser HiExpr
parser = do
  spaced ""
  e <- oprExpr
  eof
  return e

permissions = fromList [AllowRead, AllowWrite, AllowTime]

getResult :: String -> InputT IO ()
getResult input = eval' (parse parser "aba" (pack input))
  where
    eval' :: Either (ParseErrorBundle Text Void) HiExpr -> InputT IO ()
    eval' (Left s) = outputStrLn . show $ s
    eval' (Right v) = do
      x <-
        liftIO $
          runHIO
            (eval v :: HIO (Either HiError HiValue))
            permissions
      either printRes printRes (prettyValue <$> x)
      where
        printRes v = outputStrLn . show $ v

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
