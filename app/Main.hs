{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (catch)
import Control.Monad
import Control.Monad.Except (ExceptT)
import Control.Monad.RWS.Lazy (MonadIO (liftIO))
import qualified Data.Set as S
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
import qualified Text.Megaparsec as TM

-- feel free to change permissions below
permissions :: S.Set HiPermission
permissions = S.fromList [AllowRead, AllowWrite, AllowTime]

getResult :: String -> InputT IO ()
getResult input = eval' (parse input)
  where
    eval' :: Either (TM.ParseErrorBundle String Void) HiExpr -> InputT IO ()
    eval' (Left s) = outputStrLn . TM.errorBundlePretty $ s
    eval' (Right v) =
      liftIO $
        catch
          body
          ((\e -> putStrLn $ "IOException: " ++ show e) :: PermissionException -> IO ())
      where
        printRes v = show $ v
        body = do
          putStrLn ("tree: " ++ show v)
          x <-
            runHIO
              (eval v :: HIO (Either HiError HiValue))
              permissions
          putStrLn $
            either
              (("error: " ++) . printRes)
              (("value: " ++) . printRes)
              (prettyValue <$> x)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just "q" -> return ()
        Just input -> do
          getResult input
          loop
