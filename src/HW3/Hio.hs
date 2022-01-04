{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module HW3.Hio where

import Control.Exception (throw)
import qualified Control.Monad.Cont as Control.Monad
import qualified Data.ByteString as B
import Data.Set
import HW3.Base
import System.Directory

newtype HIO a = HIO {runHIO :: Set HiPermission -> IO a}

instance Functor HIO where
  fmap g (HIO f) = HIO (\s -> g <$> f s)

instance Monad HIO where
  return val = HIO (\s -> return val)
  m >>= f =
    HIO
      ( \s -> do
          x <- runHIO m s
          runHIO (f x) s
      )

instance Applicative HIO where
  pure = return
  (<*>) = Control.Monad.ap

runAction' :: HiPermission -> IO HiValue -> HIO HiValue
runAction' perm io = HIO $ \s ->
  if perm `member` s
    then io
    else throw (PermissionRequired perm)

instance HiMonad HIO where
  runAction HiActionCwd = runAction' AllowRead $ do
    path <- getCurrentDirectory
    return . HiValueString . pack $ path
  runAction (HiActionRead file) = runAction' AllowRead $ do
    path <- B.readFile file
    return . HiValueString . pack $ path
  runAction (HiActionWrite file bs) = runAction' AllowWrite $ do
    path <- writeFile file (toUtf8 bs)
    return . HiValueString . pack $ path
  runAction _ = undefined