module HW3.Action where

import Control.Exception (Exception, throw, throwIO)
import qualified Control.Monad.Cont as Control.Monad
import qualified Data.ByteString as B
import Data.Set
import Data.Text (pack, unpack)
import Data.Text.Encoding as Enc
import Data.Time (getCurrentTime)
import HW3.Base
import System.Directory
import System.Random (getStdRandom, mkStdGen, uniformR)
import System.Random.Stateful (IOGen (IOGen), IOGenM (IOGenM), Random (randomR), UniformRange (uniformRM), newIOGenM, randomRM)

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
    else throwIO (PermissionRequired perm)

instance HiMonad HIO where
  runAction HiActionCwd = runAction' AllowRead $ do
    path <- getCurrentDirectory
    return . HiValueString . pack $ path
  runAction (HiActionRead file) = runAction' AllowRead $ do
    bt <- B.readFile $ file
    return $ case Enc.decodeUtf8' bt of
      (Left _) -> HiValueBytes bt
      (Right t) -> HiValueString t
  runAction (HiActionWrite file bs) = runAction' AllowWrite $ do
    path <- B.writeFile file bs
    return HiValueNull
  runAction (HiActionMkDir path) = runAction' AllowWrite $ do
    createDirectory path
    return HiValueNull
  runAction (HiActionChDir path) = runAction' AllowRead $ do
    setCurrentDirectory path
    return HiValueNull
  runAction (HiActionNow) = runAction' AllowTime $ do
    x <- getCurrentTime
    return . HiValueTime $ x
  runAction (HiActionRand l r) | r < l = runAction (HiActionRand r l)
  runAction (HiActionRand l r) = HIO $ \s -> do
    rand <- getStdRandom (uniformR (l, r))
    return . HiValueNumber . toRational $ rand
  runAction (HiActionEcho t) = runAction' AllowWrite $ do
    putStrLn . unpack $ t
    return HiValueNull

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Ord, Eq)

data PermissionException
  = PermissionRequired HiPermission
  deriving (Show)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

instance Exception PermissionException