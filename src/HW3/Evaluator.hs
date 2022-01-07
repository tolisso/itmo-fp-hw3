{-# LANGUAGE OverloadedStrings #-}

module HW3.Evaluator where

import Codec.Compression.GZip (compressLevel)
import qualified Codec.Compression.Zlib as Zl
import qualified Codec.Serialise as Ser
import Control.Applicative (liftA2)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader (ReaderT)
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString as B
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict, transpose)
import qualified Data.Foldable as F
import Data.List as L
import qualified Data.Map as M
import Data.Ratio (denominator, numerator)
import Data.Semigroup (Semigroup (stimes))
import qualified Data.Sequence as S
import Data.Set (Set)
import Data.Text as T
import Data.Text.Encoding as Enc
import Data.Text.Encoding.Error (UnicodeException)
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import qualified Data.Word as W
import HW3.Action
import HW3.Base
import Text.Read (readMaybe)

type Status = ExceptT HiError

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval ex = convertM $ evalM ex

convertM :: HiMonad m => Status m HiValue -> m (Either HiError HiValue)
convertM (ExceptT val) = val

-- `eval`'s in-depth realization with
evalM :: HiMonad m => HiExpr -> Status m HiValue
evalM (HiExprValue v) = return v
evalM (HiExprApply f args) = do
  fr <- evalM f
  if reduced fr
    then do
      values <- traverse (\v -> evalM v) args
      apply fr values
    else applyFull fr args
evalM (HiExprRun exp) = do
  x <- evalM exp
  case x of
    (HiValueAction act) -> lift $ runAction act
    _ -> throwError HiErrorInvalidArgument
evalM (HiExprDict arr) = do
  xs <- traverse (evalM . fst) $ arr
  ys <- traverse (evalM . snd) $ arr
  return . HiValueDict . M.fromList $ Prelude.zip xs ys

-- determine in `evalM` what we need to use, `apply` or `applyFull`
reduced :: HiValue -> Bool
reduced (HiValueFunction HiFunIf) = False
reduced (HiValueFunction HiFunAnd) = False
reduced (HiValueFunction HiFunOr) = False
reduced _ = True

toBool' :: HiMonad m => HiValue -> Status m Bool
toBool' (HiValueBool True) = return True
toBool' (HiValueBool False) = return False
toBool' (HiValueNull) = return False
toBool' _ = throwError HiErrorInvalidArgument

toBool :: HiMonad m => HiExpr -> Status m Bool
toBool x = do
  xr <- evalM x
  toBool' $ xr

applyFull :: HiMonad m => HiValue -> [HiExpr] -> Status m HiValue
-- if
applyFull (HiValueFunction HiFunIf) [condExpr, a, b] = do
  cond <- evalM condExpr
  applyIf cond
  where
    applyIf (HiValueBool False) = evalM b
    applyIf (HiValueNull) = evalM b
    applyIf _ = evalM a
-- bool
applyFull (HiValueFunction HiFunAnd) [a, b] = do
  ra <- evalM a
  case ra of
    (HiValueBool False) -> return (HiValueBool False)
    HiValueNull -> return HiValueNull
    _ -> evalM b
applyFull (HiValueFunction HiFunOr) [a, b] = do
  ra <- evalM a
  case ra of
    (HiValueBool False) -> evalM b
    HiValueNull -> evalM b
    x -> return x
-- other
applyFull (HiValueFunction f) args = do
  check (Prelude.length args == numArgs f) HiErrorArityMismatch
  throwError HiErrorInvalidArgument
applyFull _ _ = throwError HiErrorInvalidFunction

apply :: HiMonad m => HiValue -> [HiValue] -> Status m HiValue
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
  let xi = fromIntegral x
  return $
    if checkBoundsStr s xi
      then HiValueString . pack $ [T.index s xi]
      else HiValueNull
apply (HiValueString s) [(HiValueNumber a), (HiValueNumber b)] =
  slice s a b T.length substr $ \str ->
    return . HiValueString $ str
apply (HiValueString s) [x, (HiValueNull)] =
  apply (HiValueString s) [x, (HiValueNumber . toRational $ T.length s)]
apply (HiValueString s) [(HiValueNull), y] =
  apply (HiValueString s) [(HiValueNumber 0), y]
apply (HiValueString _) args = argsError args [1, 2]
-- list
apply (HiValueFunction HiFunList) arr =
  return . HiValueList . S.fromList $ arr
apply (HiValueFunction HiFunReverse) [HiValueList arr] =
  return . HiValueList . S.reverse $ arr
apply (HiValueFunction HiFunFold) [(HiValueFunction f), HiValueList arr] = do
  if S.null $ arr
    then return $ HiValueNull
    else
      Prelude.foldl1
        ( \x y -> do
            xr <- x
            yr <- y
            evalM $
              HiExprApply
                (HiExprValue . HiValueFunction $ f)
                [HiExprValue xr, HiExprValue yr]
        )
        (Prelude.map return (F.toList arr))
apply (HiValueFunction HiFunRange) [(HiValueNumber x), (HiValueNumber y)] =
  return
    . HiValueList
    . S.fromList
    . Prelude.map (HiValueNumber . toRational)
    $ [x .. y]
apply (HiValueFunction HiFunLength) [(HiValueList arr)] =
  return . HiValueNumber . toRational $ S.length arr
apply (HiValueList arr) [(HiValueNumber i)] = do
  x <- getInt i
  let xi = fromIntegral x
  return $
    if checkBoundsSeq arr xi
      then HiValueList . S.singleton $ S.index arr xi
      else HiValueNull
apply (HiValueList arr) [(HiValueNumber a), (HiValueNumber b)] =
  slice arr a b S.length subseq $ \seq ->
    return . HiValueList $ seq
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
apply (HiValueList _) args = argsError args [1, 2]
-- bytes
apply (HiValueFunction HiFunPackBytes) [(HiValueList arr)] = do
  vals <- traverse (toWord8) (F.toList arr)
  return . HiValueBytes . B.pack $ vals
apply (HiValueFunction HiFunUnpackBytes) [(HiValueBytes bs)] =
  do
    return
    . HiValueList
    . S.fromList
    . Prelude.map (HiValueNumber . fromIntegral)
    . B.unpack
    $ bs
apply (HiValueFunction HiFunSerialise) [arg] =
  return . HiValueBytes . toStrict . Ser.serialise $ arg
apply (HiValueFunction HiFunDeserialise) [HiValueBytes bs] = do
  let res = Ser.deserialise . fromStrict $ bs
  return (res :: HiValue)
apply (HiValueFunction HiFunDecodeUtf8) [HiValueBytes bt] = do
  let res = Enc.decodeUtf8' bt
  return . decodeResToValue $ res
apply (HiValueFunction HiFunEncodeUtf8) [HiValueString t] = do
  return . HiValueBytes . Enc.encodeUtf8 $ t
apply (HiValueFunction HiFunZip) [HiValueBytes bt] =
  do
    return
    . HiValueBytes
    . toStrict
    . compress
    . fromStrict
    $ bt
apply (HiValueFunction HiFunUnzip) [HiValueBytes bt] =
  return
    . HiValueBytes
    . toStrict
    . Zl.decompress
    . fromStrict
    $ bt
apply (HiValueFunction HiFunAdd) [HiValueBytes a, HiValueBytes b] =
  return
    . HiValueBytes
    $ B.append a b
apply (HiValueFunction HiFunMul) [HiValueBytes a, HiValueNumber n] =
  mulBytes n a
apply (HiValueFunction HiFunMul) [HiValueNumber n, HiValueBytes a] =
  mulBytes n a
apply (HiValueBytes bt) [HiValueNumber n] =
  do
    x <- getInt n
    let xi = fromIntegral x
    return $
      if checkBoundsByte bt xi
        then HiValueNumber . fromInteger . toInteger . (`B.index` xi) $ bt
        else HiValueNull
apply (HiValueBytes bt) [HiValueNumber a, HiValueNumber b] =
  slice bt a b B.length subbyte $
    \bt -> return . HiValueBytes $ bt
apply (HiValueBytes _) args = argsError args [1, 2]
-- io actions
apply (HiValueFunction HiFunRead) [HiValueString str] =
  return . HiValueAction . HiActionRead . unpack $ str
apply (HiValueFunction HiFunWrite) [HiValueString str, HiValueBytes bt] =
  return . HiValueAction $ HiActionWrite (unpack str) bt
apply (HiValueFunction HiFunChDir) [HiValueString str] =
  return . HiValueAction . HiActionChDir . unpack $ str
apply (HiValueFunction HiFunMkDir) [HiValueString str] =
  return . HiValueAction . HiActionMkDir . unpack $ str
apply (HiValueFunction HiFunEcho) [HiValueString str] =
  return . HiValueAction . HiActionEcho $ str
-- time
apply (HiValueFunction HiFunParseTime) [HiValueString t] =
  case readMaybe (unpack t) of
    (Nothing) -> throwError HiErrorInvalidArgument
    (Just t) -> return . HiValueTime $ t
apply (HiValueFunction HiFunAdd) [HiValueTime t, HiValueNumber n] =
  addTime t n
apply (HiValueFunction HiFunAdd) [HiValueNumber n, HiValueTime t] =
  addTime t n
apply (HiValueFunction HiFunSub) [HiValueTime x, HiValueTime y] = do
  return . HiValueNumber . toRational $ diffUTCTime x y
-- rand
apply (HiValueFunction HiFunRand) [HiValueNumber x, HiValueNumber y] = do
  l <- fromInteger <$> getInt x
  r <- fromInteger <$> getInt y
  return . HiValueAction $ HiActionRand l r
-- dict
apply (HiValueDict m) [k] = do
  case M.lookup k m of
    Nothing -> throwError HiErrorInvalidArgument
    (Just v) -> return v
apply (HiValueDict _) args = argsError args [1]
apply (HiValueFunction HiFunKeys) [HiValueDict m] = convertMap m fst
apply (HiValueFunction HiFunValues) [HiValueDict m] = convertMap m snd
apply (HiValueFunction HiFunCount) [HiValueString t] =
  countToMap (unpack t) (\x -> HiValueString (pack [x]))
apply (HiValueFunction HiFunCount) [HiValueList l] =
  countToMap (F.toList l) id
apply (HiValueFunction HiFunCount) [HiValueBytes bt] =
  countToMap (B.unpack $ bt) intToValue
-- other
apply (HiValueFunction f) args = do
  check (Prelude.length args == numArgs f) HiErrorArityMismatch
  throwError HiErrorInvalidArgument
apply _ _ = throwError HiErrorInvalidFunction

-- assertion
check :: HiMonad m => Bool -> HiError -> Status m ()
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

getInt :: HiMonad m => Rational -> Status m Integer
getInt n | denominator n /= 1 = throwError HiErrorInvalidArgument
getInt n = return . fromIntegral . numerator $ n

nTimes ::
  (HiMonad m, Semigroup a) =>
  Rational ->
  a ->
  (a -> HiValue) ->
  Status m HiValue
nTimes n s val = do
  x <- getInt n
  if x <= 0
    then throwError HiErrorInvalidArgument
    else
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

checkBoundsSeq :: S.Seq HiValue -> Int -> Bool
checkBoundsSeq arr = checkBounds $ S.length arr

checkBoundsByte :: B.ByteString -> Int -> Bool
checkBoundsByte arr = checkBounds $ B.length arr

-- abstraction converting `Rational` borders to `Int` and apply last argument function
slice ::
  HiMonad m =>
  t -> -- what to slice
  Rational -> -- l rational
  Rational -> -- r rational
  (t -> Int) -> -- length
  (Int -> Int -> t -> t) -> -- func to get slice with int borders
  (t -> Status m HiValue) -> -- func to get slice with int borders
  Status m HiValue
slice arr a b len getSlice wrap = do
  x <- fromIntegral <$> getInt a
  y <- fromIntegral <$> getInt b
  let l = len arr
  let start' = if x >= 0 then x else x + l
  let end' = if y >= 0 then y else y + l
  let start = limit 0 l start'
  let end = limit 0 l end'
  wrap . getSlice start end $ arr
  where
    limit a _ i | i < a = 0
    limit _ b i | b <= i = b
    limit _ _ i = i

substr :: Int -> Int -> Text -> Text
substr l r t = T.drop l . T.take r $ t

subseq :: Int -> Int -> S.Seq HiValue -> S.Seq HiValue
subseq l r t = S.drop l . S.take r $ t

subbyte :: Int -> Int -> B.ByteString -> B.ByteString
subbyte l r t = B.drop l . B.take r $ t

toWord8 :: HiMonad m => HiValue -> Status m W.Word8
toWord8 (HiValueNumber n) = do
  i <- getInt n
  if 0 <= i && i <= 255
    then return (fromIntegral i)
    else throwError HiErrorInvalidArgument
toWord8 _ = throwError HiErrorInvalidArgument

decodeResToValue :: Either UnicodeException Text -> HiValue
decodeResToValue (Left _) = HiValueNull
decodeResToValue (Right t) = HiValueString t

compress =
  Zl.compressWith
    Zl.defaultCompressParams
      { compressLevel = Zl.bestCompression
      }

mulBytes n a = do
  i <- getInt n
  return . HiValueBytes . stimes i $ a

addTime ::
  HiMonad m =>
  UTCTime ->
  Rational ->
  Status m HiValue
addTime t n = do
  i <- getInt n
  return . HiValueTime $ addUTCTime (fromIntegral i) t

argsError :: HiMonad m => [HiValue] -> [Int] -> Status m a
argsError args accepted =
  let sz = Prelude.length args
   in if sz `elem` accepted
        then throwError HiErrorInvalidArgument
        else throwError HiErrorArityMismatch

convertMap ::
  HiMonad m =>
  (M.Map HiValue HiValue) ->
  ((HiValue, HiValue) -> HiValue) ->
  Status m HiValue
convertMap m f =
  return . HiValueList . S.fromList . Prelude.map f . M.toList $ m

mapInc :: Ord a => M.Map a Int -> a -> M.Map a Int
mapInc m x = case M.lookup x m of
  Nothing -> M.insert x 1 m
  (Just freq) -> M.insert x (freq + 1) m

intToValue :: Integral a => a -> HiValue
intToValue = HiValueNumber . fromInteger . toInteger

countToMap :: (Ord a, HiMonad m) => [a] -> (a -> HiValue) -> Status m HiValue
countToMap arr toKey =
  let map = M.assocs $ F.foldl mapInc M.empty arr
   in return
        . HiValueDict
        . M.fromList
        . Prelude.map (Bi.bimap toKey intToValue)
        $ map