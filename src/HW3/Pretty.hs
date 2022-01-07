module HW3.Pretty where

import Control.Applicative.Combinators (between)
import qualified Control.Applicative.Combinators as F
import qualified Data.ByteString as B
import Data.Char
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as M
import Data.Ratio (denominator, numerator)
import Data.Scientific
import qualified Data.Sequence as S
import Data.Text
import HW3.Base
import Numeric (showHex)
import Prettyprinter
import Prettyprinter.Render.Terminal
import Text.Printf (printf)

prettyValue :: HiValue -> (Doc AnsiStyle)
-- number
-- denminator is 1
prettyValue (HiValueNumber a)
  | denominator a == 1 =
    pretty (numerator a)
-- denominator is ten power
prettyValue (HiValueNumber a)
  | isPow10 a =
    pretty $
      formatScientific
        Fixed
        Nothing
        (normalize . fst . fromRationalRepetendUnlimited $ a)
-- standard rational
prettyValue (HiValueNumber a) =
  let num = numerator a
      den = denominator a
      (d, m) = quotRem num den
      frac = show (abs m) ++ "/" ++ show den
      sign = if m >= 0 then "+" else "-"
      signWeak = if m >= 0 then "" else "-"
   in if (d) /= 0
        then pretty d <+> pretty sign <+> pretty frac
        else pretty $ signWeak ++ frac
-- bool
prettyValue (HiValueBool True) = pretty "true"
prettyValue (HiValueBool False) = pretty "false"
-- function
prettyValue (HiValueFunction f) = pretty $ funcStr f
-- null
prettyValue (HiValueNull) = pretty "null"
-- string
prettyValue (HiValueString str) = pretty $ "\"" ++ (unpack str) ++ "\""
-- list
prettyValue (HiValueList arr) =
  if S.null arr
    then pretty "[ ]"
    else
      pretty "[ "
        <> prettyArgs (F.toList arr)
        <> pretty " ]"
-- bytes
prettyValue (HiValueBytes str) =
  pretty "[# "
    <> ( F.fold
           . Prelude.map
             -- Word8 -> enum -> hex string -> Doc (wich pretty returns)
             ( pretty
                 . (\s -> printf "%02x " s :: String)
                 . fromEnum
             )
           . B.unpack
           $ str
       )
    <> pretty "#]"
-- action
prettyValue (HiValueAction (HiActionRead path)) =
  prettyAction "read" [str (path)]
prettyValue (HiValueAction (HiActionWrite path sb)) =
  prettyAction "write" [str path, HiValueBytes sb]
prettyValue (HiValueAction (HiActionMkDir path)) =
  prettyAction "mkdir" [str path]
prettyValue (HiValueAction (HiActionChDir path)) =
  prettyAction "cd" [str path]
prettyValue (HiValueAction HiActionCwd) = pretty "cwd"
prettyValue (HiValueAction HiActionNow) = pretty "now"
prettyValue (HiValueAction (HiActionRand l r)) =
  prettyAction "rand" [int l, int r]
prettyValue (HiValueAction (HiActionEcho t)) =
  prettyAction "echo" [HiValueString t]
-- time
prettyValue (HiValueTime time) =
  pretty "parse-time(\""
    <> pretty (show time)
    <> pretty "\")"
-- dict
prettyValue (HiValueDict m) =
  pretty "{ "
    <> ( F.fold
           . L.intersperse (pretty ", ")
           . Prelude.map (\(a, b) -> prettyValue a <> (pretty ": ") <> prettyValue b)
           . M.assocs
           $ m
       )
    <> pretty " }"

prettyAction :: String -> [HiValue] -> Doc AnsiStyle
prettyAction name args =
  pretty name
    <> pretty "("
    <> prettyArgs args
    <> pretty ")"

str s = HiValueString (pack s)

int i = HiValueNumber . toRational $ i

prettyArgs args =
  F.fold
    . L.intersperse (pretty ", ")
    . Prelude.map (prettyValue)
    $ args

isPow10 :: Rational -> Bool
isPow10 a = isInf (fromRationalRepetendUnlimited a)
  where
    isInf (_, Nothing) = True
    isInf _ = False