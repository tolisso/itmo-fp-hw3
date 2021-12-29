module HW3.Pretty where

import Control.Applicative.Combinators (between)
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Ratio (denominator, numerator)
import Data.Scientific
import qualified Data.Sequence as S
import Data.Text
import HW3.Base
import Prettyprinter
import Prettyprinter.Render.Terminal

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
  pretty "[ "
    <> ( F.fold
           . L.intersperse (pretty ", ")
           . Prelude.map (prettyValue)
           . F.toList
           $ arr
       )
    <> pretty " ]"

isPow10 :: Rational -> Bool
isPow10 a = isInf (fromRationalRepetendUnlimited a)
  where
    isInf (_, Nothing) = True
    isInf _ = False