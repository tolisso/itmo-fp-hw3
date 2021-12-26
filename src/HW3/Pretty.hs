module HW3.Pretty where
import HW3.Base
import Prettyprinter.Render.Terminal
import Prettyprinter
import Data.Ratio (numerator, denominator)
import Data.Scientific
    
prettyValue :: HiValue -> (Doc AnsiStyle)
prettyValue (HiValueNumber a)
    | denominator a == 1
    = pretty (numerator a)
prettyValue (HiValueNumber a)
    | isPow10 a
    = pretty $ formatScientific Fixed Nothing 
        (normalize . fst . fromRationalRepetendUnlimited $ a)
        
prettyValue (HiValueNumber a)
    = let 
        num = numerator a
        den = denominator a 
        (d, m) = quotRem num den
        frac = show (abs m) ++ "/" ++ show den
        sign = if m >= 0 then "+" else "-"
        signWeak = if m >= 0 then "" else "-"
        in 
        if (d) /= 0 
            then pretty d <+> pretty sign <+> pretty frac
            else pretty $ signWeak ++ frac
        

prettyValue _ = error "not implemented yet"

isPow10 :: Rational -> Bool
isPow10 a = isInf (fromRationalRepetendUnlimited a) where
    isInf (_, Nothing) = True
    isInf _ = False