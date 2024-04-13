module HW5.Pretty
  ( prettyValue
  , prettyEvalError
  , prettyParserError
  ) where


import HW5.Base 
import Prettyprinter (Doc, pretty, viaShow, list, (<+>))
import Data.Foldable (toList)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Data.Scientific (FPFormat(Fixed), fromRationalRepetendUnlimited, formatScientific)
import GHC.Real (Ratio (..))
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Data.Void (Void)
import Data.ByteString (ByteString)
import Numeric (showHex)
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq


prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueFunction f) = prettyFunction f
prettyValue (HiValueNumber (p :% 1)) = pretty p
prettyValue (HiValueNumber x@(p :% q)) = case fromRationalRepetendUnlimited x of 
  (a, Nothing) -> pretty (formatScientific Fixed Nothing a)
  _ -> prettyFractional $ quotRem p q 
    where 
      sign val = if val < 0 
        then "-" 
        else "+"
      prettyFractional (0, m) = pretty (show m <> "/" <> show q)
      prettyFractional (n, m) = pretty n <+> pretty (sign m) <+> prettyFractional (0, abs m)
      
prettyValue (HiValueBool x) = prettyBool x
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueString x) = viaShow x
prettyValue (HiValueList x) = prettyList x
prettyValue (HiValueBytes x) = prettyBytes x

prettyFunction:: HiFun -> Doc AnsiStyle
prettyFunction HiFunAdd            = pretty "add"
prettyFunction HiFunSub            = pretty "sub"
prettyFunction HiFunMul            = pretty "mul"
prettyFunction HiFunDiv            = pretty "div"
prettyFunction HiFunNot            = pretty "not"
prettyFunction HiFunAnd            = pretty "and"
prettyFunction HiFunOr             = pretty "or"
prettyFunction HiFunLessThan       = pretty "less-than"
prettyFunction HiFunGreaterThan    = pretty "greater-than"
prettyFunction HiFunEquals         = pretty "equals"
prettyFunction HiFunNotLessThan    = pretty "not-less-than"
prettyFunction HiFunNotGreaterThan = pretty "not-greater-than"
prettyFunction HiFunNotEquals      = pretty "not-equals"
prettyFunction HiFunIf             = pretty "if"
prettyFunction HiFunLength         = pretty "length"
prettyFunction HiFunToUpper        = pretty "to-upper"
prettyFunction HiFunToLower        = pretty "to-lower"
prettyFunction HiFunReverse        = pretty "reverse"
prettyFunction HiFunTrim           = pretty "trim"
prettyFunction HiFunList           = pretty "list"
prettyFunction HiFunRange          = pretty "range"
prettyFunction HiFunFold           = pretty "fold"
prettyFunction HiFunPackBytes      = pretty "pack-bytes"
prettyFunction HiFunUnpackBytes    = pretty "unpack-bytes"
prettyFunction HiFunEncodeUtf8     = pretty "encode-utf8"
prettyFunction HiFunDecodeUtf8     = pretty "decode-utf8"
prettyFunction HiFunZip            = pretty "zip"
prettyFunction HiFunUnzip          = pretty "unzip"
prettyFunction HiFunSerialise      = pretty "serialise"
prettyFunction HiFunDeserialise    = pretty "deserialise"

prettyBool :: Bool -> Doc AnsiStyle
prettyBool True  = pretty "true"
prettyBool False = pretty "false"

prettyList :: Seq.Seq HiValue -> Doc AnsiStyle
prettyList = list . toList . fmap prettyValue

prettyBytes :: ByteString -> Doc AnsiStyle
prettyBytes x = pretty ("[# " <> concatMap (space . complement . flip showHex "") (BS.unpack x) <> " #]") 
  where 
    complement s = if length s == 1 
      then " 0" <> s
      else s
    space s = s <> " "

prettyEvalError :: HiError -> Doc AnsiStyle
prettyEvalError HiErrorInvalidArgument = pretty "Invalid argument"
prettyEvalError HiErrorInvalidFunction = pretty "Invalid function"
prettyEvalError HiErrorArityMismatch   = pretty "Arity mismatch"
prettyEvalError HiErrorDivideByZero    = pretty "Divide by zero"

prettyParserError :: ParseErrorBundle String Void -> Doc AnsiStyle
prettyParserError x = pretty (errorBundlePretty x)