module HW5.Evaluator
  ( eval
  ) where

import HW5.Base
import Control.Monad
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.Text as T
import Data.Semigroup (stimes)
import GHC.Real (Ratio (..))
import Data.Sequence (Seq ((:<|)))
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS


getNumber :: Monad m => Rational -> ExceptT HiError m HiValue
getNumber = return . HiValueNumber

getBool :: Monad m => Bool-> ExceptT HiError m HiValue
getBool = return . HiValueBool

getString :: Monad m => T.Text -> ExceptT HiError m HiValue
getString = return . HiValueString

getList :: Monad m => Seq HiValue -> ExceptT HiError m HiValue
getList = return . HiValueList

getBytes :: Monad m => ByteString -> ExceptT HiError m HiValue
getBytes = return . HiValueBytes

evalFunction :: Monad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
evalFunction HiFunDiv [HiValueNumber _, HiValueNumber 0] = throwE HiErrorDivideByZero
evalFunction HiFunDiv [HiValueNumber x, HiValueNumber y] = getNumber (x / y)
evalFunction HiFunDiv [HiValueString x, HiValueString y] = getString (x <> T.pack "/" <> y)

evalFunction HiFunMul [HiValueNumber x, HiValueNumber y]        = getNumber (x * y)
evalFunction HiFunMul [HiValueString x, HiValueNumber (p :% 1)] = getString (stimes p x)
evalFunction HiFunMul [HiValueList x, HiValueNumber (p :% 1)]   = getList (stimes p x)
evalFunction HiFunMul [HiValueBytes x, HiValueNumber (p :% 1)]  = getBytes (stimes p x)

evalFunction HiFunAdd [HiValueNumber x, HiValueNumber y] = getNumber (x + y)
evalFunction HiFunAdd [HiValueString x, HiValueString y] = getString (x <> y)
evalFunction HiFunAdd [HiValueList x, HiValueList y]     = getList (x <> y)
evalFunction HiFunAdd [HiValueBytes x, HiValueBytes y]   = getBytes (x <> y)

evalFunction HiFunSub [HiValueNumber x, HiValueNumber y] = getNumber (x - y)

evalFunction HiFunNot [HiValueBool x]                = getBool (not x)
evalFunction HiFunAnd [HiValueBool x, HiValueBool y] = getBool (x && y)
evalFunction HiFunOr [HiValueBool x, HiValueBool y]  = getBool (x || y)
evalFunction HiFunLessThan [x, y]                    = getBool (x < y)
evalFunction HiFunGreaterThan [x, y]                 = getBool (x > y)
evalFunction HiFunEquals [x, y]                      = getBool (x == y)
evalFunction HiFunNotLessThan [x, y]                 = getBool (x >= y)
evalFunction HiFunNotGreaterThan [x, y]              = getBool (x <= y)
evalFunction HiFunNotEquals [x, y]                   = getBool (x /= y)

evalFunction HiFunIf [HiValueBool True, x, _]  = return x
evalFunction HiFunIf [HiValueBool False, _, y] = return y

evalFunction HiFunLength [HiValueString x]  = getNumber $ toRational $ T.length x
evalFunction HiFunLength [HiValueList x]    = getNumber $ toRational $ Seq.length x
evalFunction HiFunToUpper [HiValueString x] = getString $ T.toUpper x
evalFunction HiFunToLower [HiValueString x] = getString $ T.toLower x
evalFunction HiFunReverse [HiValueString x] = getString $ T.reverse x
evalFunction HiFunReverse [HiValueList x]   = getList $ Seq.reverse x
evalFunction HiFunTrim [HiValueString x]    = getString $ T.strip x

evalFunction HiFunList args = getList $ Seq.fromList args
evalFunction HiFunRange [HiValueNumber x, HiValueNumber y] = 
  getList $ Seq.fromList $ HiValueNumber <$> [x..y]
evalFunction HiFunFold [HiValueFunction _, HiValueList Seq.Empty] = return HiValueNull
evalFunction HiFunFold [HiValueFunction f, HiValueList (x :<| y)] = 
  foldM (\a b -> evalFunction f [a, b]) x y

evalFunction HiFunPackBytes [HiValueList x] =  HiValueBytes . BS.pack . toList <$> mapM byte x
  where
    byte (HiValueNumber (p :% 1)) = if p >= 0 && p <= 255
      then return $ fromIntegral p
      else throwE HiErrorInvalidArgument
    byte _ = throwE HiErrorInvalidArgument
evalFunction HiFunUnpackBytes [HiValueBytes x] = getList $ Seq.fromList

evalFunction _ _ = throwE HiErrorInvalidArgument

evalSlice :: 
  Monad m => [a]
  -> [HiValue]
  -> Int
  -> (a -> HiValue)
  -> ([a] -> HiValue)
  -> ExceptT HiError m HiValue
evalSlice arr args len ind slice = case args of
  [HiValueNumber (x :% 1)] -> if x >= 0 && x < fromIntegral len
    then return $ ind $ arr !! fromIntegral x
    else return HiValueNull
  [HiValueNumber (x :% 1), HiValueNumber (y :% 1)] -> 
    return $ slice $ take (y' - x') $ drop x' arr
    where
      complement a = if a < 0
          then fromIntegral a + len
          else fromIntegral a
      x' = complement x
      y' = complement y
  [a@(HiValueNumber (_ :% 1)), HiValueNull] -> evalSlice arr [a, HiValueNumber $ toRational len] len ind slice
  [HiValueNull, b@(HiValueNumber (_ :% 1))] -> evalSlice arr [HiValueNumber (0 :% 1), b] len ind slice
  _ -> throwE HiErrorInvalidArgument

checkArity' :: Monad m => [HiValue] -> Int -> ExceptT HiError m ()
checkArity' args k = if length args == k 
  then return ()
  else throwE HiErrorArityMismatch

checkArityFunction :: Monad m => HiFun -> [HiValue] -> ExceptT HiError m ()
checkArityFunction f args = checkArity' args $ case f of 
  HiFunDiv ->  2
  HiFunMul ->  2
  HiFunAdd ->  2
  HiFunSub ->  2
  HiFunNot ->  1
  HiFunAnd ->  2
  HiFunOr ->  2
  HiFunLessThan -> 2
  HiFunGreaterThan -> 2
  HiFunEquals -> 2
  HiFunNotLessThan -> 2
  HiFunNotGreaterThan -> 2
  HiFunNotEquals -> 2
  HiFunIf -> 3
  HiFunLength -> 1
  HiFunToUpper -> 1
  HiFunToLower -> 1
  HiFunReverse -> 1
  HiFunTrim -> 1
  HiFunRange -> 2
  HiFunList -> length args
  HiFunFold -> 2
  HiFunPackBytes -> 1
  HiFunUnpackBytes -> 1
  HiFunEncodeUtf8 -> 1
  HiFunDecodeUtf8 -> 1
  HiFunZip -> 1
  HiFunUnzip -> 1
  HiFunSerialise -> 1
  HiFunDeserialise -> 1

checkAritySlice :: Monad m => [HiValue] -> ExceptT HiError m ()
checkAritySlice [_, _] = return ()
checkAritySlice [_] = return ()
checkAritySlice _ = throwE HiErrorArityMismatch


eval' :: Monad m => HiExpr -> ExceptT HiError m HiValue
eval' (HiExprValue val) = return val
eval' (HiExprApply expr args) = do 
  val <- eval' expr
  case val of 
    (HiValueFunction f) -> do 
      evaled <- mapM eval' args
      _ <- checkArityFunction f evaled
      evalFunction f evaled
    (HiValueString s) -> do
      evaled <- mapM eval' args
      _ <- checkAritySlice evaled
      evalSlice (T.unpack s) evaled (T.length s) (HiValueString . T.pack . (:[])) (HiValueString . T.pack)
    (HiValueList l) -> do
      evaled <- mapM eval' args
      _ <- checkAritySlice evaled
      evalSlice (toList l) evaled (Seq.length l) id (HiValueList . Seq.fromList)
    _ -> throwE HiErrorInvalidFunction
    

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (eval' expr)
