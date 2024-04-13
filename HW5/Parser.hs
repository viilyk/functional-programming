module HW5.Parser
  ( parse
  ) where

import HW5.Base
import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Megaparsec (eof, notFollowedBy, Parsec, runParser, choice, empty, sepBy, between, optional, try, manyTill, many, (<|>))
import Text.Megaparsec.Char (char, space1, string)
import Control.Monad.Combinators.Expr (Operator(InfixL, InfixN, InfixR), makeExprParser)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Text.Megaparsec.Char.Lexer as L

type Parser =  Parsec Void String

skipSpaces :: Parser ()
skipSpaces = L.space space1 empty empty

symbol :: String -> Parser String
symbol = L.symbol skipSpaces

parenthesis :: Parser a -> Parser a
parenthesis = between (symbol "(") (symbol ")") 

pHiValueBytes :: Parser ByteString
pHiValueBytes = BS.pack <$> between (symbol "[#") (symbol "#]") (many (L.hexadecimal <* skipSpaces))

pList :: Parser HiExpr
pList = do
  args <- between (symbol "[") (symbol "]") (sepBy pExpr (symbol ","))
  return $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) args   

pHiValueString :: Parser T.Text
pHiValueString = T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

pHiValueNull :: Parser String
pHiValueNull = string "null"

pHiValueBool :: Parser Bool
pHiValueBool = choice
  [ True  <$ string "true"
  , False <$ string "false" ]

pHiValueNumber :: Parser Rational
pHiValueNumber = toRational <$> L.signed skipSpaces L.scientific

pHiValueFunction :: Parser HiFun
pHiValueFunction = choice
  [ HiFunDiv            <$ string "div"
  , HiFunMul            <$ string "mul"
  , HiFunAdd            <$ string "add"
  , HiFunSub            <$ string "sub" 
  , HiFunAnd            <$ string "and"
  , HiFunOr             <$ string "or"
  , HiFunLessThan       <$ string "less-than"
  , HiFunGreaterThan    <$ string "greater-than"
  , HiFunEquals         <$ string "equals"
  , HiFunNotLessThan    <$ string "not-less-than"
  , HiFunNotGreaterThan <$ string "not-greater-than"
  , HiFunNotEquals      <$ string "not-equals"
  , HiFunIf             <$ string "if"
  , HiFunLength         <$ string "length"
  , HiFunToUpper        <$ string "to-upper"
  , HiFunToLower        <$ string "to-lower"
  , HiFunReverse        <$ string "reverse"
  , HiFunTrim           <$ string "trim" 
  , HiFunList           <$ string "list"
  , HiFunRange          <$ string "range"
  , HiFunFold           <$ string "fold" 
  , HiFunPackBytes      <$ string "pack-bytes"
  , HiFunUnpackBytes    <$ string "unpack-bytes"
  , HiFunEncodeUtf8     <$ string "encode-utf8"
  , HiFunDecodeUtf8     <$ string "decode-utf8"
  , HiFunZip            <$ string "zip"
  , HiFunUnzip          <$ string "unzip"
  , HiFunSerialise      <$ string "serialise"
  , HiFunDeserialise    <$ string "deserialise" 
  , HiFunNot            <$ string "not" ]

pHiValue :: Parser HiValue
pHiValue = choice 
  [ HiValueNumber   <$> pHiValueNumber
  , HiValueFunction <$> pHiValueFunction 
  , HiValueBool     <$> pHiValueBool 
  , HiValueNull     <$  pHiValueNull
  , HiValueString   <$> pHiValueString 
  , HiValueBytes    <$> pHiValueBytes ]

pArguments :: Parser [HiExpr]
pArguments = parenthesis $ sepBy pExpr (symbol ",")

pApply :: HiExpr -> Parser HiExpr
pApply expr = do
  args <- optional $ pArguments <* skipSpaces
  case args of 
    Just checkedArgs -> pApply $ HiExprApply expr checkedArgs
    Nothing -> return expr

pTerm :: Parser HiExpr
pTerm = do
  expr <- (HiExprValue <$> pHiValue) <|> pList <|> parenthesis pExpr 
  _ <- skipSpaces
  pApply expr

pExpr :: Parser HiExpr
pExpr = makeExprParser pTerm operatorTable

binaryWrap :: HiFun -> HiExpr -> HiExpr -> HiExpr
binaryWrap f x y = HiExprApply (HiExprValue $ HiValueFunction f) [x, y]

binary ::
     (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr)
  -> Parser String
  -> HiFun
  -> Operator Parser HiExpr
binary c p f = c (binaryWrap f <$ p)

binaryL :: String -> HiFun -> Operator Parser HiExpr
binaryL s = binary InfixL (symbol s)

binaryN :: String -> HiFun -> Operator Parser HiExpr
binaryN s = binary InfixN (symbol s)

binaryR :: String -> HiFun -> Operator Parser HiExpr
binaryR s = binary InfixR (symbol s)

binaryLNotFollowedBy :: String -> String -> HiFun -> Operator Parser HiExpr
binaryLNotFollowedBy s next = binary InfixL $ try $ string s <* notFollowedBy (symbol next) <* skipSpaces

operatorTable :: [[Operator Parser HiExpr]]
operatorTable = 
  [
    [ binaryL "*" HiFunMul
    , binaryLNotFollowedBy "/" "=" HiFunDiv ]
    ,
    [ binaryL "+" HiFunAdd
    , binaryL "-" HiFunSub ]
    , 
    [ binaryN "<=" HiFunNotGreaterThan
    , binaryN ">=" HiFunNotLessThan
    , binaryN "<" HiFunLessThan
    , binaryN ">" HiFunGreaterThan
    , binaryN "==" HiFunEquals
    , binaryN "/=" HiFunNotEquals ]
    ,
    [ binaryR "&&" HiFunAnd ]
    , 
    [ binaryR "||" HiFunOr ]
  ]

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (between skipSpaces eof pExpr) ""
