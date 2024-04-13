{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Numeric.Natural (Natural)
import Control.Applicative
import Control.Monad

import HW4.Types
import HW4.T1 (ExceptState(..))

data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e) = Error e
mapExcept f (Success x) = Success $ f x

runP :: Parser a -> String -> Except ParseError a
runP (P (ES f)) str = mapExcept unwrapAnnotated $ f (0, str)
  where
    unwrapAnnotated (a :# _) = a

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (P (ES x)) <|> (P (ES y)) = P $ ES $ \s -> 
    case x s of
      Error _ -> y s
      a -> a
-- No metohds
instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) -> 
  case s of 
    [] -> Success $ () :# (pos, [])
    _ -> Error $ ErrorAtPos pos

pExpr :: Parser Expr
pExpr = P $ ES $ 

parseExpr :: String -> Except ParseError Expr
parseExpr = runP $ pExpr <* pEof
