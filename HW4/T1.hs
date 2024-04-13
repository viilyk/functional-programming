module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types
import qualified Control.Monad

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }


mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e) = Error e
mapExcept f (Success x) = Success $ f x

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (x :# e) = f x :# e

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES g) = ES $ mapExcept (mapAnnotated f) . g

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ Success . (a :#)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES f) = ES $ \s -> joinUnwrap $ f s
  where
    joinUnwrap (Error s) = Error s
    joinUnwrap (Success ((ES g) :# e)) = g e


modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success $ () :# f s

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = Control.Monad.ap

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState $ fmap f m 

data EvaluationError = DivideByZero
  deriving Show
evalBinary 
  :: Expr
  -> Expr
  -> (Double -> Double -> ExceptState EvaluationError [Prim Double] Double)
  -> (Double -> Double -> Prim Double)
  -> ExceptState EvaluationError [Prim Double] Double
evalBinary x y f op = do 
    arg1 <- eval x;
    arg2 <- eval y;
    modifyExceptState (op arg1 arg2 :);
    f arg1 arg2

exeptedEval
  :: (Double -> Double -> Double)
  -> Double
  -> Double
  -> ExceptState EvaluationError [Prim Double] Double
exeptedEval f x y = return $ f x y 

exeptedDiv
  :: Double
  -> Double
  -> ExceptState EvaluationError [Prim Double] Double
exeptedDiv _ 0 = throwExceptState DivideByZero
exeptedDiv x y = exeptedEval (/) x y

evalUnary
  :: Expr
  -> (Double -> Double)
  -> (Double -> Prim Double)
  -> ExceptState EvaluationError [Prim Double] Double
evalUnary x f op = do 
    arg <-eval x;
    modifyExceptState (op arg :);
    return $ f arg

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = pure x
eval (Op (Add x y)) = evalBinary x y (exeptedEval (+)) Add
eval (Op (Sub x y)) = evalBinary x y (exeptedEval (-)) Sub
eval (Op (Mul x y)) = evalBinary x y (exeptedEval (*)) Mul
eval (Op (Div x y)) = evalBinary x y exeptedDiv Div
eval (Op (Abs x)) = evalUnary x abs Abs
eval (Op (Sgn x)) = evalUnary x signum Sgn