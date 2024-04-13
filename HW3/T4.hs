module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f (S g) = S $ mapAnnotated f . g

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState (S f) = S $ \x -> let (S g :# e) = f x in g e

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (S f) <*> (S x) = S (\s -> let (h :# _) = f s in mapAnnotated h $ x s)

instance Monad (State s) where
  m >>= f = joinState $ fmap f m

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)

evalBinary 
  :: Expr
  -> Expr
  -> (Double -> Double -> Double)
  -> (Double -> Double -> Prim Double)
  -> State [Prim Double] Double
evalBinary x y f op = do 
    arg1 <- eval x;
    arg2 <- eval y;
    modifyState (op arg1 arg2 :);
    return $ f arg1 arg2

evalUnary
  :: Expr
  -> (Double -> Double)
  -> (Double -> Prim Double)
  -> State [Prim Double] Double
evalUnary x f op = do 
    arg <-eval x;
    modifyState (op arg :);
    return $ f arg

eval :: Expr -> State [Prim Double] Double
eval (Val x) = pure x
eval (Op (Add x y)) = evalBinary x y (+) Add
eval (Op (Sub x y)) = evalBinary x y (-) Sub
eval (Op (Mul x y)) = evalBinary x y (*) Mul
eval (Op (Div x y)) = evalBinary x y (/) Div
eval (Op (Abs x)) = evalUnary x abs Abs
eval (Op (Sgn x)) = evalUnary x signum Sgn
