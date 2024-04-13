module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  Last x <> l = x :+ l 
  (x1 :+ ys1) <> l = x1 :+ (ys1 <> l)

data Inclusive a b = This a | That b | Both a b
  deriving Show

-- You may necessary constraints there
instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This x  <>  This y  =  This (x <> y)
  That x  <>  That y  =  That (x <> y)
  Both x1 y1 <>  Both x2 y2  =  Both (x1 <> x2) (y1 <> y2)
  This x <> That y = Both x y
  That x <> This y = Both y x
  This x1 <> Both x2 y = Both (x1 <> x2) y
  Both x1 y <> This x2 = Both (x1 <> x2) y
  That y1 <> Both x y2 = Both x (y1 <> y2)
  Both x y1 <> That y2 = Both x (y1 <> y2)

newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  DS "" <> x = x
  x <> DS "" = x
  DS a <> DS b = DS (a ++ "." ++ b)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F a <> F b = F (a . b)

instance Monoid (Fun a) where
  mempty = F id
