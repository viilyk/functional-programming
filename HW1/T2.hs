module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus Z n = n
nplus n Z = n
nplus (S n1) (S n2) = S $ nplus (S n1) n2

nmult :: N -> N -> N
nmult Z _ = Z
nmult _ Z = Z
nmult (S n1) (S n2) = nplus (S n1) (nmult n2 $ S n1)

nsub :: N -> N -> Maybe N
nsub n Z = Just n 
nsub Z _ = Nothing
nsub (S n1) (S n2) = nsub n1 n2

ncmp :: N -> N -> Ordering
ncmp Z Z = EQ
ncmp Z (S _) = LT
ncmp (S _) Z = GT
ncmp (S n1) (S n2) = ncmp n1 n2

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S $ nFromNatural (n - 1)

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S n) = nToNum n + 1

nEven :: N -> Bool
nEven Z = True
nEven (S Z) = False
nEven (S (S n)) = nEven n

nOdd :: N -> Bool
nOdd = not . nEven

ndiv :: N -> N -> N
ndiv _ Z = error "division by zero"
ndiv Z _ = Z
ndiv n1 n2 = maybe Z (\x -> S $ ndiv x n2) (nsub n1 n2)


nmod :: N -> N -> N
nmod _ Z = error "division by zero"
nmod Z _ = Z
nmod n1 n2 = maybe n1 (\x -> nmod x n2) (nsub n1 n2)
