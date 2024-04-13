module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty(..))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn x = foldr f ([] :| [])
  where 
    f a (l :| t) 
      | a == x    = [] :| (l : t)
      | otherwise = (a : l) :| t

joinWith :: a -> NonEmpty [a] -> [a]
joinWith x = foldr1 (\a l -> a ++ (x : l))
    