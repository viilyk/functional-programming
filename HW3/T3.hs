module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

joinOption :: Option (Option a) -> Option a
joinOption (Some (Some x)) = Some x
joinOption _ = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Success (Success x)) = Success x
joinExcept (Success (Error e)) = Error e
joinExcept (Error e) = Error e

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((x :# e1) :# e2) = x :# (e2 <> e1)

joinList :: List (List a) -> List a
joinList Nil = Nil
joinList (x :. xs) = myConcat x $ joinList xs

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F $ \i -> let (F g) = f i in g i
