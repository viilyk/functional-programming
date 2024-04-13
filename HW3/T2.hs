{-# LANGUAGE ScopedTypeVariables#-}
module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  ) where

import HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some x, Some y) = Some (x, y)
distOption _ = None

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P x1 x2, P y1 y2) = P (x1, y1) (x2, y2)

wrapPair :: a -> Pair a
wrapPair a = P a a

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q x1 x2 x3 x4, Q y1 y2 y3 y4) = Q (x1, y1) (x2, y2) (x3, y3) (x4, y4)

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (x :# e1, y :# e2) = (x, y) :# (e1 <> e2)

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated x = x :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Success x, Success y) = Success (x, y)
distExcept (Error x, _) = Error x
distExcept (_, Error x) = Error x

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low x, Low y) = Low (x, y)
distPrioritised (Medium x, Medium y) = Medium (x, y)
distPrioritised (High x, High y) = High (x, y)
distPrioritised (Low x, High y) = High (x, y)
distPrioritised (High x, Low y) = High (x, y)
distPrioritised (Medium x, High y) = High (x, y)
distPrioritised (High x, Medium y) = High (x, y)
distPrioritised (Low x, Medium y) = Medium (x, y)
distPrioritised (Medium x, Low y) = Medium (x, y)


wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (x :> xs, y :> ys) = (x, y) :> distStream (xs, ys)

wrapStream :: a -> Stream a
wrapStream x = x :> wrapStream x

distList :: (List a, List b) -> List (a, b)
distList (x :. xs, y) = myConcat (make_row y) (distList (xs, y))
  where
    make_row Nil = Nil
    make_row (h :. t) = (x, h) :. make_row t
distList _ = Nil

wrapList :: a -> List a
wrapList x = x :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F (\x -> (f x, g x))

wrapFun :: a -> Fun i a
wrapFun x = F (const x)
