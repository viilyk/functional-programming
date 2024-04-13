module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = (Int, Int)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (n, _) _ _ _) = n

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch (_, _) l _ r) = max (tdepth l) (tdepth r) + 1

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember k (Branch (_, _) l v r) 
  | k == v = True
  | k < v = tmember k l
  | otherwise  = tmember k r

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert k Leaf = Branch (1, 0) Leaf k Leaf
tinsert k (Branch (h, diff) l v r) 
  | k == v = (Branch (h, diff) l k r)
  | k < v && (diff + 1) == 0 = Branch (h + 1, diff + 1) (tinsert k l) v r 
  | k < v && (diff + 1) == 1 || (diff + 1) == -1 = Branch (h + 1, diff + 1) (tinsert k l) v r
  | k < v && (diff + 1) == 2 = mkR (tinsert k l) v r
  | k > v && (diff - 1) == 0 = Branch (h + 1, diff - 1) l v (tinsert k r)
  | k > v && (diff - 1) == 1 || (diff - 1) == -1 = Branch (h + 1, diff - 1) l v (tinsert k r)
  | k > v && (diff - 1) == -2 = mkL l v (tinsert k r)

mkLR :: Tree a -> a -> Tree a -> Tree a
mkLR (Branch (h, diff) ll vl lr) v r 
  | diff == 1 = Branch (tsize r + h + 1, 0) ll vl (Branch (tsize lr + tsize r + 1, 0) lr v r)
  | otherwise = Branch (tsize r + h + 1, -1) ll vl (Branch (tsize lr + tsize r + 1, 1) lr v r)

mkLL :: Tree a -> a -> Tree a -> Tree a
mkLL l v (Branch (h, diff) rl rv rr)
  | diff == -1 = Branch (tsize l + h + 1, 0) (Branch (tsize l + tsize rl + 1, 0) l v rl) rv rr
  | otherwise = Branch (tsize l + h + 1, 1) (Branch (tsize l + tsize rl + 1, -1) l v rl) rv rr

mkBL :: Tree a -> a -> Tree a -> Tree a
mkBL l v (Branch (hr, diffr) rl vr rr) = mkLL l v (mkLR rl vr rr)

mkBR :: Tree a -> a -> Tree a -> Tree a
mkBR (Branch (hl, diffl) ll vl lr) v r = mkLR (mkLL ll vl lr) v r

mkL :: Tree a -> a -> Tree a -> Tree a
mkL l v (Branch (h, diff) rl rv rr)
  | diff == -1 || diff == 0 = mkLL l v (Branch (h, diff) rl rv rr)
  | otherwise = mkBL l v (Branch (h, diff) rl rv rr)

mkR :: Tree a -> a -> Tree a -> Tree a
mkR (Branch (h, diff) ll lv lr) v r
  | diff == 1 || diff == 0 = mkLR (Branch (h, diff) ll lv lr) v r
  | otherwise = mkBR (Branch (h, diff) ll lv lr) v r

tFromList :: Ord a => [a] -> Tree a
tFromList [] = Leaf
tFromList (x:xs) = tinsert x (tFromList xs)
