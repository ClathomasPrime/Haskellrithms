module Heap
  ( Heap(..)
  , emptyHeap
  , isEmpty
  , peek
  , insert
  , fromList
  , toList
  , delete
  , MinEntry(..)
  , MaxEntry(..)
  ) where

import Claylude
import Tree

data Heap a = Heap
  { cHeapTree :: Tree a
  , nextOpen :: [Dir]
  } deriving(Show)

emptyHeap :: Heap a
emptyHeap = Heap Nil []

isEmpty :: Heap a -> Bool
isEmpty (Heap Nil _) = True
isEmpty _ = False

peek :: (Ord a) => Heap a -> Maybe a
peek (Heap Nil []) = Nothing --Once again, let it fail
peek (Heap (Tree max _ _) _) = Just max

insert :: (Ord a) => a -> Heap a -> Heap a
insert a (Heap tree dirs)
  = let zip = goDown dirs (makeZipper tree)
        broken = insertHere a zip
        fixed = swimUp broken
    in Heap (fst $ beach fixed) (nextComplete dirs)

fromList :: Ord a => [a] -> Heap a
fromList = foldl (flip insert) emptyHeap

toList :: Ord a => Heap a -> [a]
toList heap = case peek heap of
                   Nothing -> []
                   Just top -> top : toList (delete heap)

delete :: (Ord a) => Heap a -> Heap a
delete heap@(Heap Nil _) = heap
delete (Heap (Tree _ Nil Nil) _) = emptyHeap
delete (Heap tree dirs)
  = let zip = makeZipper tree
        prev = prevComplete dirs
        (Tree leaf Nil Nil, crumbs) = goDown prev zip
        (delLeaf,_) = beach (Nil, crumbs)
        (replMax,_) = setHere leaf (makeZipper delLeaf)
        fixed = swimDown (replMax,[])
        (fixedTree,_) = beach fixed
    in Heap fixedTree prev

swimDown :: (Ord a) => Zipper a -> Zipper a
swimDown zip@(Tree _ Nil Nil, _) = zip
swimDown zip@(Tree p (Tree c Nil Nil) Nil, crs)
  | p < c = (Tree c (Tree p Nil Nil) Nil, crs)
  | otherwise = zip
swimDown zip@(Tree p l@(Tree ln ll lr) r@(Tree rn rl rr), crs)
  | p >= ln && p >= rn = zip
  | ln > p && ln >= rn =
      swimDown (Tree p ll lr, LeftCrumb ln r : crs)
  | rn > p && rn > ln =
    swimDown (Tree p rl rr, RightCrumb l rn : crs)
  | otherwise = error "nonconformant instance of Ord. crashed in swimDown"

swimUp :: (Ord a) => Zipper a -> Zipper a
swimUp zip@(_, []) = zip
swimUp zip@(Tree a lC rC, LeftCrumb p r : crs)
  | a > p = swimUp (Tree a (Tree p lC rC) r, crs)
  | otherwise = zip
swimUp zip@(Tree a lC rC, RightCrumb l p : crs)
  | a > p = swimUp (Tree a l (Tree p lC rC), crs)
  | otherwise = zip

data MaxEntry p a = MaxEntry p a
  deriving(Show)

instance Eq p => Eq (MaxEntry p a) where
  MaxEntry p _ == MaxEntry p' _ = p == p'

instance Ord p => Ord (MaxEntry p a) where
  MaxEntry p _ `compare` MaxEntry p' _ = p `compare` p'

data MinEntry p a = MinEntry p a
  deriving(Show)

instance Eq p => Eq (MinEntry p a) where
  MinEntry p _ == MinEntry p' _ = p == p'

instance Ord p => Ord (MinEntry p a) where
  MinEntry p _ `compare` MinEntry p' _ = p' `compare` p

dispHeap :: (Show a) => Heap a -> String
dispHeap (Heap tree dir) = show dir ++ "\n" ++ dispTree tree

bigHeap = foldr insert emptyHeap [78,35,78,35,7,7,7,2,14,23,64,7,5,4,6,456]

