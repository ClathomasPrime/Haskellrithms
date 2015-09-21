module Lookup
  ( lookupBy
  , lookup
  ) where

import Prelude hiding (lookup)

import Tree

lookupBy :: (a -> Ordering) -> Tree a -> Maybe a
lookupBy _ Nil = Nothing
lookupBy ord (Two l n r)
  | ord n == LT = lookupBy ord l
  | ord n == GT = lookupBy ord r
  | ord n == EQ = Just n
  | otherwise = error "incosistent logic (or ordering function). lookupBy, Two instance"
lookupBy ord (Three t1 a t2 b t3)
  | ord a == LT && ord b == LT = lookupBy ord t1
  | ord a == EQ && ord b == LT = Just a
  | ord a == GT && ord b == LT = lookupBy ord t2
  | ord a == GT && ord b == EQ = Just b
  | ord a == GT && ord b == GT = lookupBy ord t3
  | otherwise = error "incosistent tree (or ordering function). lookupBy, Three instance"

lookup :: (Ord a) => a -> Tree a -> Maybe a
lookup a = lookupBy (compare a)
