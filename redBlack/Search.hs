module Search
  ( searchFor
  , searchBy
  ) where

import Tree

searchFor :: (Ord a) => a -> Tree a -> Maybe a
searchFor a = searchBy (compare a)

searchBy :: (a -> Ordering) -> Tree a -> Maybe a
searchBy _ Nil = Nothing
searchBy f (Tree _ l n r)
  | f n == LT = searchBy f l
  | f n == GT = searchBy f r
  | f n == EQ = Just n
  | otherwise = error "inconsistent ordering function (crashed in searchBy)"

