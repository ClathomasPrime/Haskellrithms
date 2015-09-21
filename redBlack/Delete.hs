module Delete
  (
  ) where

import Tree

deleteBy :: (a -> Ordering) -> Tree a -> (Maybe a, Tree a)
deleteBy ord t = (Nothing, Nil)

delete :: (Ord a) =>

--delete and return successor
snagSuccesor :: Tree a -> (a,Tree a)
snagSuccesor (Tree c l n r) = (a, Tree c l n r')
  where (a,r') = snagLeftmost r
        snagLeftmost (Tree _ Nil n _) = (n,Nil)
        snagLeftmost (Tree c l n r) =
          (\l' -> Tree c l' n r) `fmap` snagLeftmost l

deleteHere :: Tree a -> (
