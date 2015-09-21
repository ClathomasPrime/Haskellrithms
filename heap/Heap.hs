module Heap
  (
  ) where

import State
import Tree

data Heap a = Heap { heap :: Tree a
                   , openPath :: [Dir] }

--insert :: State (Heap a) ()
--insert = State insert'
--  where insert' (Heap t p) = ((), Heap (insertWith p t) (nextOpen p))

delete :: Ord a => State (Heap a) a
delete = State $ \(Heap tree path) ->
    let path' = prevOpen path
        (top, Zipper tree' []) = run delete' (Zipper tree [])
        delete' :: State (Zipper a) a
        delete' = do perform $ followDown path
                     leaf <- deleteLeaf
                     perform surface
                     insertHere leaf
                     --swimDown
                     return leaf
     in (top, Heap tree' path')

deleteLeaf :: State (Zipper a) a
deleteLeaf = State $ \(Zipper (Tree a Nil Nil) path) -> (a, Zipper Nil path)

insertHere :: a -> State (Zipper a) ()
insertHere a = State $ \(Zipper t ds)
  -> case t of
          Nil -> ((), Zipper (Tree a Nil Nil) ds)
          Tree _ l r -> ((), Zipper (Tree a l r) ds)

swimDown :: Ord a => State (Zipper a) ()
swimDown


nextOpen :: [Dir] -> [Dir]
nextOpen [] = [L]
nextOpen (L:ds) = R:ds
nextOpen (R:ds) = L:nextOpen ds

prevOpen :: [Dir] -> [Dir]
prevOpen [L] = []
prevOpen (R:ds) = L:ds
prevOpen (L:ds) = R:prevOpen ds

