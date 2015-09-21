module CHeap
  ( CHeap()
  , emptyCHeap
  , insertWith
  , insertMinHeap
  , insertMaxHeap
  ) where

import Data.Maybe
import Control.Monad

import Tree

--Note: tree predicats are assumed to be transitive
--(if pred a b, pred b c, then pred a c
data CHeap a = CHeap
  { heapTree :: Tree a
  , pathNext :: [Dir]
  , pathPrevious :: [Dir]
  } deriving(Show)

emptyCHeap = CHeap Nil [] []

pr :: (Show a) => CHeap a -> IO ()
pr = putStrLn . showIndented . heapTree

prM :: (Show a) => Maybe (CHeap a) -> IO ()
prM Nothing = putStrLn "Cry"
prM (Just a) = pr a

incrPath :: [Dir] -> [Dir]
incrPath = reverse . incr . reverse
  where incr [] = [L]
        incr (L:ds) = R:ds
        incr (R:ds) = L:incr ds

decrPath :: [Dir] -> [Dir]
decrPath = reverse . decr . reverse
  where decr [] = []
        decr [L] = []
        decr (R:ds) = L:ds
        decr (L:ds) = R:decr ds

insertIntoBottom :: a -> CHeap a -> Maybe (Zipper a)
insertIntoBottom a heap =
  let zip = walkPath (pathNext heap) (heapTree heap)
  in setHere a `liftM` zip

--The predicate "goes down" the tree (e.g. (>=) for a max heap)
restoreHeapUp :: (a -> a -> Bool) -> Zipper a -> Zipper a
restoreHeapUp _ z@(_,[]) = z
restoreHeapUp _ (Nil, crs) = (Nil, crs)
restoreHeapUp pred z@(Tree lChild l' r', (L,parent,r):crs) =
  if pred parent lChild
     then z
     else restoreHeapUp pred (Tree lChild (Tree parent l' r') r, crs)
restoreHeapUp pred z@(Tree rChild l' r', (R,parent,l):crs) =
  if pred parent rChild
     then z
     else restoreHeapUp pred (Tree rChild l (Tree parent l' r'), crs)

insertWith :: (a -> a -> Bool) -> a -> CHeap a -> Maybe (CHeap a)
insertWith pred a heap@(CHeap tr pNext pPrev)  =
  do zip <- insertIntoBottom a heap
     let rest = restoreHeapUp pred zip
         (_, nextTree) = getPath rest
     return $ CHeap nextTree (incrPath pNext) pNext --TO BE FIXED

restoreHeapDown :: (a -> a -> Bool) -> Zipper a -> Zipper a
restoreHeapDown _ zip@(Nil,_) = zip
restoreHeapDown _ zip@(Tree _ Nil Nil,_) = zip
restoreHeapDown pred zip@(Tree p (Tree c l r) Nil, crs) =
  if pred p c
     then zip
     else restoreHeapDown pred $ (Tree p l r, (L,c,Nil):crs)
restoreHeapDown pred zip@(Tree p Nil (Tree c l r), crs) =
  if pred p c
     then zip
     else restoreHeapDown pred $ (Tree p l r, (R,c,Nil):crs)
--Note: tries left subtree first -> Bias (eg max heap smaller on left)
restoreHeapDown pred zip@(Tree p l@(Tree cL lL rL) r@(Tree cR lR rR), crs) =
  if pred p cL
     then if pred p cR
             then zip
             else restoreHeapDown pred $ (Tree p lR rR, (R,cR,l):crs)
     else restoreHeapDown pred $ (Tree p lL rL, (L,cL,r):crs)

deleteRootWith :: (a -> a -> Bool) -> CHeap a -> Maybe (a, CHeap a)
deleteRootWith pred heap@(CHeap tr pNext pLast) =
  do zipDown <- walkPath pLast tr
     let (ml, delZip) = deleteHere zipDown
         mRoot = getRootValue tr
         delTree = goFullyUp delZip
     root <- mRoot
     leaf <- ml
     let replRoot = setHere leaf delTree
         restZip = restoreHeapDown pred replRoot
         (rest,_) = goFullyUp restZip
     return $ (root, CHeap rest pLast (decrPath pLast) )

insertMaxHeap :: (Ord a) => a -> CHeap a -> Maybe (CHeap a)
insertMaxHeap = insertWith (>=)

insertMinHeap :: (Ord a) => a -> CHeap a -> Maybe (CHeap a)
insertMinHeap = insertWith (<=)

deleteMaxHeap :: (Ord a) => CHeap a -> Maybe (a,CHeap a)
deleteMaxHeap = deleteRootWith (>=)

deleteMinHeap :: (Ord a) => CHeap a -> Maybe (a,CHeap a)
deleteMinHeap = deleteRootWith (<=)

bigCHeap = CHeap tree path last
  where tree =
          (Tree 20
            (Tree 15
              (Tree 13
                (Tree 7 Nil Nil)
                (Tree 4 Nil Nil) )
              (Tree 5
                (Tree 2 Nil Nil)
                (Tree 3 Nil Nil) ) )
            (Tree 11
              (Tree 10
                (Tree 1 Nil Nil)
                Nil)
              (Tree 2 Nil Nil) ) )
        path = [R,L,R]
        last = [R,L,L]

heapLeaf = walkPath (pathNext bigCHeap) (heapTree bigCHeap)

