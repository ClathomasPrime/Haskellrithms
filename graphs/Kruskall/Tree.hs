{-# LANGUAGE TupleSections #-}
module Tree
  ( Tree (..)
  , Crumb (..)
  , Dir (..)
  , Zipper
  , makeZipper
  , goDown
  , beach
  , insertHere
  , setHere
  , nextComplete
  , prevComplete
  , dispTree
  ) where

data Tree a = Nil
            | Tree a (Tree a) (Tree a)
            deriving(Show)

data Crumb a = LeftCrumb a (Tree a)
             | RightCrumb (Tree a) a
             deriving(Show)

data Dir = L | R
  deriving(Show)

type Zipper a = (Tree a, [Crumb a])

makeZipper :: Tree a -> Zipper a
makeZipper = (,[])

goDown :: [Dir] -> Zipper a -> Zipper a
goDown [] zip = zip
--goDown _ zip@(Nil,crs) = zip --Let it crash (it's an ADT)
goDown (L:ds) (Tree a lft rgt,crs)
  = goDown ds (lft, LeftCrumb a rgt : crs)
goDown (R:ds) (Tree a lft rgt,crs)
  = goDown ds (rgt, RightCrumb lft a : crs)

beach :: Zipper a -> (Tree a, [Dir])
beach (t,[]) = (t,[])
beach (lft, LeftCrumb a rgt : crs)
  = (++ [L]) `fmap` beach (Tree a lft rgt, crs)
beach (rgt, RightCrumb lft a : crs)
  = (++ [R]) `fmap` beach (Tree a lft rgt, crs)

insertHere :: a -> Zipper a -> Zipper a
insertHere a (Nil, crs) = (Tree a Nil Nil, crs)

setHere :: a -> Zipper a -> Zipper a
--setHere a (Nil,crs) = (Tree a Nil Nil, crs)
setHere a (Tree n l r, crs) = (Tree a l r, crs)

nextComplete :: [Dir] -> [Dir]
nextComplete = reverse . next . reverse
  where next [] = [L]
        next (L:ds) = R:ds
        next (R:ds) = L:next ds

prevComplete :: [Dir] -> [Dir]
prevComplete = reverse . prev . reverse
  where prev [] = [] --consistent with empty heap
        prev [L] = []
        prev (R:ds) = L:ds
        prev (L:ds) = R:prev ds

dispTree :: (Show a) => Tree a -> String
dispTree t = dispWith 0 t
  where dispWith i Nil
          = replicate (2*i) ' ' ++ "Nil\n"
        dispWith i (Tree a Nil Nil)
          = replicate (2*i) ' ' ++ show a ++ "\n"
        dispWith i (Tree a l r)
          = replicate (2*i) ' ' ++ show a ++ "\n"
                ++ dispWith (i+1) l
                ++ dispWith (i+1) r

bigTree = Tree 4
            (Tree 8
              Nil
              (Tree 3
                (Tree 2 Nil Nil)
                (Tree 8 Nil Nil) ) )
            (Tree 10
              (Tree 8
                (Tree 3 Nil Nil)
                (Tree 7 Nil Nil) )
              (Tree 5
                (Tree 2 Nil Nil)
                (Tree 1 Nil Nil) ) )

