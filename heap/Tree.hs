module Tree
  ( Tree(..)
  , Dir(..)
  , Zipper(..)
  , isRoot      -- Zipper a -> Bool
  , isLeaf      -- Zipper a -> Bool
  , goUp        -- Zipper a -> Zipper a
  , surface     -- Zipper a -> Zipper a
  , goLeft      -- Zipper a -> Zipper a
  , goRight     -- Zipper a -> Zipper a
  , goDown      -- Dir -> Zipper a -> Zipper a
  , followDown  -- [Dir] -> a -> Zipper a  --GOES FROM BACK TO FRONT
  ) where

data Tree a = Nil
            | Tree a (Tree a) (Tree a)
data Dir = L | R
data Zipper a = Zipper { descendants :: Tree a
                       , ancestors :: [(Dir,a,Tree a)] }

isRoot :: Zipper a -> Bool
isRoot (Zipper _ []) = True
isRoot _ = False

isLeaf :: Zipper a -> Bool
isLeaf (Zipper (Tree _ Nil Nil) _) = True
isLeaf _ = False

goUp :: Zipper a -> Zipper a
goUp z@(Zipper _ []) = z
goUp (Zipper t (up:path)) = case up of
                                 (L,p,r) -> Zipper (Tree p t r) path
                                 (R,p,l) -> Zipper (Tree p l t) path

surface :: Zipper a -> Zipper a
surface z | isRoot z = z
          | otherwise = surface . goUp $ z

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper (Tree _ Nil _) _) = z
goLeft (Zipper (Tree p l r) path) = Zipper l $ (L,p,r):path

goRight :: Zipper a -> Zipper a
goRight z@(Zipper (Tree _ _ Nil) _) = z
goRight (Zipper (Tree p l r) path) = Zipper r $ (R,p,l):path

goDown :: Dir -> Zipper a -> Zipper a
goDown L = goLeft
goDown R = goRight

followDown :: [Dir] -> Zipper a -> Zipper a
followDown ds zip = foldr goDown zip ds



