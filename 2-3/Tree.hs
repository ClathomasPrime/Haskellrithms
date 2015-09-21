module Tree
  ( Tree(..)
  , Crumb(..)
  , Zipper
  , rebuild
  , flatten
  ) where

data Tree a = Nil
            | Two (Tree a) a (Tree a)
            | Three (Tree a) a (Tree a) a (Tree a)
            | Four (Tree a) a (Tree a) a (Tree a) a (Tree a) --transient value
            deriving(Show)

type Zipper a = ( Tree a, [ Crumb a ] )

data Crumb a = LCrumb a (Tree a)
             | RCrumb (Tree a) a
             | OneCrumb a (Tree a) a (Tree a)
             | TwoCrumb (Tree a) a a (Tree a)
             | ThreeCrumb (Tree a) a (Tree a) a

data Dir = DirLeft | DirRight | DirOne | DirTwo | DirThree


rebuild :: Zipper a -> (Tree a, [Dir])
rebuild zip = rebuild' [] zip
  where rebuild' ds (t1, [])
          = (t1, ds)

        rebuild' ds (t1, (LCrumb a t2):crumbs)
          = rebuild' (DirLeft:ds) (Two t1 a t2, crumbs)
        rebuild' ds (t2, (RCrumb t1 a):crumbs)
          = rebuild' (DirRight:ds) (Two t1 a t2, crumbs)

        rebuild' ds (t1, (OneCrumb a t2 b t3):crumbs)
          = rebuild' (DirOne:ds) (Three t1 a t2 b t3, crumbs)
        rebuild' ds (t2, (TwoCrumb t1 a b t3):crumbs)
          = rebuild' (DirTwo:ds) (Three t1 a t2 b t3, crumbs)
        rebuild' ds (t3, (ThreeCrumb t1 a t2 b):crumbs)
          = rebuild' (DirThree:ds) (Three t1 a t2 b t3, crumbs)


flatten :: Tree a -> [a]
flatten Nil = []
flatten (Two t1 a t2) = flatten t1 ++ [a] ++ flatten t2
flatten (Three t1 a t2 b t3)
  = flatten t1 ++ [a] ++ flatten t2 ++ [b] ++ flatten t3
