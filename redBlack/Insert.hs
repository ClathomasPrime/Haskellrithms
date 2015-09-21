module Insert
  ( insert
  , bigTree
  ) where

import Control.Monad

import Claylude
import Tree

--Forward balancing inserts

insert :: (Ord a) => a -> Tree a -> Tree a
insert a t = rebuild . forward a . makeZipper $ t

forward :: (Ord a) => a -> Zipper a -> Zipper a
forward a zip@(t,crs) =
  if isFour t
     then insert' a . correct $ (flipColor t,crs)
     else insert' a zip

insert' :: (Ord a) => a -> Zipper a -> Zipper a
insert' a (Tree c l n r, crs)
  | a < n = forward a (l, Crumb L c n r:crs)
  | a > n = forward a (r, Crumb R c n l:crs)
  | a == n = (Tree c l a r, crs)
insert' a (Nil, crs) = correct (Tree Red Nil a Nil, crs)


correct :: Zipper a -> Zipper a
correct (Tree Red l n r,[]) = (Tree Black l n r,[])
correct zip@(_,[]) = zip

correct zip@(_,Crumb _ Black _ _:_) = zip

correct (t, [Crumb d Red p s]) = (t, [Crumb d Black p s])

correct (Tree Red t3 c t4,
            Crumb R Red b t2
              :Crumb R Black a t1:crs)
  = (Tree Black (Tree Red t1 a t2) b (Tree Red t3 c t4), crs)
correct (Tree Red t2 b t3,
            Crumb L Red c t4
              :Crumb R Black a t1:crs)
  = (Tree Black (Tree Red t1 a t2) b (Tree Red t3 c t4), crs)
correct (Tree Red t2 b t3,
            Crumb R Red a t1
              :Crumb L Black c t4:crs)
  = (Tree Black (Tree Red t1 a t2) b (Tree Red t3 c t4), crs)
correct (Tree Red t1 a t2,
            Crumb L Red b t3
              :Crumb L Black c t4:crs)
  = (Tree Black (Tree Red t1 a t2) b (Tree Red t3 c t4), crs)


--colors oposite of a four node
flipColor :: Tree a -> Tree a
flipColor (Tree c l n r)
  = Tree Red (colorRoot Black l) n (colorRoot Black r)


isFour :: Tree a -> Bool
isFour (Tree Black l n r) =
  case liftM2 (bh (==Red) (&&) (==Red))
              (getRootColor l) (getRootColor r) of
       Just True -> True
       _ -> False
isFour _ = False


bigTree :: Tree Int
bigTree = foldl (flip insert) Nil [9,54,2,1,49,24,4,19,18,21,6,12]

goodTree = foldl (flip insert) Nil [4,6,3,8,14,1]

