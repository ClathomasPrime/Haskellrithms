module Tree
  ( Tree(..)
  , Dir(..)
  , Zipper
  , showIndented
  , printTree
  , printMTree
  , zipperfy
  , getRootValue
  , setHere
  , deleteHere
  , getPath
  , walkPath
  , goDown
  , goLeft
  , goFullyLeft
  , goRight
  , goFullyRight
  , goUp
  , goFullyUp
  ) where

import Control.Monad

data Tree a
  = Nil
  | Tree a (Tree a) (Tree a)
  deriving(Show,Eq)

showIndented :: (Show a) => Tree a -> String
showIndented tree = si 0 tree
  where pad i = replicate (2*i) ' '
        si i Nil = pad i ++ "Nil\n"
        si i (Tree a Nil Nil) = pad i ++ show a ++ " Nil Nil\n"
        si i (Tree a l r) =
          pad i ++ show a ++ "\n"
          ++ si (i+1) l
          ++ si (i+1) r

printTree :: (Show a) => Tree a -> IO ()
printTree = putStrLn . showIndented

printMTree :: (Show a) => Maybe (Tree a) -> IO ()
printMTree (Just x) = printTree x
printMTree Nothing = putStrLn "Failed computation"

data Dir = L | R
  deriving(Show,Eq)
type Zipper a = ( Tree a, [ (Dir,a,Tree a) ] )

zipperfy :: Tree a -> Zipper a
zipperfy t = (t,[])

getRootValue :: Tree a -> Maybe a
getRootValue (Tree a _ _) = Just a
getRootValue Nil = Nothing

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Tree a l r) =
    Tree (f a) (fmap f l) (fmap f r)

setHere :: a -> Zipper a -> Zipper a
setHere n (Nil,crs) = (Tree n Nil Nil, crs)
setHere n (Tree o l r, crs) = (Tree n l r, crs)

deleteHere :: Zipper a -> (Maybe a, Zipper a)
deleteHere (Nil, crs) = (Nothing, (Nil,crs))
deleteHere (Tree a _ _, crs) = (Just a, (Nil, crs))

goDown :: [Dir] -> Zipper a -> Maybe (Zipper a)
goDown (L:ds) t = goDown ds =<< goLeft t
goDown (R:ds) t = goDown ds =<< goRight t
goDown [] t = Just t

walkPath :: [Dir] -> Tree a -> Maybe (Zipper a)
walkPath ds t = goDown ds $ zipperfy t

getPath :: Zipper a -> ([Dir], Tree a)
getPath zip = getPath' [] zip
  where getPath' ds (t,[]) = (ds,t)
        getPath' ds (l, (L,p,r):crs) =
          getPath' (L:ds) (Tree p l r, crs)
        getPath' ds (r, (R,p,l):crs) =
          getPath' (R:ds) (Tree p l r, crs)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Nil,_) = Nothing
goLeft (Tree a l r, crumbs) = Just $ (l, (L,a,r):crumbs)

goFullyLeft :: Zipper a -> Zipper a
goFullyLeft z =
  case goLeft z of
       Nothing -> z
       Just zip -> goFullyLeft zip

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Nil,_) = Nothing
goRight (Tree a l r, crumbs) = Just $ (r , (R,a,l):crumbs)

goFullyRight :: Zipper a -> Zipper a
goFullyRight z =
  case goRight z of
       Nothing -> z
       Just zip -> goFullyRight zip

goUp :: Zipper a -> Maybe (Zipper a)
goUp (c, (L,p,r):crumbs) = Just (Tree p c r, crumbs)
goUp (c, (R,p,l):crumbs) = Just (Tree p l c, crumbs)
goUp (_, []) = Nothing

goFullyUp :: Zipper a -> Zipper a
goFullyUp z =
  case goUp z of
       Nothing -> z
       Just zip -> goFullyUp zip

bigTree =
  Tree 20
    (Tree 10
      (Tree 2 Nil Nil)
      (Tree 9
        (Tree 4
          (Tree 1 Nil Nil)
          Nil)
        Nil) )
      (Tree 15 Nil Nil)
