{-# LANGUAGE TupleSections #-}
module Tree
  ( Tree(..), Color(..), Dir(..), Crumb(..), Zipper, makeZipper
  , stepUpOne, stepUpBy, stepUpFully, walkDown, rebuild
  , colorRoot, getRootColor
  ) where

import Data.Maybe

data Tree a = Nil
            | Tree Color (Tree a) a (Tree a)
            deriving(Show)

data Color = Red | Black
  deriving(Eq, Show)

data Dir = L | R
  deriving(Eq,Show)

data Crumb a = Crumb Dir Color a (Tree a)

type Zipper a = (Tree a, [Crumb a])

makeZipper :: Tree a -> Zipper a
makeZipper = (,[])

stepUpOne :: Zipper a -> (Zipper a, Maybe Dir)
stepUpOne zip@(_,[]) = (zip,Nothing)
stepUpOne (l, Crumb L c p r:crs)
  = ((Tree c l p r, crs), Just L)
stepUpOne (r, Crumb R c p l:crs)
  = ((Tree c l p r, crs), Just R)

stepUpBy :: Int -> Zipper a -> (Zipper a, [Dir])
stepUpBy 0 zip = (zip,[])
stepUpBy n zip = (++maybeToList dir) `fmap` stepUpBy (n-1) upzip
  where (upzip,dir) = stepUpOne zip

stepUpFully :: Zipper a -> (Zipper a, [Dir])
stepUpFully zip@(_,[]) = (zip,[])
stepUpFully zip = (++maybeToList dir) `fmap` stepUpFully upzip
  where (upzip,dir) = stepUpOne zip

rebuild :: Zipper a -> Tree a
rebuild = fst . fst . stepUpFully

walkDown :: [Dir] -> Zipper a -> Zipper a
walkDown [] zip = zip
walkDown _ zip@(Nil,_) = zip
walkDown (L:dirs) (Tree c l p r,crs)
  = walkDown dirs (l, Crumb L c p r:crs)
walkDown (R:dirs) (Tree c l p r,crs)
  = walkDown dirs (r, Crumb R c p l:crs)

colorRoot :: Color -> Tree a -> Tree a
colorRoot _ Nil = Nil
colorRoot c (Tree _ l p r) = Tree c l p r

getRootColor :: Tree a -> Maybe Color
getRootColor Nil = Nothing
getRootColor (Tree c _ _ _) = Just c



