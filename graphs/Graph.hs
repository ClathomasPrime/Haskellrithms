{-# LANGUAGE TupleSections
           #-}

module Graph
  ( Vertex, DiEdge, Graph, Path
  , neighbors, degree, vertices, diEdges
  , getTag, setTag, mapTag
  ) where

import qualified Data.HashMap as Hash


type Vertex = Int
type DiEdge = (Vertex,Vertex)
--a represents a tag type
type Graph a = Hash.Map Vertex (a, [Vertex])
type Path = [Vertex]


neighbors :: Graph a -> Vertex -> [Vertex]
neighbors gr v = case Hash.lookup v gr of
                      Just (_,vs) -> vs
                      Nothing -> error "YOU CANT MAKE ME WRITE TOTAL FUNCTIONS THIS ISN'T PRODUCTION"

degree :: Graph a -> Vertex -> Int
degree gr v = length $ neighbors gr v

vertices :: Graph a -> [Vertex]
vertices = Hash.keys

diEdges :: Graph a -> [DiEdge]
diEdges gr = let assoc = Hash.assocs gr
                 outEdges (v,(_,vs)) = fmap (v,) vs
              in assoc >>= outEdges

getTag :: Graph a -> Vertex -> a
getTag graph v = case Hash.lookup v graph of
                      Nothing -> undefined
                      Just (tag,vs) -> tag

setTag :: Graph a -> Vertex -> a -> Graph a
setTag graph v a = mapTag graph v (const a)

mapTag :: Graph a -> Vertex -> (a -> a) -> Graph a
mapTag graph v f = Hash.adjust f' v graph
  where f' (a,es) = (f a,es)
