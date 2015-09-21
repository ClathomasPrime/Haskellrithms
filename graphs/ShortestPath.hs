module ShortestPath
  ( shortestPath
  ) where

import Data.List
import qualified Data.HashMap as Hash
import Graph


shortestPath :: Graph a -> Vertex -> Vertex -> Maybe Path
shortestPath graph start goal = fmap reverse $ shortestPath' tagged [start] goal
  where tagged = Hash.mapWithKey tag graph
        tag k (_,vs) = if k == start
                          then ([k],vs)
                          else ([],vs)

--Calculate shortest path to goal from any element of frontier (in reverse order)
shortestPath' :: Graph Path -> [Vertex] -> Vertex -> Maybe Path
shortestPath' graph frontier goal
  = let (graph', frontier') = expand graph frontier []
     in if frontier' == []
           then Nothing
           else case getTag graph' goal of
                     [] -> shortestPath' graph' frontier' goal
                     path -> Just path

--Take a vertex, expand off of it, update the graph, and return additions to the frontier
expandHere :: Graph Path -> Vertex -> (Graph Path, [Vertex])
expandHere graph vertex =
  let pathV = getTag graph vertex
      newNeighbors = filter (isNew graph) (neighbors graph vertex)
      update gr v = setTag gr v (v:pathV)
   in (foldl update graph newNeighbors, newNeighbors)

--Expand over a full frontier (accumulating the new frontier along the way)
expand :: Graph Path -> [Vertex] -> [Vertex] -> (Graph Path, [Vertex])
expand graph [] frontier = (graph, frontier)
expand graph (vertex:vertices) frontier
  = let (newGraph, newFrontier) = expandHere graph vertex
     in expand newGraph vertices (newFrontier ++ frontier)

isNew :: Graph Path -> Vertex -> Bool
isNew graph v = null (getTag graph v)

