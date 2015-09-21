module Connected
  ( connected
  , connectedParts
  ) where

import qualified Data.HashMap as Hash
import Graph

--Fails to be meaningful for diGraphs
connected :: Graph a -> Bool
connected graph = Hash.null . Hash.filter isVisited $ exFromStart
  where start = head . Hash.keys $ graph
        exFromStart = explore (markUnvisited graph) start

isVisited :: ((Bool,a), [Vertex]) -> Bool
isVisited ((b,_), _) = b

markUnvisited :: Graph a -> Graph (Bool,a)
markUnvisited graph = Hash.map (\(a,es) -> ((False,a),es)) graph

unmark :: Graph (a,b) -> Graph b
unmark graph = Hash.map (\( (a,b), es) -> (b,es)) graph

explore :: Graph (Bool,a) -> Vertex -> Graph (Bool,a)
explore graph current
  = let graph' = mapTag graph current (\(_,a) -> (True,a))
        next = filter (not . fst . getTag graph) . neighbors graph $ current
     in case next of
             [] -> graph'
             (v:vs) -> let graph'' = explore graph' v
                        in explore graph'' current

--Breaks on digraphs
connectedParts :: Graph a -> [Graph a]
connectedParts graph = fmap unmark . connectedParts' . markUnvisited $ graph

connectedParts' :: Graph (Bool,a) -> [Graph (Bool,a)]
connectedParts' graph
  = case vertices graph of
         [] -> []
         vs@(start:_) ->
           let explored = explore graph start
               connectedNow = Hash.filter isVisited explored
               connectedRest = connectedParts' $ Hash.filter (not . isVisited) explored
            in connectedNow : connectedRest


