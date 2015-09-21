module Kruskal
  (
  ) where

import qualified Heap as H

import Graph
import Partition
import State

main = print (minimalSpan exciting)

getEdgeHeap :: Ord w => DiGraph w a -> H.Heap (H.MinEntry w (Label,Label))
getEdgeHeap graph = H.fromList . fmap makeEntry . edges $ graph
  where makeEntry (a,b,w) = H.MinEntry w (a,b)

minimalSpan :: Ord w => DiGraph w a -> DiGraph w a
minimalSpan graph = maskVertices graph (kruskal vs es emptyGraph)
  where vs = partition . labels $ graph
        es = getEdgeHeap graph

kruskal :: Ord w
        => Partition
        -> H.Heap (H.MinEntry w (Label,Label))
        -> DiGraph w ()
        -> DiGraph w ()
kruskal vertices edges spanning
  = case H.peek edges of
         Nothing -> spanning
         Just (H.MinEntry w (a,b)) ->
           let edges' = H.delete edges
               (isInSpan, vertices') = run (union a b) vertices
               spanning' = if isInSpan
                              then addEdge a b w spanning
                              else spanning
            in kruskal vertices' edges' spanning'

exciting :: DiGraph Double ()
exciting = construct . nicify $ graph
  where
    nicify = fmap $ \(i,es) -> (i,((),es))
    graph =
      [ (0, [ (0,3.0)
            , (1,7.0) ])
      , (1, [ (2,9.0) ])
      , (2, [ (0,4.0)
            , (0,2.0)
            , (3,1.0) ])
      , (3, [ (4,6.0) ])
      , (4, [ (3,10.0)
            , (1,12.0)
            , (2,5.0) ])
      ]
