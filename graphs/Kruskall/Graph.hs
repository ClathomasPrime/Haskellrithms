module Graph
  ( Label       -- Int
  , DiGraph     -- Map.IntMap (a, [(Label,w)])
  , emptyGraph  -- DiGraph w a
  , construct   -- [(Label, (a, [(Label,w)]))] -> DiGraph w a
  , vertices    -- DiGraph w a -> [a]
  , labels      -- DiGraph w a -> [Label]
  , edges       -- DiGraph w a -> [(Label,Label,w)]
  , addEdge     -- Label -> Label -> w -> DiGraph w () -> DiGraph w ()
  , maskVertices    -- DiGraph w a -> DiGraph w b -> DiGraph w a
  ) where

import qualified Data.IntMap as Map

type Label = Int
--Integer referenced edge weighted directed graph
--Stores `a`s at each node
type DiGraph w a = Map.IntMap (a, [(Label,w)])

emptyGraph :: DiGraph w a
emptyGraph = Map.empty

construct :: [(Label, (a, [(Label,w)]))] -> DiGraph w a
construct = Map.fromList

vertices :: DiGraph w a -> [a]
vertices = fmap fst . Map.elems

valueAt :: Label -> DiGraph w a -> a
valueAt x graph = case Map.lookup x graph of
                       Just (a,_) -> a
                       Nothing -> undefined

labels :: DiGraph w a -> [Label]
labels = Map.keys

edges :: DiGraph w a -> [(Label,Label,w)]
edges graph = Map.toList graph >>= expand
  where
    expand :: (Label,(a,[(Label,w)])) -> [(Label,Label,w)]
    expand (start, (_,edges)) = fmap (putStart start) edges
    putStart :: Label -> (Label, w) -> (Label, Label, w)
    putStart a (b,w) = (a,b,w)

addEdge :: Label -> Label -> w -> DiGraph w () -> DiGraph w ()
addEdge a b w graph = Map.alter f a graph
  where f Nothing = Just ((), [(b,w)])
        f (Just ((),edges)) = Just ((),(b,w):edges)

maskVertices :: DiGraph w a -> DiGraph w b -> DiGraph w a
maskVertices mask structure = Map.mapWithKey f structure
  where f key (_,edges) = (valueAt key mask,edges)



