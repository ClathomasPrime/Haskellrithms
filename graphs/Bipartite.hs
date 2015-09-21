module Bipartite
  (
  ) where

import Data.HashMap

data Color = Red | Blue | IDK

paintDefault :: Graph a -> Graph Color
paintDefault graph = Hash.map (\(_,es) -> (IDK,es)) graph

--Only for connected graphs
bipartite :: Graph a -> Bool
bipartite graph = snd . bipartite' . paintDefault $ graph

bipartite' :: Graph Color -> [Vertex] -> (Graph Color, Bool)
bipartite' graph frontier =

