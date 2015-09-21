# Haskellrithms
Some Haskell implementations of CS251 data structures and algorithms

# Structure 

## Trees

* [2-3 trees](2-3) with a pattern-matching implementation of backward-balancing 2-3 trees
* [red-black trees](redBlack) with forward balancing
* [binary heap](heap) with a complete binary tree implementation that keeps track of the paths to the next open leaf
* [trie](trie/Trie.hs) with a list-of-subtrees implementation. Uses prefix tries as name-value maps. Uses suffix tries to search for substrings.

##Graphs

Most graphs are node-labeled edge lists.
Specifically, they are hash maps with keys in any hashable type and values consisting of node lables and a list of vertices that the edge connects to.

* [shortest path](graphs/shortestPath.hs) algorithms using breadth-first search
* [connected components](graphs/Connected.hs) finder. Outputs a partition of the graph, with all elements connected.
* [Kruskall's algorithm](graphs/Kruskall) for finding minimal spanning trees of a connected graph.
    * [Edge labeled graphs](graphs/Kruskall/Graph.hs) implemented using intMaps.
    * [Union-Find](graphs/Kruskall/Partition.hs) algorithm using the "pointer to representative node" model. Uses path compression.
        * The [state monad](graphs/Kruskall/State.hs) is used to keep track of the minimal spanning tree as the algorithm runs.
          This was before I understood monad transformers, so I was scared of library implementations of the state monad.
    * The [algorithm itself](graphs/Kruskall/Kruskall.hs) simply adds more and more edges to the minimal-spanning tree until all vertices are captured.
