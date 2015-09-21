module Test
  (
  ) where

import qualified Data.HashMap as Hash

import Graph
import ShortestPath
import Connected

tg1 :: Graph ()
tg1 = let ins (k,v) = Hash.insert k v
          tag (i,as) = (i,((),as))
          tagged = fmap tag [ (0, [1,3])
                            , (1, [2,3])
                            , (2, [3])
                            , (3, []) ]
       in foldr ins Hash.empty  tagged

tg2 :: Graph ()
tg2 = let ins (k,v) = Hash.insert k v
          tag (i,as) = (i,((),as))
          tagged = fmap tag [ (0, [1,2,3])
                            , (1, [0,3])
                            , (2, [0,3])
                            , (3, [0,1]) ]
       in foldr ins Hash.empty  tagged

tg3 :: Graph () --Undirectional
tg3 = let ins (k,v) = Hash.insert k v
          tag (i,as) = (i,((),as))
          tagged = fmap tag [ (0, [1,2,3])
                            , (1, [0,2,6,7])
                            , (2, [0,1,3])
                            , (3, [0,2,4,8])
                            , (4, [3,5,8,9])
                            , (5, [4,7,8])
                            , (6, [1,7])
                            , (7, [1,5,6])
                            , (8, [3,4,5])
                            , (9, [4,8]) ]
       in foldr ins Hash.empty tagged

tg4 :: Graph () --Directed
tg4 = let ins (k,v) = Hash.insert k v
          tag (i,as) = (i,((),as))
          tagged = fmap tag [ (0, [1,2,3,8])
                            , (1, [2])
                            , (2, [0,3])
                            , (3, [4,8])
                            , (4, [3,4,5,8,9])
                            , (5, [5,6,7,8])
                            , (6, [1,7])
                            , (7, [1])
                            , (8, [])
                            , (9, [4,8]) ]
       in foldr ins Hash.empty tagged

tg5 :: Graph () --Undirected
tg5 = let ins (k,v) = Hash.insert k v
          tag (i,as) = (i,((),as))
          tagged = fmap tag [ (0, [1,2])
                            , (1, [1,2])
                            , (2, [0,1])
                            , (3, [4])
                            , (4, [3]) ]
       in foldr ins Hash.empty tagged
