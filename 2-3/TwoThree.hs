module TwoThree
  ( T.Tree(Nil)
  , I.insert
  , L.lookup
  , L.lookupBy
  ) where

import Claylude

import Tree as T
import Insert as I
import Lookup as L

--Test code
bigTree = Nil -$ insert 12 -. insert 30 -. insert 23
              -. insert 18 -. insert 7  -. insert 14
              -. insert 29 -. insert 24 -. insert 7
              -. insert 18 -. insert 8  -. insert 10
              -. insert 16 -. insert 7  -. insert 19
              -. insert 24 -. insert 2  -. insert 3
              -. insert 3  -. insert 27
