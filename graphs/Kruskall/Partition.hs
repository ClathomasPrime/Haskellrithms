{-# LANGUAGE TupleSections
           , RankNTypes
           #-}
module Partition
  ( Size          --Int
  , Label         --Int
  , Pointer(..)   --Size Size | Next Label
  , Partition     --Map.IntMap Pointer
  , partition     --[Int] -> Partition
  , rep           --Label -> State Partition (Label,Size)
  , union         --Label -> Label -> State Partition Bool
  , full          --State Partition Bool
  ) where

import State
import Control.Applicative
import qualified Data.IntMap as Map

import Graph

type Size = Int
data Pointer = Size Size | Next Label deriving(Show)
type Partition = Map.IntMap Pointer

partition :: [Int] -> Partition
partition keys = Map.fromList . fmap (,Size 1) $ keys

--Chases down the representative and compresses nodes along the way
rep :: Label -> State Partition (Label,Size)
rep start = State (chase start)
  where chase i set =
          case Map.lookup i set of
               Nothing -> error "cry"
               Just (Size size) -> ((i,size), set) --leave set unchanged
               Just (Next next) ->
                 let ((rep,size), set') = chase next set
                  in ((rep,size), Map.insert i (Next rep) set')

--Joins the two corresponding partitions if they are disjoint
--Returns true when the sets were disjoint
--If they are not disjoint it just compresses paths
union :: Label -> Label -> State Partition Bool
union aStart bStart =
  do (aRep,aSize) <- rep aStart
     (bRep,bSize) <- rep bStart
     if aRep == bRep
        then return False
        else if aSize <= bSize
             then do let aToB = Map.insert aRep (Next bRep)
                         bToRep = Map.insert bRep (Size $ aSize + bSize)
                     perform $ aToB . bToRep
                     return True
             else do let bToA = Map.insert bRep (Next aRep)
                         aToRep = Map.insert aRep (Size $ aSize + bSize)
                     perform $ bToA . aToRep
                     return True

full :: State Partition Bool
full = State $ \set -> let elem = head . Map.keys $ set
                           ((_,size), set') = run (rep elem) set
                        in (size == Map.size set', set')

boring = partition [1..10]

interesting = exec (sequence $ map (uncurry union) list) boring
  where list = [(1,5),(1,4),(2,10),(3,5),(9,10),(5,9)]


