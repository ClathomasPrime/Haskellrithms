{-# LANGUAGE TupleSections
  #-}
module Partition
  (
  ) where

import qualified Data.IntMap as Map

type Size = Int
type Label = Int
data Pointer = Size Size | Next Label

type Partition = Map.IntMap Pointer

partition :: [Int] -> Partition
partition keys = Map.fromList . map (\i -> (i,Size 1)) $ keys

find :: Label -> Partition -> (Label,Size)
find e set = case Map.lookup e set of
                  Nothing -> error "cry"
                  Just (Size i) -> e
                  Just (Just i) -> find i set

findRep :: Label -> Partition -> Label
findRep e set = fst . find e $ set

findSize :: Label -> Partition -> Size
findSize e set = snd . find e $ set

buddies :: Label -> Label -> Partition -> Bool
buddies a b set = findRep a set == findRep b set

union :: Label -> Label -> Partition -> Partition
union a b set = let (aRep, na) = find a set
                    (bRep, nb) = find b set
                    aToMain = Map.insert aRep (Size $ na + nb)
                    bToA = Map.insert bRep (Next aRep)
                    bToMain = Map.insert bRep (Size $ na + nb)
                    aToB = Map.insert aRep (Next bRep)
                 in if na >= nb
                       then aToMain . bToA $ set
                       else bToMain . aToB $ set


boring :: Partition
boring = partition [1..10]
