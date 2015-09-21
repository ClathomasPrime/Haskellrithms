module Insert
  ( insert
  ) where

import Tree


insert :: (Ord a) => a -> Tree a -> Tree a
insert a t = fst . rebuild . restoreUp . insertDown a $ (t,[])


restoreUp :: (Ord a) => Zipper a -> Zipper a

restoreUp z@(Two _ _ _, _) = z
restoreUp z@(Three _ _ _ _ _, _) = z

restoreUp (Four t1 a t2 b t3 c t4,
           [] ) --root
  = (Two (Two t1 a t2) b (Two t3 c t4),  [] )

restoreUp (Four t1 a t2 b t3 c t4, --Four
           (LCrumb d t5):crumbs )  --under two
  = (Three (Two t1 a t2) b (Two t3 c t4) d t5,  crumbs )
restoreUp (Four t2 b t3 c t4 d t5, --Four
           (RCrumb t1 a):crumbs )  --under two
  = (Three t1 a (Two t2 b t3) c (Two t4 d t5),  crumbs )

restoreUp (Four t1 a t2 b t3 c t4,       --Four
           (OneCrumb d t5 e t6):crumbs ) --under three
  = restoreUp $
    (Four (Two t1 a t2) b (Two t3 c t4) d t5 e t6,  crumbs)
restoreUp (Four t2 b t3 c t4 d t5,       --Four
           (TwoCrumb t1 a e t6):crumbs ) --under three
  = restoreUp $
    (Four t1 a (Two t2 b t3) c (Two t4 d t5) e t6,  crumbs)
restoreUp (Four t3 c t4 d t5 e t6,         --Four
           (ThreeCrumb t1 a t2 b):crumbs ) --under three
  = restoreUp $
    (Four t1 a t2 b (Two t3 c t4) d (Two t5 e t6),  crumbs)


insertDown :: (Ord a) => a -> Zipper a -> Zipper a

insertDown a (Nil, []) = (Two Nil a Nil, []) --empty tree

insertDown a (Two t1 node t2, crumbs)
  | a < node = insertDown a $
               (t1, (LCrumb node t2):crumbs)
  | a > node = insertDown a $
               (t2, (RCrumb t1 node):crumbs)
  | a == node = (Two t1 a t2, crumbs)
  | otherwise = error $ "invalid instance of Ord. "
        ++ "Killed in insertDown, Two spec"

insertDown a (Three t1 v1 t2 v2 t3, crumbs)
  | a < v1 && a < v2 = insertDown a $
                      (t1, (OneCrumb v1 t2 v2 t3):crumbs)
  | v1 < a && a < v2 = insertDown a $
                      (t2, (TwoCrumb t1 v1 v2 t3):crumbs)
  | v1 < a && v2 < a = insertDown a $
                      (t3, (ThreeCrumb t1 v1 t2 v2):crumbs)
  | a == v1 = (Three t1 a t2 v2 t3, crumbs)
  | a == v2 = (Three t1 v1 t2 a t3, crumbs)
  | otherwise = error $ "invalid instance of Ord or faulty tree "
        ++ "construction. Killed in insertDown, Three spec"

insertDown a (Nil, (LCrumb node Nil):crumbs)
  | a < node = (Three Nil a Nil node Nil, crumbs)
  | otherwise = error $ "inconsitent internal logic in insertDown. "
        ++ "Killed in LCrumb spec"
insertDown a (Nil, (RCrumb Nil node):crumbs)
  | a > node = (Three Nil node Nil a Nil, crumbs)
  | otherwise = error $ "inconsitent internal logic in insertDown. "
        ++ "Killed in RCrumb spec"

insertDown a (Nil, (OneCrumb b Nil c Nil):crumbs)
  | a < b && a < c =
        restoreUp (Four Nil a Nil b Nil c Nil, crumbs)
  | otherwise = error $ "inconsitent internal logic in insertDown. "
        ++ "Killed in OneCrumb spec"
insertDown b (Nil, (TwoCrumb Nil a c Nil):crumbs)
  | a < b && a < c =
        restoreUp (Four Nil a Nil b Nil c Nil, crumbs)
  | otherwise = error $ "inconsitent internal logic in insertDown. "
        ++ "Killed in TwoCrumb spec"
insertDown c (Nil, (ThreeCrumb Nil a Nil b):crumbs)
  | a < b && b < c =
        restoreUp (Four Nil a Nil b Nil c Nil, crumbs)
  | otherwise = error $ "inconsitent internal logic in insertDown. Killed in ThreeCrumb spec"

insertDown _ _ = error $ "non-exaustive pat match in insertDown."
      ++ "This tree was probably incorrectly constructed "
      ++ "(or there may be some internal error"


