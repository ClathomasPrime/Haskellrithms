
data Tree a = Nil
            | Tree a (Tree a) (Tree a)

insert :: (Ord a) => a -> Tree a -> Tree a
insert k Nil = Tree k Nil Nil
insert k (Tree n l r)
  | k < n = insert k l
  | k > n = insert k r
  | otherwise = Tree k l r

search :: (Ord a) => a -> Tree a -> Maybe a
search k (Tree n l r)
  | k < a = search k l
  | k > a = search k r
  | otherwise = a

floor :: (Ord a) => a -> Tree a -> Maybe a
floor
