


import Data.Maybe
import Control.Monad

data Trie a b = Trie { value :: Maybe a
                     , children :: [(b,Trie a b)]
                     } deriving(Show)

empty = Trie Nothing []


--
--Regular Tries
--

getTrie :: Eq b => [b] -> Trie a b -> Maybe (Trie a b)
getTrie [] t = Just t
getTrie (b:bs) t = case lookup b $ children t of
                        Just t' -> getTrie bs t'
                        Nothing -> Nothing

get :: Eq b => [b] -> Trie a b -> Maybe a
get bs t = case getTrie bs t of
                Just trie -> value trie
                Nothing -> Nothing

over :: Eq b => [b] -> (Maybe a -> a) -> Trie a b -> Trie a b
over [] f (Trie a xs) = Trie (Just $ f a) xs
over (b:bs) f (Trie a xs) = Trie a $ doOver b tmap xs
  where tmap Nothing = over bs f (Trie Nothing [])
        tmap (Just t) = over bs f t

set :: Eq b => [b] -> a -> Trie a b -> Trie a b
set bs a t = over bs (const a) t

insert :: Eq b => [b] -> a -> Trie [a] b -> Trie [a] b
insert bs a t = over bs amap t
  where amap Nothing = [a]
        amap (Just as) = a:as

dickSize = foldr (uncurry set) empty pairs
  where pairs = [ ("clay", 69)
                , ("ryusuke", 104)
                , ("jason", 4)
                , ("elliott", 6)
                , ("connorr", 10)
                , ("clarence",14)
                , ("jasmin", 0)
                , ("ryan", 7)       ]


--
--Suffix tries
--
--This guy defeats the whole purpose of a suffix trie because it doesn't
--store substring information at each node (you have to go down to find it)
buildSuffixBad :: Eq b => [b] -> Trie [Int] b
buildSuffixBad xs = build xs 0
  where build (b:bs) i = insert (b:bs) i $ build bs (i+1)
        build [] _ = empty

findBad :: Eq b => [b] -> Trie [a] b -> [a]
findBad bs trie = join . maybeToList $ fmap children (getTrie bs trie)
  where
    children :: Trie [a] b -> [a]
    children (Trie a xs)
      = join (maybeToList a ++ fmap (\(b,t) -> children t) xs)

doOver :: Eq b => b -> (Maybe a -> a) -> [(b,a)] -> [(b,a)]
doOver b f xs = doOver' False b f xs
  where doOver' p b f ((b',a):xs)
          | b == b'     = (b',f (Just a)) : doOver' True b f xs
          | otherwise  = (b',a) : doOver' p b f xs
        doOver' True b f [] = []
        doOver' False b f [] = [(b,f Nothing)]

data Suffix a b = Suffix { locations :: [a]
                         , suffixes :: [(b,Suffix a b)]
                         }deriving(Show)

buildSuffix :: Eq b => [b] -> Suffix Int b
buildSuffix text = build text 0
  where build (b:bs) i = insertSuffix (b:bs) i $ build bs (i+1)
        build [] _ = Suffix [] []

--Doesn't do redundancy checks (only really suitable for use in buildSuffix
insertSuffix :: Eq b => [b] -> a -> Suffix a b -> Suffix a b
insertSuffix [] _ suf = suf
insertSuffix (b:bs) a (Suffix as xs) = Suffix (a:as) ins
  where ins = doOver b repl xs
        repl Nothing = insertSuffix bs a (Suffix [] [])
        repl (Just suf) = insertSuffix bs a suf

find :: Eq b => [b] -> Suffix a b -> [a]
find [] (Suffix as _) = as
find (b:bs) (Suffix _ xs) = case lookup b xs of
                                 Nothing -> []
                                 Just suf -> find bs suf

phrase = buildSuffix "it was the best of times it was the worst of times$"
--                    012345678901234567890123456789012345678901234567890
--                    0         1         2         3         4         5

