module Claylude
  ( (-$)
  , (-.)
  , mf
  , bf
  , mh
  , bh
  ) where

infixr 0 -$
infixl 9 -.

(-$) :: a -> (a -> b) -> b
a -$ f = f a

(-.) :: (a -> b) -> (b -> c) -> (a -> c)
f -. g = g . f

mf :: (a -> b -> c) -> (a -> b) -> (a -> c)
mf f g = \a -> a `f` g a

bf :: (a -> c -> d) -> (b -> c) -> (a -> b -> d)
bf f g = \a b -> a `f` g b

mh :: (a -> b) -> (b -> c -> d) -> (a -> c) -> (a -> d)
mh f g h = \x -> f x `g` h x

bh :: (x -> a) -> (a -> b -> c) -> (y -> b) -> (x -> y -> c)
bh f g h = \x y -> f x `g` h y

