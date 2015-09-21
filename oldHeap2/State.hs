module State
  ( State(..)
  ) where

newtype State s a
  = State { runState :: s -> (a,s) }

instance Monad (State s) where
  return a = State $ \s -> (a,s)
  (State f) >>= g
    = State $ \s -> let (a,s') = f s
                        State h = g a
                    in h s'

