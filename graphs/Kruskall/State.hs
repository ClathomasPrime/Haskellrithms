module State
  ( State(..)   -- run :: s -> (a,s)
  , eval        -- eval :: State s a -> s -> a
  , exec        -- exec :: State s a -> s -> s
  , perform     -- perform :: (s -> s) -> State s ()
  , view        -- view :: State s s
  , change      -- change :: s -> State s ()
  ) where

import Control.Applicative

data State s a = State { run :: s -> (a,s) }

eval :: State s a -> s -> a
eval u = fst . run u

exec :: State s a -> s -> s
exec u = snd . run u

perform :: (s -> s) -> State s ()
perform f = State $ \s -> ((),f s)

view :: State s s
view = State $ \s -> (s,s)

change :: s -> State s ()
change s = State $ const ((),s)

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (a,s') = g s in (f a,s')

instance Applicative (State s) where
  pure a = State $ \s -> (a,s)
  State u <*> State f = State
    $ \s -> let (g,s') = u s
                (a,s'') = f s'
             in (g a,s'')

instance Monad (State s) where
  return = pure
  State f >>= g = State
    $ \s -> let (a,s') = f s
             in run (g a) s'
