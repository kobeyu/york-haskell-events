module IOInstances where
import Problems
import Control.Applicative 

instance Functor IOAction where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  fmap = undefined

instance Applicative IOAction where
  -- pure :: Applicative f => a -> f a
  pure = undefined

  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  (<*>) = undefined

instance Monad IOAction where
  -- return :: Monad m => a -> m a
  return = pure

  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  (>>=) = undefined

-- Now we can do something exciting: we can use do-notation with your
-- 'IOAction' type and it'll all work!
testIOActionDo :: IOAction ()
testIOActionDo = do
  writeln "Hello, what is your name?"
  name <- readln
  writeln ("Nice to meet you, " ++ name)
