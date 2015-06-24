import Control.Applicative ((<$>))
import Control.Concurrent  (forkIO, newMVar, swapMVar, readMVar)
import Control.Concurrent.CVar (newCVar, swapCVar, readCVar)
import Control.Monad.Conc.Class (MonadConc, fork)
import Control.Monad (liftM)

void :: Monad m => m a -> m ()
void = liftM $ const ()

-- | A simple example of nondeterministic concurrency.
example :: IO Int
example = do
  x <- newMVar 0
  forkIO . void $ swapMVar x 1
  forkIO . void $ swapMVar x 2
  readMVar x

-- | Déjà Fu version of the above.
example2 :: MonadConc m => m Int
example2 = do
  x <- newCVar 0
  fork . void $ swapCVar x 1
  fork . void $ swapCVar x 2
  readCVar x

-- | Run an 'IO' action 1000 times and report the frequency of each
-- result.
gather :: Eq a => IO a -> IO [(a, Double)]
gather c = compress <$> go runs where
  runs = 1000

  go 0 = return []
  go n = do
    c'   <- c
    rest <- go (n-1)
    return (c':rest)

  compress [] = []
  compress (x:xs) =
    let num = length $ filter (==x) xs
        xs' = filter (/=x) xs
    in (x, fromIntegral num / fromIntegral runs) : compress xs'

{-

Run @gather example@, see what you get.

Now import Control.Concurrent, run @setNumCapabilities 4@, and see
what @gather example@ gives you.

--------

Déjà Fu free monad type is like this:

data Action ... =
    AFork thread_action action
  | AMyTId (thread_id -> action)
  | APut cvar new_value action
  | ATryPut cvar new_value (Bool -> action)
  | AGet cvar (value -> action)
  | ATake cvar (value -> action)
  | ATryTake cvar (Maybe value -> action)
  | AReadRef cref (value -> action)
  | AModRef cref function (result -> action)
  | AAtom stm_action (result -> action)
  | ANew action
  | ANewRef action
  | ALift (underlying_monad action)
  | AThrow SomeException
  | AThrowTo thread_id SomeException action
  | ACatching handler action (result -> action)
  | AMasking mask_state action (result -> action)
  | AStop

https://github.com/barrucadu/dejafu/blob/master/Test/DejaFu/Deterministic/Internal/Common.hs

--------

SCT output:

       [ schedules with zero pre-emptions ]
     /  /  |  |  |  |  |  |  |  |  |  |  \  \
    [     schedules with one pre-emption     ]
  /  /  |  |  |  |  |  |  |  |  |  |  |  |  \  \
 [       schedules with two pre-emptions       ]

https://github.com/barrucadu/dejafu/blob/master/Test/DejaFu/SCT.hs

--------

Source: https://github.com/barrucadu/dejafu
Docs:   https://barrucadu.github.io/dejafu/

-}
