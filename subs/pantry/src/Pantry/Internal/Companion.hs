{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Companion threads, such as for printing messages saying we're
-- still busy. Ultimately this could be put into its own package. This
-- is a non-standard API for use by Pantry and Stack, please /DO NOT
-- DEPEND ON IT/.
module Pantry.Internal.Companion
  ( withCompanion
  , onCompanionDone
  , Companion
  , Delay
  , StopCompanion
  ) where

import RIO

-- | A companion thread which can perform arbitrary actions as well as delay
type Companion m = Delay -> m ()

-- | Delay the given number of microseconds. If 'StopCompanion' is
-- triggered before the timer completes, a 'CompanionDone' exception
-- will be thrown (which is caught internally by 'withCompanion').
type Delay = forall mio. MonadIO mio => Int -> mio ()

-- | Tell the 'Companion' to stop. The next time 'Delay' is
-- called, or if a 'Delay' is currently blocking, the 'Companion' thread
-- will exit with a 'CompanionDone' exception.
type StopCompanion m = m ()

-- | When a delay was interrupted because we're told to stop, perform
-- this action.
onCompanionDone
  :: MonadUnliftIO m
  => m () -- ^ the delay
  -> m () -- ^ action to perform
  -> m ()
onCompanionDone theDelay theAction =
  theDelay `withException` \CompanionDone -> theAction

-- | Internal exception used by 'withCompanion' to allow short-circuiting
-- of the 'Companion'. Should not be used outside of this module.
data CompanionDone = CompanionDone
  deriving (Show, Typeable)
instance Exception CompanionDone

-- | Keep running the 'Companion' action until either the inner action
-- completes or calls the 'StopCompanion' action. This can be used to
-- give the user status information while running a long running
-- operations.
withCompanion
  :: forall m a. MonadUnliftIO m
  => Companion m
  -> (StopCompanion m -> m a)
  -> m a
withCompanion companion inner = do
  -- Variable to indicate 'Delay'ing should result in a 'CompanionDone'
  -- exception.
  shouldStopVar <- newTVarIO False
  let -- Relatively simple: set shouldStopVar to True
      stopCompanion = atomically $ writeTVar shouldStopVar True

      delay :: Delay
      delay usec = do
        -- Register a delay with the runtime system
        delayDoneVar <- registerDelay usec
        join $ atomically $
          -- Delay has triggered, keep going
          (pure () <$ (readTVar delayDoneVar >>= checkSTM)) <|>
          -- Time to stop the companion, throw a 'CompanionDone' exception immediately
          (throwIO CompanionDone <$ (readTVar shouldStopVar >>= checkSTM))

  -- Run the 'Companion' and inner action together
  runConcurrently $
    -- Ignore a 'CompanionDone' exception from the companion, that's expected behavior
    Concurrently (companion delay `catch` \CompanionDone -> pure ()) *>
    -- Run the inner action, giving it the 'StopCompanion' action, and
    -- ensuring it is called regardless of exceptions.
    Concurrently (inner stopCompanion `finally` stopCompanion)
