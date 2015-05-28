{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A limited interface to/wrapper around the Shake library.

module Shake
  (-- * Types
   S.Resource
  ,S.Rules
  ,S.ShakeOptions(..)
  ,MonadAction(..)
  -- * Actions
  ,shakeArgs
  ,S.shakeOptions
  ,newResource
  ,S.want
  ,actionFinally
  ,need
  ,withResource
  ,(S.%>))
  where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.Text as T
import           Development.Shake (Rules,Resource,Rules,Action)
import qualified Development.Shake as S
import           Path
import           System.Environment

-- | A Shake build action.
class (MonadIO m,Functor m,MonadLogger m,MonadThrow m) => MonadAction m where
  liftAction :: Action a -> m a
  unliftAction :: m (m a -> Action a)

instance MonadThrow Action where
    throwM = liftIO . throw

-- | Run a build system using command line arguments for
-- configuration.
shakeArgs :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
          => Path Abs Dir -- ^ Directory to put shake cache files.
          -> Int          -- ^ Threads.
          -> Rules ()     -- ^ Rules.
          -> m ()
shakeArgs dir threads m =
    liftBaseWith
        (\run ->
              (shake (opts run) m))
  where
    shake opts_ =
        liftIO .
        withArgs [] .
        S.shakeArgs opts_
    opts run =
        S.shakeOptions
        { S.shakeOutput = output run
        , S.shakeFiles = toFilePath dir
        , S.shakeThreads = threads
        , S.shakeVerbosity = S.Quiet
        }
    output run v =
        void . run . logFunction . T.pack
      where
        logFunction =
            case v of
                S.Silent ->
                    $logInfo
                S.Quiet ->
                    $logInfo
                S.Normal ->
                    $logInfo
                S.Loud ->
                    $logDebug
                S.Chatty ->
                    $logDebug
                S.Diagnostic ->
                    const (return ())

-- | Allocate a new resource.
newResource :: MonadIO m
            => String -> Int -> m Resource
newResource name int =
    liftIO (S.newResourceIO name int)

-- | Add a dependency on the file arguments, ensuring they are built
-- before continuing.
need :: MonadAction m
     => [FilePath] -> m ()
need = liftAction . S.need

-- | After an Action, perform some IO, even if there is an exception.
actionFinally :: MonadAction m
              => m a -> IO () -> m a
actionFinally maction mio = do
    run <- unliftAction
    liftAction
        (S.actionFinally
             (run maction)
             mio)

-- | Run an action which uses part of a finite resource. For more
-- details see Resource. You cannot depend on a rule (e.g. need) while
-- a resource is held.
withResource :: MonadAction m
             => Resource -> Int -> m a -> m a
withResource res int m = do
    run <- unliftAction
    liftAction
        (S.withResource res int (run m))
