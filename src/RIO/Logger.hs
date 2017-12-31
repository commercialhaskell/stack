{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
module RIO.Logger
  ( LogLevel (..)
  , LogSource
  , LogStr
  , HasLogFunc (..)
  , logGeneric
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logOther
  , logSticky
  , logStickyDone
  , runNoLogging
  , NoLogging (..)
  ) where

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Lens.Micro (Getting, to)
import Lens.Micro.Mtl (view)
import GHC.Stack (HasCallStack, CallStack)

data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelOther !Text
    deriving (Eq, Show, Read, Ord)

type LogSource = Text
type LogStr = Text
class HasLogFunc env where
  logFuncL :: Getting r env (CallStack -> LogSource -> LogLevel -> LogStr -> IO ())

logGeneric
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogSource
  -> LogLevel
  -> LogStr
  -> m ()
logGeneric src level str = do
  logFunc <- view logFuncL
  liftIO $ logFunc ?callStack src level str

logDebug
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogStr
  -> m ()
logDebug = logGeneric "" LevelDebug

logInfo
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogStr
  -> m ()
logInfo = logGeneric "" LevelInfo

logWarn
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogStr
  -> m ()
logWarn = logGeneric "" LevelWarn

logError
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogStr
  -> m ()
logError = logGeneric "" LevelError

logOther
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => Text -- ^ level
  -> LogStr
  -> m ()
logOther = logGeneric "" . LevelOther

runNoLogging :: MonadIO m => ReaderT NoLogging m a -> m a
runNoLogging = flip runReaderT NoLogging

data NoLogging = NoLogging
instance HasLogFunc NoLogging where
  logFuncL = to (\_ _ _ _ _ -> return ())

-- | Write a "sticky" line to the terminal. Any subsequent lines will
-- overwrite this one, and that same line will be repeated below
-- again. In other words, the line sticks at the bottom of the output
-- forever. Running this function again will replace the sticky line
-- with a new sticky line. When you want to get rid of the sticky
-- line, run 'logStickyDone'.
--
logSticky :: (MonadIO m, HasCallStack, MonadReader env m, HasLogFunc env) => Text -> m ()
logSticky = logOther "sticky"

-- | This will print out the given message with a newline and disable
-- any further stickiness of the line until a new call to 'logSticky'
-- happens.
--
-- It might be better at some point to have a 'runSticky' function
-- that encompasses the logSticky->logStickyDone pairing.
logStickyDone :: (MonadIO m, HasCallStack, MonadReader env m, HasLogFunc env) => Text -> m ()
logStickyDone = logOther "sticky-done"
