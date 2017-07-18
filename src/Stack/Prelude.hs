{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Stack.Prelude
  ( mapLeft
  , runConduitRes
  , withSystemTempDir
  , StackT (..)
  , HasLogFunc (..)
  , module X
  ) where

import Text.Read as X (readMaybe)
import UnliftIO as X
import Control.Monad.Catch as X (MonadThrow (..)) -- future consideration: move to explicit Either
import Data.Conduit as X (runConduit, (.|), ConduitM)
import Data.Void as X (Void, absurd)
import Path as X (Path, Abs, Rel, Dir, File)
import Control.Monad.Logger as X
       (MonadLogger(..), MonadLoggerIO(..), logDebug, logInfo, logWarn,
        logError, toLogStr, Loc, LogSource, LogLevel, LogStr)
import Control.Monad.Reader as X (MonadReader, ask, asks, ReaderT (..), MonadTrans (..))
import Lens.Micro as X (Getting)
import Lens.Micro.Mtl as X (view)

import qualified Path.IO

mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b
mapLeft f (Left a1) = Left (f a1)
mapLeft _ (Right b) = Right b

runConduitRes :: MonadUnliftIO m => ConduitM () Void (ResourceT m) r -> m r
runConduitRes = runResourceT . runConduit

-- | Path version
withSystemTempDir :: MonadUnliftIO m => String -> (Path Abs Dir -> m a) -> m a
withSystemTempDir str inner = withRunInIO $ \run -> Path.IO.withSystemTempDir str $ run . inner

--------------------------------------------------------------------------------
-- Main StackT monad transformer

-- | The monad used for the executable @stack@.
newtype StackT env m a =
  StackT {unStackT :: ReaderT env m a}
  deriving (Functor,Applicative,Monad,MonadIO,MonadReader env,MonadThrow,MonadTrans)

class HasLogFunc env where
  logFuncL :: Getting r env (Loc -> LogSource -> LogLevel -> LogStr -> IO ())

instance (MonadIO m, HasLogFunc env) => MonadLogger (StackT env m) where
  monadLoggerLog a b c d = do
    f <- view logFuncL
    liftIO $ f a b c $ toLogStr d

instance (MonadIO m, HasLogFunc env) => MonadLoggerIO (StackT env m) where
  askLoggerIO = view logFuncL

instance MonadUnliftIO m => MonadUnliftIO (StackT config m) where
    askUnliftIO = StackT $ ReaderT $ \r ->
                  withUnliftIO $ \u ->
                  return (UnliftIO (unliftIO u . flip runReaderT r . unStackT))
