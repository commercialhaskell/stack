{-# LANGUAGE RankNTypes #-}
-- | FIXME to be moved to an external package at some point
module Control.Monad.IO.Unlift
  ( MonadUnliftIO (..)
  , UnliftIO (..)
  , askRunIO
  , withUnliftIO
  , withRunIO
  , toIO
  , MonadIO (..)

  , Res.ResourceT
  , runResourceT
  , liftResourceT
  , runConduitRes

  , catch
  , catchIO
  , catchAny
  , catchAnyDeep
  , catchJust

  , handle
  , handleIO
  , handleAny
  , handleAnyDeep
  , handleJust

  , try
  , tryIO
  , tryAny
  , tryAnyDeep
  , tryJust

  , ES.Exception (..)
  , ES.SomeException (..)
  , E.ErrorCall
  , ES.IOException
  , E.assert
  , ES.MonadThrow -- FIXME perhaps completely ditch MonadThrow?
  , throwIO
  , ES.throwM
  , ES.impureThrow
  , ES.Handler (..)
  , evaluate
  , bracket
  , bracket_
  , bracketOnError
  , bracketOnError_
  , finally
  , withException
  , onException

  , M.MVar
  , newMVar
  , modifyMVar
  , modifyMVar_
  , takeMVar
  , withMVar
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class
import Control.Monad.Logger (LoggingT (..), NoLoggingT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Control.Monad.Trans.Resource as Res
import qualified Control.Monad.Trans.Resource.Internal as Res
import qualified Control.Exception as E (ErrorCall, evaluate, assert)
import qualified Control.Exception.Safe as ES
import qualified Data.Conduit as Con
import Data.Void (Void)
import qualified Control.Concurrent.MVar as M

-- FIXME consider making MonadThrow a superclass and demanding that
-- throwIO = throwM
class MonadIO m => MonadUnliftIO m where
  askUnliftIO :: m (UnliftIO m)
  -- Would be better, but GHC hates us
  -- askUnliftIO :: m (forall a. m a -> IO a)
instance MonadUnliftIO IO where
  askUnliftIO = return (UnliftIO id)
instance MonadUnliftIO m => MonadUnliftIO (ReaderT r m) where
  askUnliftIO = ReaderT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip runReaderT r))
instance MonadUnliftIO m => MonadUnliftIO (LoggingT m) where
  askUnliftIO = LoggingT $ \f ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip runLoggingT f))
instance MonadUnliftIO m => MonadUnliftIO (NoLoggingT m) where
  askUnliftIO = NoLoggingT $
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . runNoLoggingT))
instance MonadUnliftIO m => MonadUnliftIO (Res.ResourceT m) where
  askUnliftIO = Res.ResourceT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip Res.unResourceT r))

{- Invalid instance, violates the laws
instance MonadUnliftIO (StateT s IO) where
  askUnliftIO = StateT $ \s0 -> do
    let u = UnliftIO $ \m -> do
          (a, s1) <- runStateT m s0 -- Invalid by construction! Fails the MonadUnliftIO laws
          return a
    return (u, s0)
-}

newtype UnliftIO m = UnliftIO { unliftIO :: forall a. m a -> IO a }

askRunIO :: MonadUnliftIO m => m (m a -> IO a)
askRunIO = fmap unliftIO askUnliftIO

withUnliftIO :: MonadUnliftIO m => (UnliftIO m -> IO a) -> m a
withUnliftIO inner = askUnliftIO >>= liftIO . inner

withRunIO :: MonadUnliftIO m => ((m a -> IO a) -> IO b) -> m b
withRunIO inner = askRunIO >>= liftIO . inner

toIO :: MonadUnliftIO m => m a -> m (IO a)
toIO m = withRunIO $ \run -> return $ run m

runResourceT :: MonadUnliftIO m => Res.ResourceT m a -> m a
runResourceT m = withRunIO $ \run -> Res.runResourceT $ Res.transResourceT run m

liftResourceT :: MonadIO m => Res.ResourceT IO a -> Res.ResourceT m a
liftResourceT (Res.ResourceT f) = Res.ResourceT $ liftIO . f

runConduitRes :: MonadUnliftIO m => Con.ConduitM () Void (Res.ResourceT m) r -> m r
runConduitRes = runResourceT . Con.runConduit

catch :: (MonadUnliftIO m, ES.Exception e) => m a -> (e -> m a) -> m a
catch x y = withUnliftIO $ \u -> unliftIO u x `ES.catch` (unliftIO u . y)

catchIO :: MonadUnliftIO m => m a -> (ES.IOException -> m a) -> m a
catchIO = catch

catchAny :: MonadUnliftIO m => m a -> (ES.SomeException -> m a) -> m a
catchAny = catch

catchAnyDeep :: (NFData a, MonadUnliftIO m) => m a -> (ES.SomeException -> m a) -> m a
catchAnyDeep x y = withUnliftIO $ \u -> unliftIO u x `ES.catchAnyDeep` (unliftIO u . y)

catchJust :: (MonadUnliftIO m, ES.Exception e) => (e -> Maybe b) -> m a -> (b -> m a) -> m a
catchJust f a b = a `catch` \e -> maybe (liftIO (ES.throwM e)) b $ f e

handle :: (MonadUnliftIO m, ES.Exception e) => (e -> m a) -> m a -> m a
handle = flip catch

handleIO :: MonadUnliftIO m => (ES.IOException -> m a) -> m a -> m a
handleIO = handle

handleAny :: MonadUnliftIO m => (ES.SomeException -> m a) -> m a -> m a
handleAny = handle

handleAnyDeep :: (MonadUnliftIO m, NFData a) => (ES.SomeException -> m a) -> m a -> m a
handleAnyDeep = flip catchAnyDeep

handleJust :: (MonadUnliftIO m, ES.Exception e) => (e -> Maybe b) -> (b -> m a) -> m a -> m a
handleJust f = flip (catchJust f)

try :: (MonadUnliftIO m, ES.Exception e) => m a -> m (Either e a)
try m = withRunIO $ \run -> ES.try (run m)

tryIO :: MonadUnliftIO m => m a -> m (Either ES.IOException a)
tryIO = try

tryAny :: MonadUnliftIO m => m a -> m (Either ES.SomeException a)
tryAny = try

tryAnyDeep :: (MonadUnliftIO m, NFData a) => m a -> m (Either ES.SomeException a)
tryAnyDeep m = withRunIO $ \run -> ES.tryAnyDeep (run m)

tryJust :: (MonadUnliftIO m, ES.Exception e) => (e -> Maybe b) -> m a -> m (Either b a)
tryJust f m = withRunIO $ \run -> ES.tryJust f (run m)

evaluate :: MonadIO m => a -> m a
evaluate = liftIO . E.evaluate

bracket :: MonadUnliftIO m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket x y z = withUnliftIO $ \u -> ES.bracket
  (unliftIO u x)
  (unliftIO u . y)
  (unliftIO u . z)

bracket_ :: MonadUnliftIO m => m a -> m b -> m c -> m c
bracket_ x y z = withUnliftIO $ \u -> ES.bracket_
  (unliftIO u x)
  (unliftIO u y)
  (unliftIO u z)

bracketOnError :: MonadUnliftIO m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketOnError x y z = withUnliftIO $ \u -> ES.bracketOnError
  (unliftIO u x)
  (unliftIO u . y)
  (unliftIO u . z)

bracketOnError_ :: MonadUnliftIO m => m a -> m b -> m c -> m c
bracketOnError_ x y z = withUnliftIO $ \u -> ES.bracketOnError_
  (unliftIO u x)
  (unliftIO u y)
  (unliftIO u z)

finally :: MonadUnliftIO m => m a -> m b -> m a
finally x y = withUnliftIO $ \u -> ES.finally
  (unliftIO u x)
  (unliftIO u y)

withException :: (MonadUnliftIO m, ES.Exception e)
              => m a -> (e -> m b) -> m a
withException x y = withUnliftIO $ \u -> ES.withException
  (unliftIO u x)
  (unliftIO u . y)

onException :: MonadUnliftIO m => m a -> m b -> m a
onException x y = withUnliftIO $ \u -> ES.onException
  (unliftIO u x)
  (unliftIO u y)

-- FIXME I'm not too happy about differing behavior between throwM and throwIO
throwIO :: (MonadIO m, ES.Exception e) => e -> m a
throwIO = liftIO . ES.throwM

newMVar :: MonadIO m => a -> m (M.MVar a)
newMVar = liftIO . M.newMVar

modifyMVar :: MonadUnliftIO m => M.MVar a -> (a -> m (a, b)) -> m b
modifyMVar var f = withRunIO $ \run -> M.modifyMVar var (run . f)

modifyMVar_ :: MonadUnliftIO m => M.MVar a -> (a -> m a) -> m ()
modifyMVar_ var f = withRunIO $ \run -> M.modifyMVar_ var (run . f)

takeMVar :: MonadIO m => M.MVar a -> m a
takeMVar = liftIO . M.takeMVar

withMVar :: MonadUnliftIO m => M.MVar a -> (a -> m b) -> m b
withMVar var f = withRunIO $ \run -> M.withMVar var (run . f)
