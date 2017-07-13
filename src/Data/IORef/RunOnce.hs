module Data.IORef.RunOnce (runOnce) where

import Control.Monad.IO.Unlift
import Data.IORef

runOnce :: (MonadUnliftIO m, MonadIO n) => m a -> m (n a)
runOnce f = withRunIO $ \runIO -> do
    ref <- newIORef Nothing
    return $ liftIO $ do
        mval <- readIORef ref
        case mval of
            Just val -> return val
            Nothing -> do
                val <- runIO f
                writeIORef ref (Just val)
                return val
