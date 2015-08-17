module Data.IORef.RunOnce (runOnce) where

import Control.Monad.IO.Class
import Data.IORef

runOnce :: MonadIO m => m a -> m (m a)
runOnce f = do
    ref <- liftIO $ newIORef Nothing
    return $ do
        mval <- liftIO $ readIORef ref
        case mval of
            Just val -> return val
            Nothing -> do
                val <- f
                liftIO $ writeIORef ref (Just val)
                return val
