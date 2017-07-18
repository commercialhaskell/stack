{-# LANGUAGE NoImplicitPrelude #-}
module Data.IORef.RunOnce (runOnce) where

import Stack.Prelude

runOnce :: (MonadUnliftIO m, MonadIO n) => m a -> m (n a)
runOnce f = withRunInIO $ \run -> do
    ref <- newIORef Nothing
    return $ liftIO $ do
        mval <- readIORef ref
        case mval of
            Just val -> return val
            Nothing -> do
                val <- run f
                writeIORef ref (Just val)
                return val
