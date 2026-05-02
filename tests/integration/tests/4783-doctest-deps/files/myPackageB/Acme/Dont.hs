module Acme.Dont where

don't :: (Monad m) => m a -> m ()
don't _action = return ()
