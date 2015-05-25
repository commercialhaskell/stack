-- | Extra Maybe utilities.

module Data.Maybe.Extra where

import Control.Monad
import Data.Maybe

-- | Monadic 'mapMaybe'.
mapMaybeM :: Monad f => (a -> f (Maybe b)) -> [a] -> f [b]
mapMaybeM f = liftM catMaybes . mapM f
