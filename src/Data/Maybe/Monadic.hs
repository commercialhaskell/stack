-- | Some utilities.

module Data.Maybe.Monadic where

import Control.Applicative
import Data.Maybe
import Data.Traversable

-- | Applicative 'mapMaybe'.
mapMaybeA :: Applicative f => (a -> f (Maybe a)) -> [a] -> f [a]
mapMaybeA f = fmap catMaybes . traverse f
