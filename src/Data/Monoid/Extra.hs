-- | Extra Monoid utilities.

module Data.Monoid.Extra
    ( fromFirst
    , module Data.Monoid
    ) where

import Data.Maybe
import Data.Monoid

fromFirst :: a -> First a -> a
fromFirst x = fromMaybe x . getFirst
