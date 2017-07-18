module Stack.Prelude
  ( mapLeft
  , readMaybe
  , module Control.Monad.IO.Unlift
  ) where

import Control.Monad.IO.Unlift
import Text.Read (readMaybe)

mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b
mapLeft f (Left a1) = Left (f a1)
mapLeft _ (Right b) = Right b
