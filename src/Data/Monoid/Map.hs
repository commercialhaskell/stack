{-|
Module      : Data.Monoid.Map
License     : BSD-3-Clause
-}

module Data.Monoid.Map
  ( MonoidMap (..)
  ) where

import qualified Data.Map as M
import           Stack.Prelude

-- | Utility newtype wrapper to make Map's Monoid also use the
-- element's Monoid.
newtype MonoidMap k a
  = MonoidMap (Map k a)
  deriving (Eq, Functor, Generic, Ord, Read, Show)

instance (Ord k, Semigroup a) => Semigroup (MonoidMap k a) where
  MonoidMap mp1 <> MonoidMap mp2 = MonoidMap (M.unionWith (<>) mp1 mp2)

instance (Ord k, Semigroup a) => Monoid (MonoidMap k a) where
  mappend = (<>)
  mempty = MonoidMap mempty
