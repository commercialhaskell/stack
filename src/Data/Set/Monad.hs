-- | Monadic operations for 'Set'.

module Data.Set.Monad
  (mapM
  ,filterM)
  where

import           Control.Monad (liftM)
import qualified Control.Monad as L
import           Data.Set (Set)
import qualified Data.Set as S
import           Prelude hiding (mapM)

-- | Map over a 'Set' in a monad.
mapM :: (Ord a,Ord b,Monad m)
      => (a -> m b) -> Set a -> m (Set b)
mapM f = liftM S.fromList . L.mapM f . S.toList

-- | Map over a 'Set' in a monad.
filterM :: (Ord a,Monad m)
         => (a -> m Bool) -> Set a -> m (Set a)
filterM f = liftM S.fromList . L.filterM f . S.toList
