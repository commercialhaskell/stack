-- | Monadic operations for 'Set'.

module Data.Set.Monad
  (mapM
  ,mapM_
  ,mapMaybeM
  ,filterM)
  where

import           Control.Monad (liftM)
import qualified Control.Monad as L
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as S
import           Prelude hiding (mapM,mapM_)

-- | Map over a 'Set' in a monad.
mapM :: (Ord a,Ord b,Monad m)
      => (a -> m b) -> Set a -> m (Set b)
mapM f = liftM S.fromList . L.mapM f . S.toList

-- | Map over a 'Set' in a monad, discarding the result.
mapM_ :: (Ord a,Ord b,Monad m)
       => (a -> m b) -> Set a -> m ()
mapM_ f = L.mapM_ f . S.toList

-- | Map over a 'Set' in a monad, keeping the 'Just's and throwing out the 'Nothing's.
mapMaybeM :: (Ord a,Ord b,Monad m)
          => (a -> m (Maybe b)) -> Set a -> m (Set b)
mapMaybeM f = liftM (S.fromList . catMaybes) . L.mapM f . S.toList

-- | Filter elements of a 'Set' in a monad.
filterM :: (Ord a,Monad m)
         => (a -> m Bool) -> Set a -> m (Set a)
filterM f = liftM S.fromList . L.filterM f . S.toList
