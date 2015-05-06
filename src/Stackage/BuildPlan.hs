{-# LANGUAGE EmptyDataDecls #-}


-- | Resolving a build plan for a set of packages in a given Stackage
-- snapshot.

module Stackage.BuildPlan where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Set (Set)
import           Stackage.Config
import           Stackage.PackageIdentifier

data BuildPlanException

-- | Includes things like flags, the current package database
-- location, etc.
data BuildPlanConfig

resolveBuildPlan :: (MonadLogger m,MonadIO m,MonadThrow m)
                 => Config
                 -> BuildPlanConfig
                 -> Set PackageIdentifier
                 -> m (Set PackageIdentifier)
resolveBuildPlan = undefined
