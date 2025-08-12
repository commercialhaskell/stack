{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Stack.Types.Plan
Description : Plan-related types and functions.
License     : BSD-3-Clause

Plan-related types and functions.
-}

module Stack.Types.Plan
  ( Plan (..)
  , Task (..)
  , TaskType (..)
  , TaskConfigOpts (..)
  , taskAnyMissing
  , taskIsTarget
  , taskLocation
  , taskProvides
  , taskTargetIsMutable
  , taskTypeLocation
  , taskTypePackageIdentifier
  , installLocationIsMutable
  ) where

import           Data.List as L
import qualified RIO.Set as Set
import           Stack.Prelude
import           Stack.Types.Cache ( CachePkgSrc )
import           Stack.Types.ComponentUtils ( StackUnqualCompName )
import           Stack.Types.ConfigureOpts
                   ( BaseConfigOpts, PackageConfigureOpts )
import           Stack.Types.EnvConfig ( EnvConfig )
import           Stack.Types.GhcPkgId ( GhcPkgId )
import           Stack.Types.IsMutable ( IsMutable (..) )
import           Stack.Types.Package
                   ( InstallLocation (..), LocalPackage (..), Package (..)
                   , packageIdentifier
                   )

-- | A complete plan of what needs to be built and how to do it
data Plan = Plan
  { tasks :: !(Map PackageName Task)
  , finals :: !(Map PackageName Task)
    -- ^ Final actions to be taken (test, benchmark, etc)
  , unregisterLocal :: !(Map GhcPkgId (PackageIdentifier, Text))
    -- ^ Text is reason we're unregistering, for display only
  , installExes :: !(Map StackUnqualCompName InstallLocation)
    -- ^ Executables that should be installed after successful building
  }
  deriving Show

-- | A type representing tasks to perform when building.
data Task = Task
  { taskType        :: !TaskType
    -- ^ The task type, telling us how to build this
  , configOpts      :: !TaskConfigOpts
    -- ^ A set of the package identifiers of dependencies for which 'GhcPkgId'
    -- are missing and a function which yields configure options, given a
    -- dictionary of those identifiers and their 'GhcPkgId'.
  , buildHaddocks   :: !Bool
  , present         :: !(Map PackageIdentifier GhcPkgId)
    -- ^ A dictionary of the package identifiers of already-installed
    -- dependencies, and their 'GhcPkgId'.
  , allInOne        :: !Bool
    -- ^ indicates that the package can be built in one step
  , cachePkgSrc     :: !CachePkgSrc
  , buildTypeConfig :: !Bool
    -- ^ Is the build type of this package Configure. Check out
    -- ensureConfigureScript in Stack.Build.Execute for the motivation
  }
  deriving Show

-- | Type representing different types of task, depending on what is to be
-- built.
data TaskType
  = TTLocalMutable LocalPackage
    -- ^ Building local source code.
  | TTRemotePackage IsMutable Package PackageLocationImmutable
    -- ^ Building something from the package index (upstream).
  deriving Show

-- | Given the IDs of any missing packages, produce the configure options
data TaskConfigOpts = TaskConfigOpts
  { missing :: !(Set PackageIdentifier)
    -- ^ Dependencies for which we don't yet have a 'GhcPkgId'
  , envConfig :: !EnvConfig
  , baseConfigOpts :: !BaseConfigOpts
  , isLocalNonExtraDep :: !Bool
  , isMutable :: !IsMutable
  , pkgConfigOpts :: PackageConfigureOpts
  }

instance Show TaskConfigOpts where
  show tco = "Missing: " ++ show tco.missing

-- | Were any of the dependencies missing?

taskAnyMissing :: Task -> Bool
taskAnyMissing task = not $ Set.null task.configOpts.missing

-- | A function to yield the package name and version of a given 'TaskType'
-- value.
taskTypePackageIdentifier :: TaskType -> PackageIdentifier
taskTypePackageIdentifier (TTLocalMutable lp) = packageIdentifier lp.package
taskTypePackageIdentifier (TTRemotePackage _ p _) = packageIdentifier p

taskIsTarget :: Task -> Bool
taskIsTarget t =
  case t.taskType of
    TTLocalMutable lp -> lp.wanted
    _ -> False

-- | A function to yield the relevant database (write-only or mutable) of a
-- given 'TaskType' value.
taskTypeLocation :: TaskType -> InstallLocation
taskTypeLocation (TTLocalMutable _) = Local
taskTypeLocation (TTRemotePackage Mutable _ _) = Local
taskTypeLocation (TTRemotePackage Immutable _ _) = Snap

-- | A function to yield the relevant database (write-only or mutable) of the
-- given task.
taskLocation :: Task -> InstallLocation
taskLocation = taskTypeLocation . (.taskType)

-- | A function to yield the package name and version to be built by the given
-- task.
taskProvides :: Task -> PackageIdentifier
taskProvides = taskTypePackageIdentifier . (.taskType)

taskTargetIsMutable :: Task -> IsMutable
taskTargetIsMutable task =
  case task.taskType of
    TTLocalMutable _ -> Mutable
    TTRemotePackage mutable _ _ -> mutable

installLocationIsMutable :: InstallLocation -> IsMutable
installLocationIsMutable Snap = Immutable
installLocationIsMutable Local = Mutable
