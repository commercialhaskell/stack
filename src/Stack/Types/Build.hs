{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Build-specific types.

module Stack.Types.Build
  ( InstallLocation (..)
  , Installed (..)
  , psVersion
  , Task (..)
  , taskAnyMissing
  , taskIsTarget
  , taskLocation
  , taskProvides
  , taskTargetIsMutable
  , taskTypeLocation
  , taskTypePackageIdentifier
  , LocalPackage (..)
  , Plan (..)
  , TestOpts (..)
  , BenchmarkOpts (..)
  , FileWatchOpts (..)
  , BuildOpts (..)
  , BuildSubset (..)
  , defaultBuildOpts
  , TaskType (..)
  , installLocationIsMutable
  , TaskConfigOpts (..)
  , BuildCache (..)
  , ConfigCache (..)
  , configureOpts
  , CachePkgSrc (..)
  , toCachePkgSrc
  , FileCacheInfo (..)
  , PrecompiledCache (..)
  , ExcludeTHLoading (..)
  , ConvertPathsToAbsolute (..)
  , KeepOutputOpen (..)
  ) where

import           Data.Aeson ( ToJSON, FromJSON )
import qualified Data.ByteString as S
import           Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import           Database.Persist.Sql
                   ( PersistField (..), PersistFieldSql (..)
                   , PersistValue (PersistText), SqlType (SqlString)
                   )
import           Path ( parent )
import qualified RIO.Set as Set
import           Stack.Prelude
import           Stack.Types.BuildOpts
                   ( BenchmarkOpts (..), BuildOpts (..), BuildSubset (..)
                   , FileWatchOpts (..), TestOpts (..), defaultBuildOpts
                   )
import           Stack.Types.ConfigureOpts ( ConfigureOpts, configureOpts )
import           Stack.Types.GhcPkgId ( GhcPkgId )
import           Stack.Types.IsMutable ( IsMutable (..) )
import           Stack.Types.Package
                   ( FileCacheInfo (..), InstallLocation (..), Installed (..)
                   , LocalPackage (..), Package (..), PackageSource (..)
                   , packageIdentifier, psVersion
                   )

-- | Package dependency oracle.
newtype PkgDepsOracle
  = PkgDeps PackageName
  deriving (Eq, NFData, Show, Typeable)

-- | Stored on disk to know whether the files have changed.
newtype BuildCache = BuildCache
  { buildCacheTimes :: Map FilePath FileCacheInfo
    -- ^ Modification times of files.
  }
  deriving (Eq, FromJSON, Generic, Show, ToJSON, Typeable)

instance NFData BuildCache

-- | Stored on disk to know whether the flags have changed.
data ConfigCache = ConfigCache
  { opts :: !ConfigureOpts
    -- ^ All options used for this package.
  , deps :: !(Set GhcPkgId)
    -- ^ The GhcPkgIds of all of the dependencies. Since Cabal doesn't take
    -- the complete GhcPkgId (only a PackageIdentifier) in the configure
    -- options, just using the previous value is insufficient to know if
    -- dependencies have changed.
  , components :: !(Set S.ByteString)
    -- ^ The components to be built. It's a bit of a hack to include this in
    -- here, as it's not a configure option (just a build option), but this
    -- is a convenient way to force compilation when the components change.
  , haddock :: !Bool
    -- ^ Are haddocks to be built?
  , pkgSrc :: !CachePkgSrc
  , pathEnvVar :: !Text
  -- ^ Value of the PATH env var, see
  -- <https://github.com/commercialhaskell/stack/issues/3138>
  }
  deriving (Data, Eq, Generic, Show, Typeable)

instance NFData ConfigCache

data CachePkgSrc
  = CacheSrcUpstream
  | CacheSrcLocal FilePath
  deriving (Data, Eq, Generic, Read, Show, Typeable)

instance NFData CachePkgSrc

instance PersistField CachePkgSrc where
  toPersistValue CacheSrcUpstream = PersistText "upstream"
  toPersistValue (CacheSrcLocal fp) = PersistText ("local:" <> T.pack fp)
  fromPersistValue (PersistText t) =
    if t == "upstream"
      then Right CacheSrcUpstream
      else case T.stripPrefix "local:" t of
        Just fp -> Right $ CacheSrcLocal (T.unpack fp)
        Nothing -> Left $ "Unexpected CachePkgSrc value: " <> t
  fromPersistValue _ = Left "Unexpected CachePkgSrc type"

instance PersistFieldSql CachePkgSrc where
  sqlType _ = SqlString

toCachePkgSrc :: PackageSource -> CachePkgSrc
toCachePkgSrc (PSFilePath lp) =
  CacheSrcLocal (toFilePath (parent lp.cabalFile))
toCachePkgSrc PSRemote{} = CacheSrcUpstream

-- | A type representing tasks to perform when building.
data Task = Task
  { taskType            :: !TaskType
    -- ^ The task type, telling us how to build this
  , taskConfigOpts      :: !TaskConfigOpts
    -- ^ A set of the package identifiers of dependencies for which 'GhcPkgId'
    -- are missing and a function which yields configure options, given a
    -- dictionary of those identifiers and their 'GhcPkgId'.
  , taskBuildHaddock    :: !Bool
  , taskPresent         :: !(Map PackageIdentifier GhcPkgId)
    -- ^ A dictionary of the package identifiers of already-installed
    -- dependencies, and their 'GhcPkgId'.
  , taskAllInOne        :: !Bool
    -- ^ indicates that the package can be built in one step
  , taskCachePkgSrc     :: !CachePkgSrc
  , taskBuildTypeConfig :: !Bool
    -- ^ Is the build type of this package Configure. Check out
    -- ensureConfigureScript in Stack.Build.Execute for the motivation
  }
  deriving Show

-- | Given the IDs of any missing packages, produce the configure options
data TaskConfigOpts = TaskConfigOpts
  { tcoMissing :: !(Set PackageIdentifier)
    -- ^ Dependencies for which we don't yet have an GhcPkgId
  , tcoOpts    :: !(Map PackageIdentifier GhcPkgId -> ConfigureOpts)
    -- ^ Produce the list of options given the missing @GhcPkgId@s
  }

instance Show TaskConfigOpts where
  show (TaskConfigOpts missing f) = concat
    [ "Missing: "
    , show missing
    , ". Without those: "
    , show $ f Map.empty
    ]

-- | Type representing different types of task, depending on what is to be
-- built.
data TaskType
  = TTLocalMutable LocalPackage
    -- ^ Building local source code.
  | TTRemotePackage IsMutable Package PackageLocationImmutable
    -- ^ Building something from the package index (upstream).
  deriving Show

-- | Were any of the dependencies missing?

taskAnyMissing :: Task -> Bool
taskAnyMissing task = not $ Set.null task.taskConfigOpts.tcoMissing

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

-- | A complete plan of what needs to be built and how to do it
data Plan = Plan
  { planTasks :: !(Map PackageName Task)
  , planFinals :: !(Map PackageName Task)
    -- ^ Final actions to be taken (test, benchmark, etc)
  , planUnregisterLocal :: !(Map GhcPkgId (PackageIdentifier, Text))
    -- ^ Text is reason we're unregistering, for display only
  , planInstallExes :: !(Map Text InstallLocation)
    -- ^ Executables that should be installed after successful building
  }
  deriving Show

-- | Information on a compiled package: the library .conf file (if relevant),
-- the sub-libraries (if present) and all of the executable paths.
data PrecompiledCache base = PrecompiledCache
  { pcLibrary :: !(Maybe (Path base File))
    -- ^ .conf file inside the package database
  , pcSubLibs :: ![Path base File]
    -- ^ .conf file inside the package database, for each of the sub-libraries
  , pcExes    :: ![Path base File]
    -- ^ Full paths to executables
  }
  deriving (Eq, Generic, Show, Typeable)

instance NFData (PrecompiledCache Abs)

instance NFData (PrecompiledCache Rel)

data ExcludeTHLoading
  = ExcludeTHLoading
  | KeepTHLoading

data ConvertPathsToAbsolute
  = ConvertPathsToAbsolute
  | KeepPathsAsIs

-- | special marker for expected failures in curator builds, using those we need
-- to keep log handle open as build continues further even after a failure
data KeepOutputOpen
  = KeepOpen
  | CloseOnException
  deriving Eq
