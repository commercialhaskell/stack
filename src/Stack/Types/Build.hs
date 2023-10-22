{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Build-specific types.

module Stack.Types.Build
  ( InstallLocation (..)
  , Installed (..)
  , psVersion
  , Task (..)
  , taskIsTarget
  , taskLocation
  , taskProvides
  , taskTargetIsMutable
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
  { configCacheOpts :: !ConfigureOpts
    -- ^ All options used for this package.
  , configCacheDeps :: !(Set GhcPkgId)
    -- ^ The GhcPkgIds of all of the dependencies. Since Cabal doesn't take
    -- the complete GhcPkgId (only a PackageIdentifier) in the configure
    -- options, just using the previous value is insufficient to know if
    -- dependencies have changed.
  , configCacheComponents :: !(Set S.ByteString)
    -- ^ The components to be built. It's a bit of a hack to include this in
    -- here, as it's not a configure option (just a build option), but this
    -- is a convenient way to force compilation when the components change.
  , configCacheHaddock :: !Bool
    -- ^ Are haddocks to be built?
  , configCachePkgSrc :: !CachePkgSrc
  , configCachePathEnvVar :: !Text
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
  CacheSrcLocal (toFilePath (parent (lpCabalFile lp)))
toCachePkgSrc PSRemote{} = CacheSrcUpstream

-- | A task to perform when building
data Task = Task
  { taskType            :: !TaskType
    -- ^ the task type, telling us how to build this
  , taskConfigOpts      :: !TaskConfigOpts
  , taskBuildHaddock    :: !Bool
  , taskPresent         :: !(Map PackageIdentifier GhcPkgId)
    -- ^ GhcPkgIds of already-installed dependencies
  , taskAllInOne        :: !Bool
    -- ^ indicates that the package can be built in one step
  , taskCachePkgSrc     :: !CachePkgSrc
  , taskAnyMissing      :: !Bool
    -- ^ Were any of the dependencies missing? The reason this is necessary is...
    -- hairy. And as you may expect, a bug in Cabal. See:
    -- <https://github.com/haskell/cabal/issues/4728#issuecomment-337937673>.
    -- The problem is that Cabal may end up generating the same package ID for a
    -- dependency, even if the ABI has changed. As a result, without this field,
    -- Stack would think that a reconfigure is unnecessary, when in fact we _do_
    -- need to reconfigure. The details here suck. We really need proper hashes
    -- for package identifiers.
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

-- | A function to yield the package name and version of a given 'TaskType'
-- value.
taskTypePackageIdentifier :: TaskType -> PackageIdentifier
taskTypePackageIdentifier (TTLocalMutable lp) = packageIdentifier $ lpPackage lp
taskTypePackageIdentifier (TTRemotePackage _ p _) = packageIdentifier p

taskIsTarget :: Task -> Bool
taskIsTarget t =
  case taskType t of
    TTLocalMutable lp -> lpWanted lp
    _ -> False

taskLocation :: Task -> InstallLocation
taskLocation task =
  case taskType task of
    TTLocalMutable _ -> Local
    TTRemotePackage Mutable _ _ -> Local
    TTRemotePackage Immutable _ _ -> Snap

-- | A funtion to yield the package name and version to be built by the given
-- task.
taskProvides :: Task -> PackageIdentifier
taskProvides = taskTypePackageIdentifier . taskType

taskTargetIsMutable :: Task -> IsMutable
taskTargetIsMutable task =
  case taskType task of
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

-- | Information on a compiled package: the library conf file (if relevant),
-- the sublibraries (if present) and all of the executable paths.
data PrecompiledCache base = PrecompiledCache
  { pcLibrary :: !(Maybe (Path base File))
    -- ^ .conf file inside the package database
  , pcSubLibs :: ![Path base File]
    -- ^ .conf file inside the package database, for each of the sublibraries
  , pcExes    :: ![Path base File]
    -- ^ Full paths to executables
  }
  deriving (Eq, Generic, Show, Typeable)

instance NFData (PrecompiledCache Abs)

instance NFData (PrecompiledCache Rel)
