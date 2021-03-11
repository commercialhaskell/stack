{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | These are the types for the Execute module. It's separated from the Control.Concurrent.Execute
-- because it's entirely stack specific and could never be generalized.
module Stack.Types.Execute
(
    ExecuteEnv(..)
    , ExecutableBuildStatus(..)
    , packageNamePrefix
    , announceTask
    , OutputType(..)
)
where

import           Stack.Prelude
import           Stack.Types.Build
import           Stack.Types.Config
import           Stack.Types.GhcPkgId
import Data.List ( repeat )
import qualified RIO

data ExecuteEnv = ExecuteEnv
    { eeConfigureLock  :: !(MVar ())
    , eeInstallLock    :: !(MVar ())
    , eeBuildOpts      :: !BuildOpts
    , eeBuildOptsCLI   :: !BuildOptsCLI
    , eeBaseConfigOpts :: !BaseConfigOpts
    , eeGhcPkgIds      :: !(TVar (Map PackageIdentifier Installed))
    -- ^ The map of installed packages, pinned by their 'PackageIdentifier'.
    -- This is a TVar to prevent errors while concurrently executing installs
    -- and trying to update the value.
    , eeTempDir        :: !(Path Abs Dir)
    , eeSetupHs        :: !(Path Abs File)
    -- ^ Temporary Setup.hs for simple builds
    , eeSetupShimHs    :: !(Path Abs File)
    -- ^ Temporary SetupShim.hs, to provide access to initial-build-steps
    , eeSetupExe       :: !(Maybe (Path Abs File))
    -- ^ Compiled version of eeSetupHs
    , eeCabalPkgVer    :: !Version
    , eeTotalWanted    :: !Int
    , eeLocals         :: ![LocalPackage]
    , eeGlobalDB       :: !(Path Abs Dir)
    , eeGlobalDumpPkgs :: !(Map GhcPkgId DumpPackage)
    , eeSnapshotDumpPkgs :: !(TVar (Map GhcPkgId DumpPackage))
    , eeLocalDumpPkgs  :: !(TVar (Map GhcPkgId DumpPackage))
    , eeLogFiles       :: !(TChan (Path Abs Dir, Path Abs File))
    , eeCustomBuilt    :: !(IORef (Set PackageName))
    -- ^ Stores which packages with custom-setup have already had their
    -- Setup.hs built.
    , eeLargestPackageName :: !(Maybe Int)
    -- ^ For nicer interleaved output: track the largest package name size
    , eePathEnvVar :: !Text
    -- ^ Value of the PATH environment variable
    }

-- | Has an executable been built or not?
data ExecutableBuildStatus
    = ExecutableBuilt
    | ExecutableNotBuilt
  deriving (Show, Eq, Ord)

-- | Make a padded prefix for log messages
packageNamePrefix :: ExecuteEnv -> PackageName -> Utf8Builder
packageNamePrefix ee name' =
  let name = packageNameString name'
      paddedName =
        case eeLargestPackageName ee of
          Nothing -> name
          Just len -> assert (len >= length name) $ RIO.take len $ name ++ repeat ' '
   in fromString paddedName <> "> "

announceTask :: HasLogFunc env => ExecuteEnv -> Task -> Utf8Builder -> RIO env ()
announceTask ee task action = logInfo $
    packageNamePrefix ee (pkgName (taskProvides task)) <>
    action

-- | How we deal with output from GHC, either dumping to a log file or the
-- console (with some prefix).
data OutputType
  = OTLogFile !(Path Abs File) !Handle
  | OTConsole !(Maybe Utf8Builder)
