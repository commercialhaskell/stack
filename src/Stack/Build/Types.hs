{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | All data types.

module Stack.Build.Types where

import Control.DeepSeq
import Control.Exception
import Data.Aeson
import Data.Binary (Binary(..))
import Data.Data
import Data.Hashable
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Distribution.Package hiding (Package,PackageName)
import GHC.Generics
import Prelude hiding (FilePath)
import Stack.Package
import Stack.Types

data StackBuildException
  = MissingTool Dependency
  | Couldn'tFindPkgId PackageName
  | MissingDep Package PackageName VersionRange
  | MissingDep2 PackageName PackageName VersionRange
  | MismatchedLocalDep PackageName Version PackageName VersionRange
  | MismatchedDep PackageName Version PackageName VersionRange
  | StackageDepVerMismatch PackageName Version VersionRange
  | StackageVersionMismatch PackageName Version Version
  | DependencyIssues [StackBuildException]
  | GHCVersionMismatch (Maybe Version) Version
  | Couldn'tParseTargets [Text]
  | UnknownTargets [PackageName]
  deriving (Typeable,Show)

instance Exception StackBuildException

-- | Configuration for building.
data BuildOpts =
  BuildOpts {boptsTargets :: !(Either [Text] [PackageName])
             -- ^ Right value indicates that we're only installing
             -- dependencies, no local packages
            ,boptsLibProfile :: !Bool
            ,boptsExeProfile :: !Bool
            ,boptsEnableOptimizations :: !(Maybe Bool)
            ,boptsFinalAction :: !FinalAction
            ,boptsDryrun :: !Bool
            ,boptsGhcOptions :: ![Text]
            }
  deriving (Show)

-- | Configuration for testing.
data TestConfig =
  TestConfig {tconfigTargets :: ![Text]
             }
  deriving (Show)

-- | Configuration for haddocking.
data HaddockConfig =
  HaddockConfig {hconfigTargets :: ![Text]
                }
  deriving (Show)

-- | Configuration for benchmarking.
data BenchmarkConfig =
  BenchmarkConfig {benchTargets :: ![Text]
                  ,benchInDocker :: !Bool}
  deriving (Show)

-- | Generated config for a package build.
data GenConfig =
  GenConfig {gconfigOptimize :: !Bool
            ,gconfigLibProfiling :: !Bool
            ,gconfigExeProfiling :: !Bool
            ,gconfigGhcOptions :: ![Text]
            ,gconfigFlags :: !(Map FlagName Bool)
            ,gconfigPkgId :: Maybe GhcPkgId}
  deriving (Generic,Show)

instance FromJSON GenConfig
instance ToJSON GenConfig

defaultGenConfig :: GenConfig
defaultGenConfig =
    GenConfig {gconfigOptimize = False
              ,gconfigLibProfiling = False
              ,gconfigExeProfiling = False
              ,gconfigGhcOptions = []
              ,gconfigFlags = mempty
              ,gconfigPkgId = Nothing}

-- | Run a Setup.hs action after building a package, before installing.
data FinalAction
  = DoTests
  | DoBenchmarks
  | DoHaddock
  | DoNothing
  deriving (Eq,Bounded,Enum,Show)

data Dependencies =
  Dependencies {depsLibraries :: [PackageName]
               ,depsTools :: [PackageName]}
  deriving (Show,Typeable,Data)

-- | Used for mutex locking on the install step. Beats magic ().
data InstallLock = InstallLock

-- | Mutex for reading/writing .config files in dist/ of
-- packages. Shake works in parallel, without this there are race
-- conditions.
data ConfigLock = ConfigLock

-- | Package dependency oracle.
newtype PkgDepsOracle =
    PkgDeps PackageName
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
