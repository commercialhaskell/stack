{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | All data types.

module Stackage.Build.Types where

import Control.Exception
import Data.Aeson
import Data.Data
import Data.Default
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Yaml
import Development.Shake (Verbosity)
import Distribution.Package hiding (Package,PackageName)
import GHC.Generics
import Prelude hiding (FilePath)
import Stackage.FlagName
import Stackage.GhcPkgId
import Stackage.Package
import Stackage.PackageName
import Stackage.PackageVersion

data StackageBuildException
  = FPNoConfigFile
  | FPConfigError ParseException
  | FPMissingTool Dependency
  | FPCouldn'tFindPkgId PackageName
  | FPMissingDep Package PackageName VersionRange
  | FPStackageDepVerMismatch PackageName PackageVersion VersionRange
  | FPStackagePackageVersionMismatch PackageName PackageVersion PackageVersion
  | FPDependencyIssues [StackageBuildException]
  deriving (Typeable,Show)

instance Exception StackageBuildException

-- | Configuration for building.
data BuildConfig =
  BuildConfig {bconfigTargets :: ![Text]
              ,bconfigVerbosity :: !Verbosity
              ,bconfigLibProfile :: !Bool
              ,bconfigExeProfile :: !Bool
              ,bconfigEnableOptimizations :: !(Maybe Bool)
              ,bconfigFinalAction :: !FinalAction
              ,bconfigDryrun :: !Bool
              ,bconfigGhcOptions :: ![Text]
              ,bconfigInDocker :: !Bool}
  deriving (Show)

-- | Configuration for testing.
data TestConfig =
  TestConfig {tconfigTargets :: ![Text]
             ,tconfigVerbosity :: !Verbosity
             ,tconfigInDocker :: !Bool}
  deriving (Show)

-- | Configuration for haddocking.
data HaddockConfig =
  HaddockConfig {hconfigTargets :: ![Text]
                ,hconfigVerbosity :: !Verbosity
                ,hconfigInDocker :: !Bool}
  deriving (Show)

-- | Configuration for benchmarking.
data BenchmarkConfig =
  BenchmarkConfig {benchTargets :: ![Text]
                  ,benchVerbosity :: !Verbosity
                  ,benchInDocker :: !Bool}
  deriving (Show)

-- | Generated config for a package build.
data GenConfig =
  GenConfig {gconfigOptimize :: !Bool
            ,gconfigForceRecomp :: !Bool
            ,gconfigLibProfiling :: !Bool
            ,gconfigExeProfiling :: !Bool
            ,gconfigGhcOptions :: ![Text]
            ,gconfigFlags :: !(Map FlagName Bool)
            ,gconfigPkgId :: GhcPkgId}
  deriving (Generic,Show)

instance FromJSON GenConfig
instance ToJSON GenConfig

instance Default GenConfig where
  def =
    GenConfig {gconfigOptimize = False
              ,gconfigForceRecomp = False
              ,gconfigLibProfiling = False
              ,gconfigExeProfiling = False
              ,gconfigGhcOptions = []
              ,gconfigFlags = mempty
              ,gconfigPkgId = fromJust (parseGhcPkgIdFromString "")}

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
