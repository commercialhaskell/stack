{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | All data types.

module Stackage.Build.Types where

import           Control.Applicative

import           Control.Exception
import           Data.Aeson
import           Data.Data
import           Data.Default
import           Data.Function
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import           Data.Yaml
import           Development.Shake (Verbosity)
import           Distribution.Package hiding (Package,PackageName)
import           GHC.Generics
import           Path as FL
import           Prelude hiding (FilePath)
import           Stackage.GhcPkgId
import           Stackage.Package
import           Stackage.PackageName
import           Stackage.PackageVersion

data StackageBuildException
  = FPNoConfigFile
  | FPConfigError ParseException
  | FPMissingTool Dependency
  | FPCouldn'tFindPkgId PackageName
  | FPMissingDep Package PackageName VersionRange
  | FPStackageDepVerMismatch PackageName PackageVersion VersionRange
  | FPStackagePackageVersionMismatch PackageName PackageVersion PackageVersion
  | FPDependencyIssues [StackageBuildException]
  | FPNoCabalFile (Path Abs Dir)
  deriving (Typeable,Show)

instance Exception StackageBuildException

-- | Stackage build config.
data Config =
  Config {configDocker :: !Docker
          -- ^ Local docker configuration.
         ,configPackages :: !(Set (Path Abs Dir))
          -- ^ List of local packages.
         ,configMaybeDir :: !(Maybe (Path Abs Dir))
          -- ^ Directory of the configuration file.
         ,configFlags :: !(Map Text Bool)
          -- ^ Flags for Cabal configuration.
         ,configPackageFlags :: !(Map PackageName (Map Text Bool))
          -- ^ Flags for each package's Cabal config.
         }
  deriving (Show)

-- | For YAML.
instance FromJSON (Path Abs Dir -> Config) where
  parseJSON v =
    do o <- parseJSON v
       config <$> o .:? dockerConfigFieldName .!= def <*>
         (do ps <- o .: "packages"
             fmap S.fromList
                  (mapM (\x ->
                           if x == "."
                              then return (Left ())
                              else fmap Right
                                        (either (fail . ("Unable to parse relative directory location: " ++) .
                                                        show)
                                                return
                                                (FL.parseRelDir x)))
                        ps)) <*>
         pure Nothing <*>
         fmap (fromMaybe mempty)
              (o .:? "flags") <*>
         fmap (M.fromList .
               mapMaybe (\(name,x) ->
                           do name' <- parsePackageNameFromString name
                              return (name',x)) .
               M.toList)
              (fmap (fromMaybe mempty)
                    (o .:? "package-flags"))
    where config docker pkgs maybedir flags pkgflags =
            \cwd ->
              Config docker
                     (S.map (\x ->
                               case x of
                                 Left () -> cwd
                                 Right p ->
                                   (cwd </> p))
                            pkgs)
                     maybedir
                     flags
                     pkgflags

-- | Defalt instance.
instance Default Config where
  def = Config {configDocker = def
               ,configPackages = def
               ,configMaybeDir = Nothing
               ,configFlags = mempty
               ,configPackageFlags = mempty}

-- | Newtype wrapper for 'FromJSON' instance that ignores non-Docker config.
newtype DockerOnlyConfig = DockerOnlyConfig {unDockerOnlyConfig :: Config}

-- | Default instance.
instance Default DockerOnlyConfig where
  def = DockerOnlyConfig def

-- | For YAML.  Only read the @docker@ map.
instance FromJSON (Path Abs Dir -> DockerOnlyConfig) where
  parseJSON v =
    do o <- parseJSON v
       config <$> o .:? dockerConfigFieldName .!= def <*> pure mempty <*>
         pure Nothing <*>
         pure mempty <*>
         pure mempty
    where config docker pkgs maybedir flags pkgflags =
            \cwd ->
              DockerOnlyConfig (Config docker (S.map (cwd </>) pkgs) maybedir flags pkgflags)

-- | Name of docker field in config file
dockerConfigFieldName :: Text
dockerConfigFieldName = "docker"

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
            ,gconfigFlags :: !(Map Text Bool)
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

-- | Docker configuration.
data Docker =
  Docker {dockerEnable :: !Bool
           -- ^ Is using Docker enabled?
         ,dockerRepoOwner :: !String
           -- ^ Docker repository (registry and) owner
         ,dockerRepo :: !String
           -- ^ Docker repository name (e.g. @dev@)
         ,dockerRepoSuffix :: !String
           -- ^ Docker repository name's suffix (e.g. stackage ver)
         ,dockerImageTag :: !(Maybe String)
           -- ^ Optional Docker image tag (e.g. the date)
         ,dockerImage :: !(Maybe String)
           -- ^ Exact Docker image tag or ID.  Overrides docker-repo-*/tag.
         ,dockerRegistryLogin :: !Bool
           -- ^ Does registry require login for pulls?
         ,dockerRegistryUsername :: !(Maybe String)
           -- ^ Optional username for Docker registry.
         ,dockerRegistryPassword :: !(Maybe String)
           -- ^ Optional password for Docker registry.
         ,dockerAutoPull :: !Bool
           -- ^ Automatically pull new images.
         ,dockerDetach :: !Bool
           -- ^ Whether to run a detached container
         ,dockerPersist :: !Bool
           -- ^ Create a persistent container (don't remove it when finished).  Implied by
           -- `dockerDetach`.
         ,dockerContainerName :: !(Maybe String)
           -- ^ Container name to use, only makes sense from command-line with `dockerPersist`
           -- or `dockerDetach`.
         ,dockerRunArgsDefault :: ![String]
           -- ^ Arguments to pass directly to @docker run@ (from @stackage-build.config@).
         ,dockerRunArgsExtra :: ![[String]]
           -- ^ Arguments to pass directly to @docker run@ (passed on stackage-build command-line).
         ,dockerMountDefault :: ![Mount]
           -- ^ Volumes to mount in the container (from @stackage-build.config@).
         ,dockerMountExtra :: ![Mount]
           -- ^ Volumes to mount in the container (from stackage-build command-line).
         ,dockerPassHost :: !Bool
           -- ^ Pass Docker daemon connection information into container.
         ,dockerExtra :: ![String]
           -- ^ This is a placeholder for command-line argument parsing.
         }
  deriving (Show)

-- | For YAML.
instance FromJSON Docker where
  parseJSON v =
    do o <- parseJSON v
       Docker <$> o .:? dockerEnableArgName .!= True
              <*> o .:? dockerRepoOwnerArgName .!= dockerRepoOwner def
              <*> o .:? dockerRepoArgName .!= dockerRepo def
              <*> o .:? dockerRepoSuffixArgName .!= dockerRepoSuffix def
              <*> o .:? dockerImageTagArgName .!= dockerImageTag def
              <*> o .:? dockerImageArgName
              <*> o .:? dockerRegistryLoginArgName .!= dockerRegistryLogin def
              <*> o .:? dockerRegistryUsernameArgName .!= dockerRegistryUsername def
              <*> o .:? dockerRegistryPasswordArgName .!= dockerRegistryPassword def
              <*> o .:? dockerAutoPullArgName .!= dockerAutoPull def
              <*> o .:? dockerDetachArgName .!= dockerDetach def
              <*> o .:? dockerPersistArgName .!= dockerPersist def
              <*> o .:? dockerContainerNameArgName .!= dockerContainerName def
              <*> o .:? dockerRunArgsArgName .!= dockerRunArgsDefault def
              <*> pure (dockerRunArgsExtra def)
              <*> o .:? dockerMountArgName .!= dockerMountDefault def
              <*> pure (dockerMountExtra def)
              <*> o .:? dockerPassHostArgName .!= dockerPassHost def
              <*> pure (dockerExtra def)

-- | Default values for Docker configuration.
instance Default Docker where
  def = Docker {dockerEnable = False
               ,dockerRepoOwner = "docker.fpcomplete.com/haskell"
               ,dockerRepo = "dev"
               ,dockerRepoSuffix = ""
               ,dockerImageTag = Nothing
               ,dockerImage = Nothing
               ,dockerRegistryLogin = False
               ,dockerRegistryUsername = Nothing
               ,dockerRegistryPassword = Nothing
               ,dockerAutoPull = False
               ,dockerDetach = False
               ,dockerPersist = False
               ,dockerContainerName = Nothing
               ,dockerRunArgsDefault = []
               ,dockerRunArgsExtra = []
               ,dockerMountDefault = []
               ,dockerMountExtra = []
               ,dockerPassHost = False
               ,dockerExtra = []}

dockerEnableArgName :: Text
dockerEnableArgName = "enable"

dockerRepoOwnerArgName :: Text
dockerRepoOwnerArgName = "repo-owner"

dockerRepoArgName :: Text
dockerRepoArgName = "repo"

dockerRepoSuffixArgName :: Text
dockerRepoSuffixArgName = "repo-suffix"

dockerImageTagArgName :: Text
dockerImageTagArgName = "image-tag"

dockerImageArgName :: Text
dockerImageArgName = "image"

dockerRegistryLoginArgName :: Text
dockerRegistryLoginArgName = "registry-login"

dockerRegistryUsernameArgName :: Text
dockerRegistryUsernameArgName = "registry-username"

dockerRegistryPasswordArgName :: Text
dockerRegistryPasswordArgName = "registry-password"

dockerAutoPullArgName :: Text
dockerAutoPullArgName = "auto-pull"

dockerDetachArgName :: Text
dockerDetachArgName = "detach"

dockerRunArgsArgName :: Text
dockerRunArgsArgName = "run-args"

dockerMountArgName :: Text
dockerMountArgName = "mount"

dockerContainerNameArgName :: Text
dockerContainerNameArgName = "container-name"

dockerPersistArgName :: Text
dockerPersistArgName = "persist"

dockerPassHostArgName :: Text
dockerPassHostArgName = "pass-host"

-- | Docker volume mount.
data Mount = Mount String String

-- | For optparse-applicative.
instance Read Mount where
  readsPrec _ s = case break (== ':') s of
                    (a,(':':b)) -> [(Mount a b,"")]
                    (a,[]) -> [(Mount a a,"")]
                    _ -> fail "Invalid value for mount"

-- | Show instance.
instance Show Mount where
  show (Mount a b) = if a == b
                        then a
                        else concat [a,":",b]

-- | For YAML.
instance FromJSON Mount where
  parseJSON v = fmap read (parseJSON v)

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
