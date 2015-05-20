{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The Config type.

module Stack.Types.Config where

import Control.Exception
import Control.Monad (liftM)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (MonadReader, ask)
import Data.Aeson (FromJSON, parseJSON, withText)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable
import Path
import Stack.Types.BuildPlan (SnapName, renderSnapName, parseSnapName)
import Stack.Types.Docker
import Stack.Types.FlagName
import Stack.Types.PackageIdentifier
import Stack.Types.PackageName
import Stack.Types.Version

-- | The top-level Stackage configuration.
data Config =
  Config {configStackRoot        :: !(Path Abs Dir)
         -- ^ ~/.stack more often than not
         ,configDocker           :: !(Maybe Docker)
         ,configPackagesPath     :: !(Set (Path Abs Dir))
         -- ^ Local packages identified by a path
         ,configPackagesIdent    :: !(Set PackageIdentifier)
         -- ^ Local packages identified by a package identifier
         ,configFlags            :: !(Map FlagName Bool) -- FIXME rename to global?
         ,configPackageFlags     :: !(Map PackageName (Map FlagName Bool))
         ,configDir              :: !(Path Abs Dir)
         -- ^ Directory containing the project's stack.yaml file
         ,configUrls             :: !(Map Text Text)
         ,configGpgVerifyIndex   :: !Bool
         ,configResolver         :: !Resolver
         -- ^ How we resolve which dependencies to install given a set of
         -- packages.
         ,configGhcVersion       :: !(Maybe Version)
         -- ^ Version of GHC we'll be using for this build, @Nothing@ if no
         -- preference
         ,configInstallDeps      :: !Bool
         -- ^ Whether or not dependencies should be installed. If @False@, any
         -- missing dependencies will result in a compilation failure. Useful
         -- to disable this flag, for example, when using a precompiled binary
         -- package database, such as via Docker.
         }
  -- ^ Flags for each package's Cabal config.
  deriving (Show)

-- | How we resolve which dependencies to install given a set of packages.
data Resolver
  = ResolverSnapshot SnapName
  -- ^ Use an official snapshot from the Stackage project, either an LTS
  -- Haskell or Stackage Nightly
  | ResolverNone
  -- ^ No way to resolve packages
  deriving (Show)

instance FromJSON Resolver where
    parseJSON = withText "Resolver" $
        either (fail . show) return . parseResolver

-- | Convert a Resolver into its @Text@ representation, as will be used by JSON/YAML
renderResolver :: Resolver -> Text
renderResolver (ResolverSnapshot name) = renderSnapName name

-- | Try to parse a @Resolver@, using same format as JSON/YAML/'renderResolver'
parseResolver :: MonadThrow m => Text -> m Resolver
parseResolver = liftM ResolverSnapshot . parseSnapName

-- | Class for environment values which have access to the stack root
class HasStackRoot env where
    getStackRoot :: env -> Path Abs Dir
    default getStackRoot :: HasConfig env => env -> Path Abs Dir
    getStackRoot = configStackRoot . getConfig

-- | Class for environment values which have access to the URLs
class HasUrls env where
    getUrls :: env -> Map Text Text
    default getUrls :: HasConfig env => env -> Map Text Text
    getUrls = configUrls . getConfig

-- | Class for environment values that can provide a 'Config'.
class (HasStackRoot env, HasUrls env) => HasConfig env where
    getConfig :: env -> Config
instance HasStackRoot Config
instance HasUrls Config
instance HasConfig Config where
    getConfig = id
    {-# INLINE getConfig #-}

data ConfigException
  = ConfigInvalidYaml String
  | ConfigNoFile
  | ConfigNoDockerConfig
  deriving (Typeable,Show)
instance Exception ConfigException

-- TODO: eliminate occurrences of this exception.
data NotYetImplemented = NotYetImplemented Text
  deriving (Show, Typeable)
instance Exception NotYetImplemented

-- | Helper function to ask the environment and apply getConfig
askConfig :: (MonadReader env m, HasConfig env) => m Config
askConfig = liftM getConfig ask

-- | Helper for looking up URLs
askUrl :: (MonadReader env m, HasUrls env)
       => Text -- ^ key
       -> Text -- ^ default
       -> m Text
askUrl key val = liftM (fromMaybe val . Map.lookup key . getUrls) ask

-- | Get the URL to request the information on the latest snapshots
askLatestSnapshotUrl :: (MonadReader env m, HasUrls env) => m Text
askLatestSnapshotUrl = askUrl "latest-snapshot-url" "https://www.stackage.org/download/snapshots.json"

-- | Git URL for the package index
askPackageIndexGitUrl :: (MonadReader env m, HasUrls env) => m Text
askPackageIndexGitUrl = askUrl "package-index-git-url" "https://github.com/commercialhaskell/all-cabal-hashes.git"

-- | HTTP URL for the package index
askPackageIndexHttpUrl :: (MonadReader env m, HasUrls env) => m Text
askPackageIndexHttpUrl = askUrl "package-index-http-url" "https://s3.amazonaws.com/hackage.fpcomplete.com/00-index.tar.gz"

-- | Location of the 00-index.cache file
configPackageIndexCache :: Config -> Path Abs File
configPackageIndexCache config = configStackRoot config </> $(mkRelFile "00-index.cache")

-- | Location of the 00-index.tar file
configPackageIndex :: Config -> Path Abs File
configPackageIndex config = configStackRoot config </> $(mkRelFile "00-index.tar")

-- | Location of the 00-index.tar.gz file
configPackageIndexGz :: Config -> Path Abs File
configPackageIndexGz config = configStackRoot config </> $(mkRelFile "00-index.tar.gz")

-- | Location of a package tarball
configPackageTarball :: MonadThrow m => Config -> PackageIdentifier -> m (Path Abs File)
configPackageTarball config ident = do
    name <- parseRelDir $ packageNameString $ packageIdentifierName ident
    ver <- parseRelDir $ versionString $ packageIdentifierVersion ident
    base <- parseRelFile $ packageIdentifierString ident ++ ".tar.gz"
    return $ configStackRoot config </> $(mkRelDir "packages") </> name </> ver </> base
