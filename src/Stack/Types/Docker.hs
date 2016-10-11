{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Docker types.

module Stack.Types.Docker where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson.Extended
import Data.List (intercalate)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Distribution.System (Platform(..), OS(..), Arch(..))
import Distribution.Text (simpleParse, display)
import Distribution.Version (anyVersion)
import GHC.Generics (Generic)
import Generics.Deriving.Monoid (mappenddefault, memptydefault)
import Path
import {-# SOURCE #-} Stack.Constants
import Stack.Types.Version

-- | Docker configuration.
data DockerOpts = DockerOpts
  {dockerEnable :: !Bool
    -- ^ Is using Docker enabled?
  ,dockerImage :: !String
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
  ,dockerRunArgs :: ![String]
    -- ^ Arguments to pass directly to @docker run@.
  ,dockerMount :: ![Mount]
    -- ^ Volumes to mount in the container.
  ,dockerEnv :: ![String]
    -- ^ Environment variables to set in the container.
  ,dockerDatabasePath :: !(Path Abs File)
    -- ^ Location of image usage database.
  ,dockerStackExe :: !(Maybe DockerStackExe)
    -- ^ Location of container-compatible stack executable
  ,dockerSetUser :: !(Maybe Bool)
   -- ^ Set in-container user to match host's
  ,dockerRequireDockerVersion :: !VersionRange
   -- ^ Require a version of Docker within this range.
  }
  deriving (Show)

-- | An uninterpreted representation of docker options.
-- Configurations may be "cascaded" using mappend (left-biased).
data DockerOptsMonoid = DockerOptsMonoid
  {dockerMonoidDefaultEnable :: !Any
    -- ^ Should Docker be defaulted to enabled (does @docker:@ section exist in the config)?
  ,dockerMonoidEnable :: !(First Bool)
    -- ^ Is using Docker enabled?
  ,dockerMonoidRepoOrImage :: !(First DockerMonoidRepoOrImage)
    -- ^ Docker repository name (e.g. @fpco/stack-build@ or @fpco/stack-full:lts-2.8@)
  ,dockerMonoidRegistryLogin :: !(First Bool)
    -- ^ Does registry require login for pulls?
  ,dockerMonoidRegistryUsername :: !(First String)
    -- ^ Optional username for Docker registry.
  ,dockerMonoidRegistryPassword :: !(First String)
    -- ^ Optional password for Docker registry.
  ,dockerMonoidAutoPull :: !(First Bool)
    -- ^ Automatically pull new images.
  ,dockerMonoidDetach :: !(First Bool)
    -- ^ Whether to run a detached container
  ,dockerMonoidPersist :: !(First Bool)
    -- ^ Create a persistent container (don't remove it when finished).  Implied by
    -- `dockerDetach`.
  ,dockerMonoidContainerName :: !(First String)
    -- ^ Container name to use, only makes sense from command-line with `dockerPersist`
    -- or `dockerDetach`.
  ,dockerMonoidRunArgs :: ![String]
    -- ^ Arguments to pass directly to @docker run@
  ,dockerMonoidMount :: ![Mount]
    -- ^ Volumes to mount in the container
  ,dockerMonoidEnv :: ![String]
    -- ^ Environment variables to set in the container
  ,dockerMonoidDatabasePath :: !(First (Path Abs File))
    -- ^ Location of image usage database.
  ,dockerMonoidStackExe :: !(First DockerStackExe)
    -- ^ Location of container-compatible stack executable
  ,dockerMonoidSetUser :: !(First Bool)
   -- ^ Set in-container user to match host's
  ,dockerMonoidRequireDockerVersion :: !IntersectingVersionRange
  -- ^ See: 'dockerRequireDockerVersion'
  }
  deriving (Show, Generic)

-- | Decode uninterpreted docker options from JSON/YAML.
instance FromJSON (WithJSONWarnings DockerOptsMonoid) where
  parseJSON = withObjectWarnings "DockerOptsMonoid"
    (\o -> do dockerMonoidDefaultEnable    <- pure (Any True)
              dockerMonoidEnable           <- First <$> o ..:? dockerEnableArgName
              dockerMonoidRepoOrImage      <- First <$>
                                              (((Just . DockerMonoidImage) <$> o ..: dockerImageArgName) <|>
                                              ((Just . DockerMonoidRepo) <$> o ..: dockerRepoArgName) <|>
                                              pure Nothing)
              dockerMonoidRegistryLogin    <- First <$> o ..:? dockerRegistryLoginArgName
              dockerMonoidRegistryUsername <- First <$> o ..:? dockerRegistryUsernameArgName
              dockerMonoidRegistryPassword <- First <$> o ..:? dockerRegistryPasswordArgName
              dockerMonoidAutoPull         <- First <$> o ..:? dockerAutoPullArgName
              dockerMonoidDetach           <- First <$> o ..:? dockerDetachArgName
              dockerMonoidPersist          <- First <$> o ..:? dockerPersistArgName
              dockerMonoidContainerName    <- First <$> o ..:? dockerContainerNameArgName
              dockerMonoidRunArgs          <- o ..:? dockerRunArgsArgName ..!= []
              dockerMonoidMount            <- o ..:? dockerMountArgName ..!= []
              dockerMonoidEnv              <- o ..:? dockerEnvArgName ..!= []
              dockerMonoidDatabasePath     <- First <$> o ..:? dockerDatabasePathArgName
              dockerMonoidStackExe         <- First <$> o ..:? dockerStackExeArgName
              dockerMonoidSetUser          <- First <$> o ..:? dockerSetUserArgName
              dockerMonoidRequireDockerVersion
                                           <- IntersectingVersionRange . unVersionRangeJSON <$> (
                                                 o ..:? dockerRequireDockerVersionArgName
                                                   ..!= VersionRangeJSON anyVersion)
              return DockerOptsMonoid{..})

-- | Left-biased combine Docker options
instance Monoid DockerOptsMonoid where
  mempty = memptydefault
  mappend = mappenddefault

-- | Where to get the `stack` executable to run in Docker containers
data DockerStackExe
    = DockerStackExeDownload  -- ^ Download from official bindist
    | DockerStackExeHost  -- ^ Host's `stack` (linux-x86_64 only)
    | DockerStackExeImage  -- ^ Docker image's `stack` (versions must match)
    | DockerStackExePath (Path Abs File) -- ^ Executable at given path
    deriving (Show)

instance FromJSON DockerStackExe where
    parseJSON a = do
        s <- parseJSON a
        case parseDockerStackExe s of
            Right dse -> return dse
            Left e -> fail (show e)

-- | Parse 'DockerStackExe'.
parseDockerStackExe :: (MonadThrow m) => String -> m DockerStackExe
parseDockerStackExe t
    | t == dockerStackExeDownloadVal = return DockerStackExeDownload
    | t == dockerStackExeHostVal = return DockerStackExeHost
    | t == dockerStackExeImageVal = return DockerStackExeImage
    | otherwise = case parseAbsFile t of
        Just p -> return (DockerStackExePath p)
        Nothing -> throwM (DockerStackExeParseException t)

-- | Docker volume mount.
data Mount = Mount String String

-- | For optparse-applicative.
instance Read Mount where
  readsPrec _ s =
    case break (== ':') s of
      (a,':':b) -> [(Mount a b,"")]
      (a,[]) -> [(Mount a a,"")]
      _ -> fail "Invalid value for Docker mount (expect '/host/path:/container/path')"

-- | Show instance.
instance Show Mount where
  show (Mount a b) = if a == b
                        then a
                        else concat [a,":",b]

-- | For YAML.
instance FromJSON Mount where
  parseJSON v = fmap read (parseJSON v)

-- | Options for Docker repository or image.
data DockerMonoidRepoOrImage
  = DockerMonoidRepo String
  | DockerMonoidImage String
  deriving (Show)

-- | Newtype for non-orphan FromJSON instance.
newtype VersionRangeJSON = VersionRangeJSON { unVersionRangeJSON :: VersionRange }

-- | Parse VersionRange.
instance FromJSON VersionRangeJSON where
  parseJSON = withText "VersionRange"
                (\s -> maybe (fail ("Invalid cabal-style VersionRange: " ++ T.unpack s))
                             (return . VersionRangeJSON)
                             (Distribution.Text.simpleParse (T.unpack s)))

-- | Exceptions thrown by Stack.Docker.
data StackDockerException
    = DockerMustBeEnabledException
      -- ^ Docker must be enabled to use the command.
    | OnlyOnHostException
      -- ^ Command must be run on host OS (not in a container).
    | InspectFailedException String
      -- ^ @docker inspect@ failed.
    | NotPulledException String
      -- ^ Image does not exist.
    | InvalidCleanupCommandException String
      -- ^ Input to @docker cleanup@ has invalid command.
    | InvalidImagesOutputException String
      -- ^ Invalid output from @docker images@.
    | InvalidPSOutputException String
      -- ^ Invalid output from @docker ps@.
    | InvalidInspectOutputException String
      -- ^ Invalid output from @docker inspect@.
    | PullFailedException String
      -- ^ Could not pull a Docker image.
    | DockerTooOldException Version Version
      -- ^ Installed version of @docker@ below minimum version.
    | DockerVersionProhibitedException [Version] Version
      -- ^ Installed version of @docker@ is prohibited.
    | BadDockerVersionException VersionRange Version
      -- ^ Installed version of @docker@ is out of range specified in config file.
    | InvalidVersionOutputException
      -- ^ Invalid output from @docker --version@.
    | HostStackTooOldException Version (Maybe Version)
      -- ^ Version of @stack@ on host is too old for version in image.
    | ContainerStackTooOldException Version Version
      -- ^ Version of @stack@ in container/image is too old for version on host.
    | CannotDetermineProjectRootException
      -- ^ Can't determine the project root (where to put docker sandbox).
    | DockerNotInstalledException
      -- ^ @docker --version@ failed.
    | UnsupportedStackExeHostPlatformException
      -- ^ Using host stack-exe on unsupported platform.
    | DockerStackExeParseException String
      -- ^ @stack-exe@ option fails to parse.
    deriving (Typeable)
instance Exception StackDockerException

instance Show StackDockerException where
    show DockerMustBeEnabledException =
        "Docker must be enabled in your configuration file to use this command."
    show OnlyOnHostException =
        "This command must be run on host OS (not in a Docker container)."
    show (InspectFailedException image) =
        concat ["'docker inspect' failed for image after pull: ",image,"."]
    show (NotPulledException image) =
        concat ["The Docker image referenced by your configuration file"
               ," has not\nbeen downloaded:\n    "
               ,image
               ,"\n\nRun '"
               ,unwords [stackProgName, dockerCmdName, dockerPullCmdName]
               ,"' to download it, then try again."]
    show (InvalidCleanupCommandException line) =
        concat ["Invalid line in cleanup commands: '",line,"'."]
    show (InvalidImagesOutputException line) =
        concat ["Invalid 'docker images' output line: '",line,"'."]
    show (InvalidPSOutputException line) =
        concat ["Invalid 'docker ps' output line: '",line,"'."]
    show (InvalidInspectOutputException msg) =
        concat ["Invalid 'docker inspect' output: ",msg,"."]
    show (PullFailedException image) =
        concat ["Could not pull Docker image:\n    "
               ,image
               ,"\nThere may not be an image on the registry for your resolver's LTS version in\n"
               ,"your configuration file."]
    show (DockerTooOldException minVersion haveVersion) =
        concat ["Minimum docker version '"
               ,versionString minVersion
               ,"' is required by "
               ,stackProgName
               ," (you have '"
               ,versionString haveVersion
               ,"')."]
    show (DockerVersionProhibitedException prohibitedVersions haveVersion) =
        concat ["These Docker versions are incompatible with "
               ,stackProgName
               ," (you have '"
               ,versionString haveVersion
               ,"'): "
               ,intercalate ", " (map versionString prohibitedVersions)
               ,"."]
    show (BadDockerVersionException requiredRange haveVersion) =
        concat ["The version of 'docker' you are using ("
               ,show haveVersion
               ,") is outside the required\n"
               ,"version range specified in stack.yaml ("
               ,T.unpack (versionRangeText requiredRange)
               ,")."]
    show InvalidVersionOutputException =
        "Cannot get Docker version (invalid 'docker --version' output)."
    show (HostStackTooOldException minVersion (Just hostVersion)) =
        concat ["The host's version of '"
               ,stackProgName
               ,"' is too old for this Docker image.\nVersion "
               ,versionString minVersion
               ," is required; you have "
               ,versionString hostVersion
               ,"."]
    show (HostStackTooOldException minVersion Nothing) =
        concat ["The host's version of '"
               ,stackProgName
               ,"' is too old.\nVersion "
               ,versionString minVersion
               ," is required."]
    show (ContainerStackTooOldException requiredVersion containerVersion) =
        concat ["The Docker container's version of '"
               ,stackProgName
               ,"' is too old.\nVersion "
               ,versionString requiredVersion
               ," is required; the container has "
               ,versionString containerVersion
               ,"."]
    show CannotDetermineProjectRootException =
        "Cannot determine project root directory for Docker sandbox."
    show DockerNotInstalledException =
        "Cannot find 'docker' in PATH.  Is Docker installed?"
    show UnsupportedStackExeHostPlatformException = concat
        [ "Using host's "
        , stackProgName
        , " executable in Docker container is only supported on "
        , display dockerContainerPlatform
        , " platform" ]
    show (DockerStackExeParseException s) = concat
        [ "Failed to parse "
        , show s
        , ". Expected "
        , show dockerStackExeDownloadVal
        , ", "
        , show dockerStackExeHostVal
        , ", "
        , show dockerStackExeImageVal
        , " or absolute path to executable."
        ]

-- | Docker enable argument name.
dockerEnableArgName :: Text
dockerEnableArgName = "enable"

-- | Docker repo arg argument name.
dockerRepoArgName :: Text
dockerRepoArgName = "repo"

-- | Docker image argument name.
dockerImageArgName :: Text
dockerImageArgName = "image"

-- | Docker registry login argument name.
dockerRegistryLoginArgName :: Text
dockerRegistryLoginArgName = "registry-login"

-- | Docker registry username argument name.
dockerRegistryUsernameArgName :: Text
dockerRegistryUsernameArgName = "registry-username"

-- | Docker registry password argument name.
dockerRegistryPasswordArgName :: Text
dockerRegistryPasswordArgName = "registry-password"

-- | Docker auto-pull argument name.
dockerAutoPullArgName :: Text
dockerAutoPullArgName = "auto-pull"

-- | Docker detach argument name.
dockerDetachArgName :: Text
dockerDetachArgName = "detach"

-- | Docker run args argument name.
dockerRunArgsArgName :: Text
dockerRunArgsArgName = "run-args"

-- | Docker mount argument name.
dockerMountArgName :: Text
dockerMountArgName = "mount"

-- | Docker environment variable argument name.
dockerEnvArgName :: Text
dockerEnvArgName = "env"

-- | Docker container name argument name.
dockerContainerNameArgName :: Text
dockerContainerNameArgName = "container-name"

-- | Docker persist argument name.
dockerPersistArgName :: Text
dockerPersistArgName = "persist"

-- | Docker database path argument name.
dockerDatabasePathArgName :: Text
dockerDatabasePathArgName = "database-path"

-- | Docker database path argument name.
dockerStackExeArgName :: Text
dockerStackExeArgName = "stack-exe"

-- | Value for @--docker-stack-exe=download@
dockerStackExeDownloadVal :: String
dockerStackExeDownloadVal = "download"

-- | Value for @--docker-stack-exe=host@
dockerStackExeHostVal :: String
dockerStackExeHostVal = "host"

-- | Value for @--docker-stack-exe=image@
dockerStackExeImageVal :: String
dockerStackExeImageVal = "image"

-- | Docker @set-user@ argument name
dockerSetUserArgName :: Text
dockerSetUserArgName = "set-user"

-- | Docker @require-version@ argument name
dockerRequireDockerVersionArgName :: Text
dockerRequireDockerVersionArgName = "require-docker-version"

-- | Argument name used to pass docker entrypoint data (only used internally)
dockerEntrypointArgName :: String
dockerEntrypointArgName = "internal-docker-entrypoint"

-- | Command-line argument for "docker"
dockerCmdName :: String
dockerCmdName = "docker"

dockerHelpOptName :: String
dockerHelpOptName = dockerCmdName ++ "-help"

-- | Command-line argument for @docker pull@.
dockerPullCmdName :: String
dockerPullCmdName = "pull"

-- | Command-line argument for @docker cleanup@.
dockerCleanupCmdName :: String
dockerCleanupCmdName = "cleanup"

-- | Command-line option for @--internal-re-exec-version@.
reExecArgName :: String
reExecArgName = "internal-re-exec-version"

-- | Platform that Docker containers run
dockerContainerPlatform :: Platform
dockerContainerPlatform = Platform X86_64 Linux
