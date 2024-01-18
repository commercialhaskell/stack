{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Docker types.

module Stack.Types.Docker
  ( DockerException (..)
  , DockerMonoidRepoOrImage (..)
  , DockerOpts (..)
  , DockerOptsMonoid (..)
  , DockerStackExe (..)
  , Mount (..)
  , VersionRangeJSON (..)
  , dockerAutoPullArgName
  , dockerCmdName
  , dockerContainerNameArgName
  , dockerContainerPlatform
  , dockerDetachArgName
  , dockerEnableArgName
  , dockerEntrypointArgName
  , dockerEnvArgName
  , dockerHelpOptName
  , dockerImageArgName
  , dockerMountArgName
  , dockerMountModeArgName
  , dockerNetworkArgName
  , dockerPersistArgName
  , dockerPullCmdName
  , dockerRegistryLoginArgName
  , dockerRegistryPasswordArgName
  , dockerRegistryUsernameArgName
  , dockerRepoArgName
  , dockerRequireDockerVersionArgName
  , dockerRunArgsArgName
  , dockerSetUserArgName
  , dockerStackExeArgName
  , dockerStackExeDownloadVal
  , dockerStackExeHostVal
  , dockerStackExeImageVal
  , parseDockerStackExe
  , reExecArgName
  ) where

import           Data.Aeson.Types ( FromJSON (..), withText )
import           Data.Aeson.WarningParser
                   ( WithJSONWarnings, (..:), (..:?), (..!=), withObjectWarnings
                   )
import           Data.List ( intercalate )
import qualified Data.Text as T
import           Distribution.System ( Arch (..), OS (..), Platform (..) )
import           Distribution.Text ( display, simpleParse )
import           Distribution.Version ( anyVersion )
import           Generics.Deriving.Monoid ( mappenddefault, memptydefault )
import           Path ( parseAbsFile )
import           Stack.Constants ( stackProgName )
import           Stack.Prelude hiding ( Display (..) )
import           Stack.Types.Version
                   ( IntersectingVersionRange (..), VersionRange
                   , versionRangeText
                   )
import           Text.Read ( Read (..) )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Docker" module.
data DockerException
  = DockerMustBeEnabledException
    -- ^ Docker must be enabled to use the command.
  | OnlyOnHostException
    -- ^ Command must be run on host OS (not in a container).
  | InspectFailedException String
    -- ^ @docker inspect@ failed.
  | NotPulledException String
    -- ^ Image does not exist.
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
  deriving (Show, Typeable)

instance Exception DockerException where
  displayException DockerMustBeEnabledException =
    "Error: [S-3223]\n"
    ++ "Docker must be enabled in your configuration file to use this \
       \command."
  displayException OnlyOnHostException =
    "Error: [S-9779]\n"
    ++ "This command must be run on host OS (not in a Docker container)."
  displayException (InspectFailedException image) = concat
    [ "Error: [S-9105]\n"
    , "'docker inspect' failed for image after pull: "
    , image
    , "."
    ]
  displayException (NotPulledException image) = concat
    [ "Error: [S-6626]\n"
    , "The Docker image referenced by your configuration file"
    , " has not\nbeen downloaded:\n    "
    , image
    , "\n\nRun '"
    , unwords [stackProgName, dockerCmdName, dockerPullCmdName]
    , "' to download it, then try again."
    ]
  displayException (InvalidImagesOutputException l) = concat
    [ "Error: [S-5841]\n"
    , "Invalid 'docker images' output line: '"
    , l
    , "'."
    ]
  displayException (InvalidPSOutputException l) = concat
    [ "Error: [S-9608]\n"
    , "Invalid 'docker ps' output line: '"
    , l
    ,"'."
    ]
  displayException (InvalidInspectOutputException msg) = concat
    [ "Error: [S-2240]\n"
    , "Invalid 'docker inspect' output: "
    , msg
    , "."
    ]
  displayException (PullFailedException image) = concat
    [ "Error: [S-6092]\n"
    , "Could not pull Docker image:\n    "
    , image
    , "\nThere may not be an image on the registry for your resolver's LTS \
      \version in\n"
    , "your configuration file."
    ]
  displayException (DockerTooOldException minVersion haveVersion) = concat
    [ "Error: [S-6281]\n"
    , "Minimum docker version '"
    , versionString minVersion
    , "' is required by "
    , stackProgName
    , " (you have '"
    , versionString haveVersion
    , "')."
    ]
  displayException (DockerVersionProhibitedException prohibitedVersions haveVersion) = concat
    [ "Error: [S-8252]\n"
    , "These Docker versions are incompatible with "
    , stackProgName
    , " (you have '"
    , versionString haveVersion
    , "'): "
    , intercalate ", " (map versionString prohibitedVersions)
    , "."
    ]
  displayException (BadDockerVersionException requiredRange haveVersion) = concat
    [ "Error: [S-6170]\n"
    , "The version of 'docker' you are using ("
    , show haveVersion
    , ") is outside the required\n"
    , "version range specified in stack.yaml ("
    , T.unpack (versionRangeText requiredRange)
    , ")."
    ]
  displayException InvalidVersionOutputException =
    "Error: [S-5827]\n"
    ++ "Cannot get Docker version (invalid 'docker --version' output)."
  displayException (HostStackTooOldException minVersion (Just hostVersion)) = concat
    [ "Error: [S-7112]\n"
    , "The host's version of '"
    , stackProgName
    , "' is too old for this Docker image.\nVersion "
    , versionString minVersion
    , " is required; you have "
    , versionString hostVersion
    , "."
    ]
  displayException (HostStackTooOldException minVersion Nothing) = concat
    [ "Error: [S-7112]\n"
    , "The host's version of '"
    , stackProgName
    , "' is too old.\nVersion "
    , versionString minVersion
    , " is required."
    ]
  displayException (ContainerStackTooOldException requiredVersion containerVersion) = concat
    [ "Error: [S-5832]\n"
    , "The Docker container's version of '"
    , stackProgName
    , "' is too old.\nVersion "
    , versionString requiredVersion
    , " is required; the container has "
    , versionString containerVersion
    , "."
    ]
  displayException CannotDetermineProjectRootException =
    "Error: [S-4078]\n"
    ++ "Cannot determine project root directory for Docker sandbox."
  displayException DockerNotInstalledException =
    "Error: [S-7058]\n"
    ++ "Cannot find 'docker' in PATH.  Is Docker installed?"
  displayException UnsupportedStackExeHostPlatformException = concat
    [ "Error: [S-6894]\n"
    , "Using host's "
    , stackProgName
    , " executable in Docker container is only supported on "
    , display dockerContainerPlatform
    , " platform."
    ]
  displayException (DockerStackExeParseException s) = concat
    [ "Error: [S-1512]\n"
    , "Failed to parse "
    , show s
    , ". Expected "
    , show dockerStackExeDownloadVal
    , ", "
    , show dockerStackExeHostVal
    , ", "
    , show dockerStackExeImageVal
    , " or absolute path to executable."
    ]

-- | Docker configuration.
data DockerOpts = DockerOpts
  { enable :: !Bool
     -- ^ Is using Docker enabled?
  , image :: !(Either SomeException String)
     -- ^ Exact Docker image tag or ID.  Overrides docker-repo-*/tag.
  , registryLogin :: !Bool
     -- ^ Does registry require login for pulls?
  , registryUsername :: !(Maybe String)
     -- ^ Optional username for Docker registry.
  , registryPassword :: !(Maybe String)
     -- ^ Optional password for Docker registry.
  , autoPull :: !Bool
     -- ^ Automatically pull new images.
  , detach :: !Bool
     -- ^ Whether to run a detached container
  , persist :: !Bool
     -- ^ Create a persistent container (don't remove it when finished). Implied
     -- by `dockerDetach`.
  , containerName :: !(Maybe String)
     -- ^ Container name to use, only makes sense from command-line with
     -- `dockerPersist` or `dockerDetach`.
  , network :: !(Maybe String)
    -- ^ The network docker uses.
  , runArgs :: ![String]
     -- ^ Arguments to pass directly to @docker run@.
  , mount :: ![Mount]
     -- ^ Volumes to mount in the container.
  , mountMode :: !(Maybe String)
     -- ^ Volume mount mode
  , env :: ![String]
     -- ^ Environment variables to set in the container.
  , stackExe :: !(Maybe DockerStackExe)
     -- ^ Location of container-compatible Stack executable
  , setUser :: !(Maybe Bool)
    -- ^ Set in-container user to match host's
  , requireDockerVersion :: !VersionRange
    -- ^ Require a version of Docker within this range.
  }
  deriving Show

-- | An uninterpreted representation of docker options. Configurations may be
-- "cascaded" using mappend (left-biased).
data DockerOptsMonoid = DockerOptsMonoid
  { defaultEnable :: !Any
    -- ^ Should Docker be defaulted to enabled (does @docker:@ section exist in
    -- the config)?
  , enable :: !(First Bool)
    -- ^ Is using Docker enabled?
  , repoOrImage :: !(First DockerMonoidRepoOrImage)
    -- ^ Docker repository name (e.g. @fpco/stack-build@ or
    -- @fpco/stack-full:lts-2.8@)
  , registryLogin :: !(First Bool)
    -- ^ Does registry require login for pulls?
  , registryUsername :: !(First String)
    -- ^ Optional username for Docker registry.
  , registryPassword :: !(First String)
    -- ^ Optional password for Docker registry.
  , autoPull :: !FirstTrue
    -- ^ Automatically pull new images.
  , detach :: !FirstFalse
    -- ^ Whether to run a detached container
  , persist :: !FirstFalse
    -- ^ Create a persistent container (don't remove it when finished). Implied
    -- by -- `dockerDetach`.
  , containerName :: !(First String)
    -- ^ Container name to use, only makes sense from command-line with
    -- `dockerPersist` or `dockerDetach`.
  , network :: !(First String)
    -- ^ See: 'dockerNetwork'
  , runArgs :: ![String]
    -- ^ Arguments to pass directly to @docker run@
  , mount :: ![Mount]
    -- ^ Volumes to mount in the container
  , mountMode :: !(First String)
    -- ^ Volume mount mode
  , env :: ![String]
    -- ^ Environment variables to set in the container
  , stackExe :: !(First DockerStackExe)
    -- ^ Location of container-compatible Stack executable
  , setUser :: !(First Bool)
    -- ^ Set in-container user to match host's
  , requireDockerVersion :: !IntersectingVersionRange
    -- ^ See: 'dockerRequireDockerVersion'
  }
  deriving (Show, Generic)

-- | Decode uninterpreted docker options from JSON/YAML.
instance FromJSON (WithJSONWarnings DockerOptsMonoid) where
  parseJSON = withObjectWarnings "DockerOptsMonoid" $ \o -> do
    let defaultEnable = Any True
    enable <- First <$> o ..:? dockerEnableArgName
    repoOrImage <- First <$>
      (   (Just . DockerMonoidImage <$> o ..: dockerImageArgName)
      <|> (Just . DockerMonoidRepo <$> o ..: dockerRepoArgName)
      <|> pure Nothing
      )
    registryLogin <- First <$> o ..:? dockerRegistryLoginArgName
    registryUsername <-
      First <$> o ..:? dockerRegistryUsernameArgName
    registryPassword <-
      First <$> o ..:? dockerRegistryPasswordArgName
    autoPull <- FirstTrue <$> o ..:? dockerAutoPullArgName
    detach <- FirstFalse <$> o ..:? dockerDetachArgName
    persist <- FirstFalse <$> o ..:? dockerPersistArgName
    containerName <- First <$> o ..:? dockerContainerNameArgName
    network <- First <$> o ..:? dockerNetworkArgName
    runArgs <- o ..:? dockerRunArgsArgName ..!= []
    mount <- o ..:? dockerMountArgName ..!= []
    mountMode <- First <$> o ..:? dockerMountModeArgName
    env <- o ..:? dockerEnvArgName ..!= []
    stackExe <- First <$> o ..:? dockerStackExeArgName
    setUser <- First <$> o ..:? dockerSetUserArgName
    requireDockerVersion <-
      IntersectingVersionRange . (.unVersionRangeJSON) <$>
        ( o ..:? dockerRequireDockerVersionArgName
          ..!= VersionRangeJSON anyVersion
        )
    pure DockerOptsMonoid
      { defaultEnable
      , enable
      , repoOrImage
      , registryLogin
      , registryUsername
      , registryPassword
      , autoPull
      , detach
      , persist
      , containerName
      , network
      , runArgs
      , mount
      , mountMode
      , env
      , stackExe
      , setUser
      , requireDockerVersion
      }

-- | Left-biased combine Docker options
instance Semigroup DockerOptsMonoid where
  (<>) = mappenddefault

-- | Left-biased combine Docker options
instance Monoid DockerOptsMonoid where
  mempty = memptydefault
  mappend = (<>)

-- | Where to get the `stack` executable to run in Docker containers
data DockerStackExe
  = DockerStackExeDownload  -- ^ Download from official bindist
  | DockerStackExeHost  -- ^ Host's `stack` (linux-x86_64 only)
  | DockerStackExeImage  -- ^ Docker image's `stack` (versions must match)
  | DockerStackExePath (Path Abs File) -- ^ Executable at given path
  deriving Show

instance FromJSON DockerStackExe where
  parseJSON a = do
    s <- parseJSON a
    case parseDockerStackExe s of
      Right dse -> pure dse
      Left e -> fail (displayException e)

-- | Parse 'DockerStackExe'.
parseDockerStackExe :: (MonadThrow m) => String -> m DockerStackExe
parseDockerStackExe t
  | t == dockerStackExeDownloadVal = pure DockerStackExeDownload
  | t == dockerStackExeHostVal = pure DockerStackExeHost
  | t == dockerStackExeImageVal = pure DockerStackExeImage
  | otherwise = case parseAbsFile t of
      Just p -> pure (DockerStackExePath p)
      Nothing -> throwM (DockerStackExeParseException t)

-- | Docker volume mount.
data Mount = Mount String String

-- | For optparse-applicative.
instance Read Mount where
  readsPrec _ s =
    case break (== ':') s of
      (a, ':':b) -> [(Mount a b, "")]
      (a, []) -> [(Mount a a, "")]
      _ -> fail "Invalid value for Docker mount (expect '/host/path:/container/path')"

-- | Show instance.
instance Show Mount where
  show (Mount a b) = if a == b then a else concat [a, ":", b]

-- | For YAML.
instance FromJSON Mount where
  parseJSON v = do
    s <- parseJSON v
    case readMaybe s of
      Nothing -> fail $ "Mount read failed: " ++ s
      Just x -> pure x

-- | Options for Docker repository or image.
data DockerMonoidRepoOrImage
  = DockerMonoidRepo String
  | DockerMonoidImage String
  deriving Show

-- | Newtype for non-orphan FromJSON instance.
newtype VersionRangeJSON =
  VersionRangeJSON { unVersionRangeJSON :: VersionRange }

-- | Parse VersionRange.
instance FromJSON VersionRangeJSON where
  parseJSON = withText "VersionRange"
    (\s -> maybe (fail ("Invalid cabal-style VersionRange: " ++ T.unpack s))
                 (pure . VersionRangeJSON)
                 (Distribution.Text.simpleParse (T.unpack s)))

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

-- | Docker mount mode argument name.
dockerMountModeArgName :: Text
dockerMountModeArgName = "mount-mode"

-- | Docker environment variable argument name.
dockerEnvArgName :: Text
dockerEnvArgName = "env"

-- | Docker container name argument name.
dockerContainerNameArgName :: Text
dockerContainerNameArgName = "container-name"
--
-- | Docker container name argument name.
dockerNetworkArgName :: Text
dockerNetworkArgName = "network"

-- | Docker persist argument name.
dockerPersistArgName :: Text
dockerPersistArgName = "persist"

-- | Docker Stack executable argument name.
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

-- | Command-line option for @--internal-re-exec-version@.
reExecArgName :: String
reExecArgName = "internal-re-exec-version"

-- | Platform that Docker containers run
dockerContainerPlatform :: Platform
dockerContainerPlatform = Platform X86_64 Linux
