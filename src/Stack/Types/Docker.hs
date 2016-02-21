{-# LANGUAGE OverloadedStrings, FlexibleInstances, RecordWildCards #-}

-- | Docker types.

module Stack.Types.Docker where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Data.Aeson.Extended
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Distribution.Text (simpleParse)
import Distribution.Version (anyVersion)
import Path
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
  {dockerMonoidDefaultEnable :: !Bool
    -- ^ Should Docker be defaulted to enabled (does @docker:@ section exist in the config)?
  ,dockerMonoidEnable :: !(Maybe Bool)
    -- ^ Is using Docker enabled?
  ,dockerMonoidRepoOrImage :: !(Maybe DockerMonoidRepoOrImage)
    -- ^ Docker repository name (e.g. @fpco/stack-build@ or @fpco/stack-full:lts-2.8@)
  ,dockerMonoidRegistryLogin :: !(Maybe Bool)
    -- ^ Does registry require login for pulls?
  ,dockerMonoidRegistryUsername :: !(Maybe String)
    -- ^ Optional username for Docker registry.
  ,dockerMonoidRegistryPassword :: !(Maybe String)
    -- ^ Optional password for Docker registry.
  ,dockerMonoidAutoPull :: !(Maybe Bool)
    -- ^ Automatically pull new images.
  ,dockerMonoidDetach :: !(Maybe Bool)
    -- ^ Whether to run a detached container
  ,dockerMonoidPersist :: !(Maybe Bool)
    -- ^ Create a persistent container (don't remove it when finished).  Implied by
    -- `dockerDetach`.
  ,dockerMonoidContainerName :: !(Maybe String)
    -- ^ Container name to use, only makes sense from command-line with `dockerPersist`
    -- or `dockerDetach`.
  ,dockerMonoidRunArgs :: ![String]
    -- ^ Arguments to pass directly to @docker run@
  ,dockerMonoidMount :: ![Mount]
    -- ^ Volumes to mount in the container
  ,dockerMonoidEnv :: ![String]
    -- ^ Environment variables to set in the container
  ,dockerMonoidDatabasePath :: !(Maybe String)
    -- ^ Location of image usage database.
  ,dockerMonoidStackExe :: !(Maybe String)
    -- ^ Location of container-compatible stack executable
  ,dockerMonoidSetUser :: !(Maybe Bool)
   -- ^ Set in-container user to match host's
  ,dockerMonoidRequireDockerVersion :: !VersionRange
  -- ^ See: 'dockerRequireDockerVersion'
  }
  deriving (Show)

-- | Decode uninterpreted docker options from JSON/YAML.
instance FromJSON (WithJSONWarnings DockerOptsMonoid) where
  parseJSON = withObjectWarnings "DockerOptsMonoid"
    (\o -> do dockerMonoidDefaultEnable    <- pure True
              dockerMonoidEnable           <- o ..:? dockerEnableArgName
              dockerMonoidRepoOrImage      <- ((Just . DockerMonoidImage) <$> o ..: dockerImageArgName) <|>
                                              ((Just . DockerMonoidRepo) <$> o ..: dockerRepoArgName) <|>
                                              pure Nothing
              dockerMonoidRegistryLogin    <- o ..:? dockerRegistryLoginArgName
              dockerMonoidRegistryUsername <- o ..:? dockerRegistryUsernameArgName
              dockerMonoidRegistryPassword <- o ..:? dockerRegistryPasswordArgName
              dockerMonoidAutoPull         <- o ..:? dockerAutoPullArgName
              dockerMonoidDetach           <- o ..:? dockerDetachArgName
              dockerMonoidPersist          <- o ..:? dockerPersistArgName
              dockerMonoidContainerName    <- o ..:? dockerContainerNameArgName
              dockerMonoidRunArgs          <- o ..:? dockerRunArgsArgName ..!= []
              dockerMonoidMount            <- o ..:? dockerMountArgName ..!= []
              dockerMonoidEnv              <- o ..:? dockerEnvArgName ..!= []
              dockerMonoidDatabasePath     <- o ..:? dockerDatabasePathArgName
              dockerMonoidStackExe         <- o ..:? dockerStackExeArgName
              dockerMonoidSetUser          <- o ..:? dockerSetUserArgName
              dockerMonoidRequireDockerVersion
                                           <- unVersionRangeJSON <$>
                                                 o ..:? dockerRequireDockerVersionArgName
                                                   ..!= VersionRangeJSON anyVersion
              return DockerOptsMonoid{..})

-- | Left-biased combine Docker options
instance Monoid DockerOptsMonoid where
  mempty = DockerOptsMonoid
    {dockerMonoidDefaultEnable    = False
    ,dockerMonoidEnable           = Nothing
    ,dockerMonoidRepoOrImage      = Nothing
    ,dockerMonoidRegistryLogin    = Nothing
    ,dockerMonoidRegistryUsername = Nothing
    ,dockerMonoidRegistryPassword = Nothing
    ,dockerMonoidAutoPull         = Nothing
    ,dockerMonoidDetach           = Nothing
    ,dockerMonoidPersist          = Nothing
    ,dockerMonoidContainerName    = Nothing
    ,dockerMonoidRunArgs          = []
    ,dockerMonoidMount            = []
    ,dockerMonoidEnv              = []
    ,dockerMonoidDatabasePath     = Nothing
    ,dockerMonoidStackExe         = Nothing
    ,dockerMonoidSetUser          = Nothing
    ,dockerMonoidRequireDockerVersion = anyVersion
    }
  mappend l r = DockerOptsMonoid
    {dockerMonoidDefaultEnable    = dockerMonoidDefaultEnable l || dockerMonoidDefaultEnable r
    ,dockerMonoidEnable           = dockerMonoidEnable l <|> dockerMonoidEnable r
    ,dockerMonoidRepoOrImage      = dockerMonoidRepoOrImage l <|> dockerMonoidRepoOrImage r
    ,dockerMonoidRegistryLogin    = dockerMonoidRegistryLogin l <|> dockerMonoidRegistryLogin r
    ,dockerMonoidRegistryUsername = dockerMonoidRegistryUsername l <|> dockerMonoidRegistryUsername r
    ,dockerMonoidRegistryPassword = dockerMonoidRegistryPassword l <|> dockerMonoidRegistryPassword r
    ,dockerMonoidAutoPull         = dockerMonoidAutoPull l <|> dockerMonoidAutoPull r
    ,dockerMonoidDetach           = dockerMonoidDetach l <|> dockerMonoidDetach r
    ,dockerMonoidPersist          = dockerMonoidPersist l <|> dockerMonoidPersist r
    ,dockerMonoidContainerName    = dockerMonoidContainerName l <|> dockerMonoidContainerName r
    ,dockerMonoidRunArgs          = dockerMonoidRunArgs r <> dockerMonoidRunArgs l
    ,dockerMonoidMount            = dockerMonoidMount r <> dockerMonoidMount l
    ,dockerMonoidEnv              = dockerMonoidEnv r <> dockerMonoidEnv l
    ,dockerMonoidDatabasePath     = dockerMonoidDatabasePath l <|> dockerMonoidDatabasePath r
    ,dockerMonoidStackExe         = dockerMonoidStackExe l <|> dockerMonoidStackExe r
    ,dockerMonoidSetUser          = dockerMonoidSetUser l <|> dockerMonoidSetUser r
    ,dockerMonoidRequireDockerVersion
                                  = intersectVersionRanges (dockerMonoidRequireDockerVersion l)
                                                           (dockerMonoidRequireDockerVersion r)
    }

-- | Where to get the `stack` executable to run in Docker containers
data DockerStackExe
    = DockerStackExeDownload  -- ^ Download from official bindist
    | DockerStackExeHost  -- ^ Host's `stack` (linux-x86_64 only)
    | DockerStackExeImage  -- ^ Docker image's `stack` (versions must match)
    | DockerStackExePath (Path Abs File) -- ^ Executable at given path
    deriving (Show)

-- | Parse 'DockerStackExe'.
parseDockerStackExe :: (MonadThrow m) => String -> m DockerStackExe
parseDockerStackExe t
    | t == dockerStackExeDownloadVal = return DockerStackExeDownload
    | t == dockerStackExeHostVal = return DockerStackExeHost
    | t == dockerStackExeImageVal = return DockerStackExeImage
    | otherwise = liftM DockerStackExePath (parseAbsFile t)

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
