{-# LANGUAGE OverloadedStrings, FlexibleInstances, RecordWildCards #-}

-- | Docker types.

module Stack.Types.ExecEnv where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Data.Aeson.Extended
import Data.Monoid
import Data.Text (Text)
import Path

import Stack.Types.PackageName

-- | Which ExecEnv are we using?
data ExecEnvType = NixShellExecEnv | DockerContainerExecEnv
  deriving (Show, Eq)

-- | Docker configuration.
data ExecEnvOpts = ExecEnvOpts
  {execEnvType :: !(Maybe ExecEnvType)
    -- ^ Are we using a special execution environment? (Docker or Nix-Shell)
  ,execEnvPackages :: ![PackageName]
    -- ^ The system packages to be installed in the environment before it runs
--  ,dockerContainerName :: !(Maybe String)
    -- ^ Container name to use, only makes sense from command-line with `dockerPersist`
    -- or `dockerDetach`.
--  ,execEnvRunArgs :: ![String]
    -- ^ Arguments to pass directly to @docker run@.
--  ,dockerEnv :: ![String]
    -- ^ Environment variables to set in the container.
--  ,dockerDatabasePath :: !(Path Abs File)
    -- ^ Location of image usage database.
--  ,dockerStackExe :: !(Maybe DockerStackExe)
    -- ^ Location of container-compatible stack executable
  }
  deriving (Show)

-- | An uninterpreted representation of stack execution environment options.
-- Configurations may be "cascaded" using mappend (left-biased).
data ExecEnvOptsMonoid = ExecEnvOptsMonoid
  {execEnvMonoidDefaultEnable :: !Bool
    -- ^ Should nix-shell be defaulted to enabled (does @execenv:@ section exist in the config)?
  ,execEnvMonoidEnable :: !(Maybe Bool)
    -- ^ Is using nix-shell enabled?
  ,execEnvMonoidPackages :: ![PackageName]
--  ,dockerMonoidContainerName :: !(Maybe String)
    -- ^ Container name to use, only makes sense from command-line with `dockerPersist`
    -- or `dockerDetach`.
--  ,dockerMonoidRunArgs :: ![String]
    -- ^ Arguments to pass directly to @docker run@
--  ,dockerMonoidEnv :: ![String]
    -- ^ Environment variables to set in the container
--  ,dockerMonoidDatabasePath :: !(Maybe String)
    -- ^ Location of image usage database.
--  ,dockerMonoidStackExe :: !(Maybe String)
    -- ^ Location of container-compatible stack executable
  }
  deriving (Show)

-- | Decode uninterpreted docker options from JSON/YAML.
instance FromJSON (ExecEnvOptsMonoid, [JSONWarning]) where
  parseJSON = withObjectWarnings "DockerOptsMonoid"
    (\o -> do execEnvMonoidDefaultEnable   <- pure True
              execEnvMonoidEnable          <- o ..:? execEnvEnableArgName
              execEnvMonoidPackages        <- o ..:? execEnvPackagesArgName ..!= []
--              dockerMonoidContainerName    <- o ..:? dockerContainerNameArgName
--              dockerMonoidRunArgs          <- o ..:? dockerRunArgsArgName ..!= []
--              dockerMonoidEnv              <- o ..:? dockerEnvArgName ..!= []
--              dockerMonoidDatabasePath     <- o ..:? dockerDatabasePathArgName
--              dockerMonoidStackExe         <- o ..:? dockerStackExeArgName
              return ExecEnvOptsMonoid{..})

-- | Left-biased combine Docker options
instance Monoid ExecEnvOptsMonoid where
  mempty = ExecEnvOptsMonoid
    {execEnvMonoidDefaultEnable    = False
    ,execEnvMonoidEnable           = Nothing
    ,execEnvMonoidPackages         = []
--    ,dockerMonoidContainerName    = Nothing
--    ,dockerMonoidRunArgs          = []
--    ,dockerMonoidEnv              = []
--    ,dockerMonoidDatabasePath     = Nothing
--    ,dockerMonoidStackExe         = Nothing
    }
  mappend l r = ExecEnvOptsMonoid
    {execEnvMonoidDefaultEnable   = execEnvMonoidDefaultEnable l || execEnvMonoidDefaultEnable r
    ,execEnvMonoidEnable          = execEnvMonoidEnable l <|> execEnvMonoidEnable r
    ,execEnvMonoidPackages        = execEnvMonoidPackages l <> execEnvMonoidPackages r
--    ,dockerMonoidContainerName    = dockerMonoidContainerName l <|> dockerMonoidContainerName r
--    ,dockerMonoidRunArgs          = dockerMonoidRunArgs r <> dockerMonoidRunArgs l
--    ,dockerMonoidEnv              = dockerMonoidEnv r <> dockerMonoidEnv l
--    ,dockerMonoidDatabasePath     = dockerMonoidDatabasePath l <|> dockerMonoidDatabasePath r
--    ,dockerMonoidStackExe         = dockerMonoidStackExe l <|> dockerMonoidStackExe r
    }

{- -- | Where to get the `stack` executable to run in Docker containers
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
-}

-- | ExecEnv enable argument name.
execEnvEnableArgName :: Text
execEnvEnableArgName = "enable"

-- | ExecEnv system packages argument name.
execEnvPackagesArgName :: Text
execEnvPackagesArgName = "packages"
{-
-- | Docker run args argument name.
dockerRunArgsArgName :: Text
dockerRunArgsArgName = "run-args"

-- | Docker environment variable argument name.
dockerEnvArgName :: Text
dockerEnvArgName = "env"

-- | Docker container name argument name.
dockerContainerNameArgName :: Text
dockerContainerNameArgName = "container-name"

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
-}
