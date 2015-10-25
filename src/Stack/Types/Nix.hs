{-# LANGUAGE OverloadedStrings, FlexibleInstances, RecordWildCards #-}

-- | Nix types.

module Stack.Types.Nix where

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
    -- ^ Are we using a special execution environment? (Docker container, Nix-shell, chroot...)
  ,execEnvPackages :: ![PackageName]
    -- ^ The system packages to be installed in the environment before it runs
  ,execEnvInitFile :: !(Maybe String)
    -- ^ The path of a file containing preconfiguration of the environment (e.g shell.nix)
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
    -- ^ System packages to use (given to nix-shell)
  ,execEnvMonoidInitFile :: !(Maybe String)
    -- ^ The path of a file containing preconfiguration of the environment (e.g shell.nix)
  }
  deriving (Show)

-- | Decode uninterpreted docker options from JSON/YAML.
instance FromJSON (ExecEnvOptsMonoid, [JSONWarning]) where
  parseJSON = withObjectWarnings "DockerOptsMonoid"
    (\o -> do execEnvMonoidDefaultEnable <- pure True
              execEnvMonoidEnable <- o ..:? execEnvEnableArgName
              execEnvMonoidPackages <- o ..:? execEnvPackagesArgName ..!= []
              execEnvMonoidInitFile <- o ..:? execEnvInitFileArgName
              return ExecEnvOptsMonoid{..})

-- | Left-biased combine Docker options
instance Monoid ExecEnvOptsMonoid where
  mempty = ExecEnvOptsMonoid
    {execEnvMonoidDefaultEnable = False
    ,execEnvMonoidEnable = Nothing
    ,execEnvMonoidPackages = []
    ,execEnvMonoidInitFile = Nothing
    }
  mappend l r = ExecEnvOptsMonoid
    {execEnvMonoidDefaultEnable = execEnvMonoidDefaultEnable l || execEnvMonoidDefaultEnable r
    ,execEnvMonoidEnable = execEnvMonoidEnable l <|> execEnvMonoidEnable r
    ,execEnvMonoidPackages = execEnvMonoidPackages l <> execEnvMonoidPackages r
    ,execEnvMonoidInitFile = execEnvMonoidInitFile l <|> execEnvMonoidInitFile r
    }

-- | ExecEnv enable argument name.
execEnvEnableArgName :: Text
execEnvEnableArgName = "enable"

-- | ExecEnv system packages argument name.
execEnvPackagesArgName :: Text
execEnvPackagesArgName = "packages"

-- | ExecEnv init env file path argument name.
execEnvInitFileArgName :: Text
execEnvInitFileArgName = "init-env-file"
