{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Nix types.

module Stack.Types.Nix where

import Control.Applicative
import Data.Aeson.Extended
import Data.Monoid
import Data.Text (Text)

import Stack.Types.PackageName

-- | Nix configuration.
data NixOpts = NixOpts
  {nixEnable   :: !Bool
  ,nixPackages :: ![PackageName]
    -- ^ The system packages to be installed in the environment before it runs
  ,nixInitFile :: !(Maybe String)
    -- ^ The path of a file containing preconfiguration of the environment (e.g shell.nix)
  ,nixShellOptions :: ![Text]
    -- ^ Options to be given to the nix-shell command line
  }
  deriving (Show)

-- | An uninterpreted representation of nix options.
-- Configurations may be "cascaded" using mappend (left-biased).
data NixOptsMonoid = NixOptsMonoid
  {nixMonoidDefaultEnable :: !Bool
    -- ^ Should nix-shell be defaulted to enabled (does @nix:@ section exist in the config)?
  ,nixMonoidEnable :: !(Maybe Bool)
    -- ^ Is using nix-shell enabled?
  ,nixMonoidPackages :: ![PackageName]
    -- ^ System packages to use (given to nix-shell)
  ,nixMonoidInitFile :: !(Maybe String)
    -- ^ The path of a file containing preconfiguration of the environment (e.g shell.nix)
  ,nixMonoidShellOptions :: ![Text]
    -- ^ Options to be given to the nix-shell command line
  }
  deriving (Show)

-- | Decode uninterpreted nix options from JSON/YAML.
instance FromJSON (NixOptsMonoid, [JSONWarning]) where
  parseJSON = withObjectWarnings "DockerOptsMonoid"
    (\o -> do nixMonoidDefaultEnable <- pure True
              nixMonoidEnable <- o ..:? nixEnableArgName
              nixMonoidPackages <- o ..:? nixPackagesArgName ..!= []
              nixMonoidInitFile <- o ..:? nixInitFileArgName
              nixMonoidShellOptions <- o ..:? nixShellOptsArgName ..!= []
              return NixOptsMonoid{..})

-- | Left-biased combine nix options
instance Monoid NixOptsMonoid where
  mempty = NixOptsMonoid
    {nixMonoidDefaultEnable = False
    ,nixMonoidEnable = Nothing
    ,nixMonoidPackages = []
    ,nixMonoidInitFile = Nothing
    ,nixMonoidShellOptions = []
    }
  mappend l r = NixOptsMonoid
    {nixMonoidDefaultEnable = nixMonoidDefaultEnable l || nixMonoidDefaultEnable r
    ,nixMonoidEnable = nixMonoidEnable l <|> nixMonoidEnable r
    ,nixMonoidPackages = nixMonoidPackages l <> nixMonoidPackages r
    ,nixMonoidInitFile = nixMonoidInitFile l <|> nixMonoidInitFile r
    ,nixMonoidShellOptions = nixMonoidShellOptions l <> nixMonoidShellOptions r
    }

-- | Nix enable argument name.
nixEnableArgName :: Text
nixEnableArgName = "enable"

-- | Nix system packages argument name.
nixPackagesArgName :: Text
nixPackagesArgName = "packages"

-- | Nix init env file path argument name.
nixInitFileArgName :: Text
nixInitFileArgName = "shell-file"

nixShellOptsArgName :: Text
nixShellOptsArgName = "nix-shell-options"
