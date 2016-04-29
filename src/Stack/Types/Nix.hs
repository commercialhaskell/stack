{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Nix types.

module Stack.Types.Nix where

import Control.Applicative
import Data.Aeson.Extended
import Data.Text (Text)
import Data.Monoid
import Prelude

-- | Nix configuration. Parameterize by resolver type to avoid cyclic
-- dependency.
data NixOpts = NixOpts
  {nixEnable   :: !Bool
  ,nixPureShell :: !Bool
  ,nixPackages :: ![Text]
    -- ^ The system packages to be installed in the environment before it runs
  ,nixInitFile :: !(Maybe FilePath)
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
  ,nixMonoidPureShell :: !(Maybe Bool)
    -- ^ Should the nix-shell be pure
  ,nixMonoidPackages :: !(Maybe [Text])
    -- ^ System packages to use (given to nix-shell)
  ,nixMonoidInitFile :: !(Maybe FilePath)
    -- ^ The path of a file containing preconfiguration of the environment (e.g shell.nix)
  ,nixMonoidShellOptions :: !(Maybe [Text])
    -- ^ Options to be given to the nix-shell command line
  ,nixMonoidPath :: !(Maybe [Text])
    -- ^ Override parts of NIX_PATH (notably 'nixpkgs')
  }
  deriving (Eq, Show)

-- | Decode uninterpreted nix options from JSON/YAML.
instance FromJSON (WithJSONWarnings NixOptsMonoid) where
  parseJSON = withObjectWarnings "NixOptsMonoid"
    (\o -> do nixMonoidDefaultEnable <- pure False
              nixMonoidEnable        <- o ..:? nixEnableArgName
              nixMonoidPureShell     <- o ..:? nixPureShellArgName
              nixMonoidPackages      <- o ..:? nixPackagesArgName
              nixMonoidInitFile      <- o ..:? nixInitFileArgName
              nixMonoidShellOptions  <- o ..:? nixShellOptsArgName
              nixMonoidPath          <- o ..:? nixPathArgName
              return NixOptsMonoid{..})

-- | Left-biased combine Nix options
instance Monoid NixOptsMonoid where
  mempty = NixOptsMonoid
    {nixMonoidDefaultEnable = False
    ,nixMonoidEnable        = Nothing
    ,nixMonoidPureShell     = Nothing
    ,nixMonoidPackages      = Nothing
    ,nixMonoidInitFile      = Nothing
    ,nixMonoidShellOptions  = Nothing
    ,nixMonoidPath          = Nothing
    }
  mappend l r = NixOptsMonoid
    {nixMonoidDefaultEnable = nixMonoidDefaultEnable l || nixMonoidDefaultEnable r
    ,nixMonoidEnable        = nixMonoidEnable l <|> nixMonoidEnable r
    ,nixMonoidPureShell     = nixMonoidPureShell l <|> nixMonoidPureShell r
    ,nixMonoidPackages      = nixMonoidPackages l <|> nixMonoidPackages r
    ,nixMonoidInitFile      = nixMonoidInitFile l <|> nixMonoidInitFile r
    ,nixMonoidShellOptions  = nixMonoidShellOptions l <|> nixMonoidShellOptions r
    ,nixMonoidPath          = nixMonoidPath l <|> nixMonoidPath r
    }

-- | Nix enable argument name.
nixEnableArgName :: Text
nixEnableArgName = "enable"

-- | Nix run in pure shell argument name.
nixPureShellArgName :: Text
nixPureShellArgName = "pure"

-- | Nix packages (build inputs) argument name.
nixPackagesArgName :: Text
nixPackagesArgName = "packages"

-- | shell.nix file path argument name.
nixInitFileArgName :: Text
nixInitFileArgName = "shell-file"

-- | Extra options for the nix-shell command argument name.
nixShellOptsArgName :: Text
nixShellOptsArgName = "nix-shell-options"

-- | NIX_PATH override argument name
nixPathArgName :: Text
nixPathArgName = "path"
