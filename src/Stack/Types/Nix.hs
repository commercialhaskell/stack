{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Nix types.
module Stack.Types.Nix
  ( NixOpts (..)
  , NixOptsMonoid (..)
  , nixAddGCRootsArgName
  , nixEnableArgName
  , nixInitFileArgName
  , nixPackagesArgName
  , nixPathArgName
  , nixPureShellArgName
  , nixShellOptsArgName
  ) where

import           Data.Aeson.Types ( FromJSON (..) )
import           Data.Aeson.WarningParser
                   ( WithJSONWarnings, (..:?), withObjectWarnings )
import           Generics.Deriving.Monoid ( mappenddefault, memptydefault )
import           Stack.Prelude

-- | Nix configuration. Parameterize by resolver type to avoid cyclic
-- dependency.
data NixOpts = NixOpts
  { nixEnable :: !Bool
  , nixPureShell :: !Bool
  , nixPackages :: ![Text]
    -- ^ The system packages to be installed in the environment before it runs
  , nixInitFile :: !(Maybe FilePath)
    -- ^ The path of a file containing preconfiguration of the environment
    -- (e.g shell.nix)
  , nixShellOptions :: ![Text]
    -- ^ Options to be given to the nix-shell command line
  , nixAddGCRoots :: !Bool
    -- ^ Should we register gc roots so running nix-collect-garbage doesn't
    -- remove nix dependencies
  }
  deriving Show

-- | An uninterpreted representation of nix options.
-- Configurations may be "cascaded" using mappend (left-biased).
data NixOptsMonoid = NixOptsMonoid
  { nixMonoidEnable :: !(First Bool)
    -- ^ Is using nix-shell enabled?
  , nixMonoidPureShell :: !(First Bool)
    -- ^ Should the nix-shell be pure
  , nixMonoidPackages :: !(First [Text])
    -- ^ System packages to use (given to nix-shell)
  , nixMonoidInitFile :: !(First FilePath)
    -- ^ The path of a file containing preconfiguration of the environment (e.g
    -- shell.nix)
  , nixMonoidShellOptions :: !(First [Text])
    -- ^ Options to be given to the nix-shell command line
  , nixMonoidPath :: !(First [Text])
    -- ^ Override parts of NIX_PATH (notably 'nixpkgs')
  , nixMonoidAddGCRoots :: !FirstFalse
    -- ^ Should we register gc roots so running nix-collect-garbage doesn't
    -- remove nix dependencies
  }
  deriving (Eq, Generic, Show)

-- | Decode uninterpreted nix options from JSON/YAML.
instance FromJSON (WithJSONWarnings NixOptsMonoid) where
  parseJSON = withObjectWarnings "NixOptsMonoid"
    ( \o -> do
        nixMonoidEnable        <- First <$> o ..:? nixEnableArgName
        nixMonoidPureShell     <- First <$> o ..:? nixPureShellArgName
        nixMonoidPackages      <- First <$> o ..:? nixPackagesArgName
        nixMonoidInitFile      <- First <$> o ..:? nixInitFileArgName
        nixMonoidShellOptions  <- First <$> o ..:? nixShellOptsArgName
        nixMonoidPath          <- First <$> o ..:? nixPathArgName
        nixMonoidAddGCRoots    <- FirstFalse <$> o ..:? nixAddGCRootsArgName
        pure NixOptsMonoid{..}
    )

-- | Left-biased combine Nix options
instance Semigroup NixOptsMonoid where
  (<>) = mappenddefault

-- | Left-biased combine Nix options
instance Monoid NixOptsMonoid where
  mempty = memptydefault
  mappend = (<>)

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

-- | Add GC roots arg name
nixAddGCRootsArgName :: Text
nixAddGCRootsArgName = "add-gc-roots"
