{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedStrings     #-}

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
  { enable :: !Bool
  , pureShell :: !Bool
  , packages :: ![Text]
    -- ^ The system packages to be installed in the environment before it runs
  , initFile :: !(Maybe FilePath)
    -- ^ The path of a file containing preconfiguration of the environment
    -- (e.g shell.nix)
  , shellOptions :: ![Text]
    -- ^ Options to be given to the nix-shell command line
  , addGCRoots :: !Bool
    -- ^ Should we register gc roots so running nix-collect-garbage doesn't
    -- remove nix dependencies
  }
  deriving Show

-- | An uninterpreted representation of nix options.
-- Configurations may be "cascaded" using mappend (left-biased).
data NixOptsMonoid = NixOptsMonoid
  { enable :: !(First Bool)
    -- ^ Is using nix-shell enabled?
  , pureShell :: !(First Bool)
    -- ^ Should the nix-shell be pure
  , packages :: !(First [Text])
    -- ^ System packages to use (given to nix-shell)
  , initFile :: !(First FilePath)
    -- ^ The path of a file containing preconfiguration of the environment (e.g
    -- shell.nix)
  , shellOptions :: !(First [Text])
    -- ^ Options to be given to the nix-shell command line
  , path :: !(First [Text])
    -- ^ Override parts of NIX_PATH (notably 'nixpkgs')
  , addGCRoots :: !FirstFalse
    -- ^ Should we register gc roots so running nix-collect-garbage doesn't
    -- remove nix dependencies
  }
  deriving (Eq, Generic, Show)

-- | Decode uninterpreted nix options from JSON/YAML.
instance FromJSON (WithJSONWarnings NixOptsMonoid) where
  parseJSON = withObjectWarnings "NixOptsMonoid" $ \o -> do
    enable        <- First <$> o ..:? nixEnableArgName
    pureShell     <- First <$> o ..:? nixPureShellArgName
    packages      <- First <$> o ..:? nixPackagesArgName
    initFile      <- First <$> o ..:? nixInitFileArgName
    shellOptions  <- First <$> o ..:? nixShellOptsArgName
    path          <- First <$> o ..:? nixPathArgName
    addGCRoots    <- FirstFalse <$> o ..:? nixAddGCRootsArgName
    pure NixOptsMonoid
      { enable
      , pureShell
      , packages
      , initFile
      , shellOptions
      , path
      , addGCRoots
      }

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
