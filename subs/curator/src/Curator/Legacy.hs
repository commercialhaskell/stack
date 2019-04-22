{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Legacy-style snapshot files
module Curator.Legacy
  ( LegacySnapshot
  , toLegacySnapshot
  ) where

import RIO
import RIO.PrettyPrint (HasTerm)
import Pantry
import Data.Yaml

data LegacySnapshot = LegacySnapshot
  { lsGhcVersion :: !Version
  , lsCorePackages :: !(Map PackageName Version)
  , lsPackages :: !(Map PackageName PackageInfo)
  }

data PackageInfo = PackageInfo
  { piVersion :: !Version
  , piCabalSha :: !SHA256
  , piCabalSize :: !FileSize
  , piFlags :: !(Map FlagName Bool)
  , piHidden :: !Bool
  }

toLegacySnapshot
  :: (HasPantryConfig env, HasTerm env)
  => Snapshot
  -> RIO env LegacySnapshot
toLegacySnapshot Snapshot {..} = do
  lsGhcVersion <-
    case snapshotCompiler of
      WCGhc v -> pure v
      x -> error $ "Unexpected snapshotCompiler: " ++ show x
  mglobalHints <- loadGlobalHints snapshotCompiler
  lsCorePackages <-
    case mglobalHints of
      Nothing -> error $ "Could not load global hints for: " ++ show snapshotCompiler
      Just x -> pure x
  lsPackages <- traverse toPackageInfo snapshotPackages
  pure LegacySnapshot {..}

toPackageInfo
  :: HasPantryConfig env
  => SnapshotPackage
  -> RIO env PackageInfo
toPackageInfo (SnapshotPackage loc piFlags piHidden ghcOptions) = do
  unless (null ghcOptions) $ error "ghc-options not supported"
  (piVersion, BlobKey piCabalSha piCabalSize) <-
    case loc of
      PLIHackage (PackageIdentifier _name version) cabalFile _treekey ->
        pure (version, cabalFile)
      x -> error $ "Unsupported package location: " ++ show x
  pure PackageInfo {..}

instance ToJSON LegacySnapshot where
  toJSON LegacySnapshot {..} = object
    [ "system-info" .= object
        [ "ghc-version" .= CabalString lsGhcVersion
        , "core-packages" .= toCabalStringMap (CabalString <$> lsCorePackages)
        ]
    , "packages" .= toCabalStringMap lsPackages
    ]

instance ToJSON PackageInfo where
  toJSON PackageInfo {..} = object
    [ "version" .= CabalString piVersion
    , "cabal-file-info" .= object
        [ "size" .= piCabalSize
        , "hashes" .= object
            [ "SHA256" .= piCabalSha
            ]
        ]
    , "constraints" .= object
        [ "flags" .= toCabalStringMap piFlags
        , "hidden" .= piHidden
        ]
    ]
