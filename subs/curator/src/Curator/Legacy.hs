{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Legacy-style snapshot files
module Curator.Legacy
  ( LegacySnapshot
  , toLegacySnapshot
  , LegacyBulkArgs (..)
  , legacyBulk
  ) where

import Conduit
import RIO
import RIO.Directory (doesFileExist)
import RIO.FilePath (splitDirectories, splitExtension, (</>))
import RIO.List (stripPrefix)
import RIO.PrettyPrint (HasTerm)
import RIO.Time (fromGregorian)
import Pantry
import Path.IO (resolveFile')
import Curator.Types
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

data LegacyBulkArgs = LegacyBulkArgs
  { lbaSnapshots :: !FilePath
  -- ^ Pantry snapshots dir
  , lbaLts :: !FilePath
  -- ^ Legacy LTS
  , lbaNightly :: !FilePath
  -- ^ Legacy nightly
  }
  deriving Show

convert :: Convert -> RIO PantryApp ()
convert Convert {..} = do
  logInfo $ "Convert from " <> fromString convertFrom <> " to " <> fromString convertTo
  abs' <- resolveFile' convertFrom
  let sloc = SLFilePath $ ResolvedPath (RelFilePath (fromString convertFrom)) abs'
  (snapshot, _, _) <- loadAndCompleteSnapshot sloc mempty mempty
  legacy <- toLegacySnapshot snapshot
  liftIO $ encodeFile convertTo legacy

data Convert = Convert
  { convertFrom :: !FilePath
  , convertTo :: !FilePath
  }
  deriving Show

legacyBulk :: LegacyBulkArgs -> RIO PantryApp ()
legacyBulk LegacyBulkArgs {..} = do
  logInfo "Bulk converting Pantry-based snapshots to legacy snapshots"
  let toDest (TargetLts major minor) = lbaLts </> concat ["lts-", show major, ".", show minor, ".yaml"]
      toDest (TargetNightly day) = lbaNightly </> concat ["nightly-", show day, ".yaml"]
  runConduitRes $
    sourceDirectoryDeep True lbaSnapshots .|
    concatMapC (\fp -> Convert fp <$> (toDest <$> (stripDirPrefix lbaSnapshots fp >>= parseTarget))) .|
    filterMC (fmap not . doesFileExist . convertTo) .|
    mapM_C (lift . convert)

stripDirPrefix :: FilePath -> FilePath -> Maybe [FilePath]
stripDirPrefix prefix fp = stripPrefix (splitDirectories prefix) (splitDirectories fp)

parseTarget :: [FilePath] -> Maybe Target
parseTarget ["lts", major, minorYaml] = do
  (minor, ".yaml") <- Just $ splitExtension minorYaml
  TargetLts <$> readMaybe major <*> readMaybe minor
parseTarget ["nightly", year, month, dayYaml] = do
  (day, ".yaml") <- Just $ splitExtension dayYaml
  TargetNightly <$> (fromGregorian
    <$> readMaybe year
    <*> readMaybe month
    <*> readMaybe day)
parseTarget _ = Nothing
