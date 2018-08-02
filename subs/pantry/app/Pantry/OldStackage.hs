{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Pantry.OldStackage
  ( parseOldStackage
  ) where

import Pantry.Types
import Pantry.StaticSHA256
import Pantry.Storage
import RIO
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import RIO.Time (Day, toGregorian)
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Distribution.Types.PackageName (PackageName, mkPackageName)
import Distribution.PackageDescription (FlagName, mkFlagName)
import Data.Monoid (Endo (..))
import Data.Yaml (decodeFileThrow)

parseOldStackage
  :: (HasPantryConfig env, HasLogFunc env)
  => Either (Int, Int) Day -- ^ LTS or nightly
  -> Text -- ^ rendered name
  -> FilePath
  -> RIO env Snapshot
parseOldStackage snapName renderedSnapName fp = do
  value <- decodeFileThrow fp
  case parseEither (parseStackageSnapshot renderedSnapName) value of
    Left s -> error $ show (fp, s)
    Right x -> do
      locs <- mapM applyCrlfHack $ snapshotLocations x
      pure $ snapshotDefFixes snapName x { snapshotLocations = locs }
  where
    applyCrlfHack (PLHackage (PackageIdentifierRevision name version (CFIHash sha (Just size))) mtree) = do
      BlobKey sha' size' <- withStorage $ checkCrlfHack $ BlobKey sha size
      pure (PLHackage (PackageIdentifierRevision name version (CFIHash sha' (Just size'))) mtree)
    applyCrlfHack x = pure x

parseStackageSnapshot :: Text -> Value -> Parser Snapshot
parseStackageSnapshot snapshotName = withObject "StackageSnapshotDef" $ \o -> do
    Object si <- o .: "system-info"
    ghcVersion <- si .: "ghc-version"
    let snapshotParent = SLCompiler $ WCGhc $ unCabalString ghcVersion
    snapshotGlobalHints <- unCabalStringMap . (fmap.fmap) unCabalString <$> (si .: "core-packages")

    packages <- o .: "packages"
    (Endo mkLocs, snapshotFlags', snapshotHidden) <- fmap mconcat $ mapM (uncurry goPkg) $ Map.toList packages
    let snapshotLocations = mkLocs []
        snapshotFlags = Map.filter (not . Map.null) snapshotFlags'

    let snapshotGhcOptions = Map.empty -- Stackage snapshots do not allow setting GHC options

    -- Not dropping any packages in a Stackage snapshot
    let snapshotDropPackages = Set.empty

    return Snapshot {..}
  where
    goPkg
      :: CabalString PackageName
      -> Value
      -> Parser
           ( Endo [PackageLocation]
           , Map PackageName (Map FlagName Bool)
           , Map PackageName Bool
           )
    goPkg (CabalString name') = withObject "StackagePackageDef" $ \o -> do
        CabalString version <- o .: "version"
        mcabalFileInfo <- o .:? "cabal-file-info"
        mcabalFileInfo' <- forM mcabalFileInfo $ \o' -> do
            msize <- Just <$> o' .: "size"
            cfiHashes <- o' .: "hashes"
            hash' <-
              case Map.lookup ("SHA256" :: Text) cfiHashes of
                Nothing -> fail "Could not find SHA256"
                Just shaText ->
                  case mkStaticSHA256FromText shaText of
                    Left e -> fail $ "Invalid SHA256: " ++ show e
                    Right x -> return x
            return $ CFIHash hash' msize

        Object constraints <- o .: "constraints"

        flags <- constraints .: "flags"
        let flags' = Map.singleton name' $ unCabalStringMap flags

        hide <- constraints .:? "hide" .!= False
        let hide' = if hide then Map.singleton name' True else Map.empty

        let location = PLHackage (PackageIdentifierRevision
              name'
              version
              (fromMaybe CFILatest mcabalFileInfo'))
              Nothing -- no pantry key in old snapshots, we'll complete it during conversion

        return (Endo (location:), flags', hide')

-- | Some hard-coded fixes for build plans, only for hysterical raisins.
snapshotDefFixes :: Either (Int, Int) Day -> Snapshot -> Snapshot
snapshotDefFixes snapName sd | isOldStackage snapName = sd
    { snapshotFlags = Map.unionWith Map.union overrides $ snapshotFlags sd
    }
  where
    overrides = Map.fromList
      [ (mkPackageName "persistent-sqlite", Map.singleton (mkFlagName "systemlib") False)
      , (mkPackageName "yaml", Map.singleton (mkFlagName "system-libyaml") False)
      ]

    -- Only apply this hack to older Stackage snapshots. In
    -- particular, nightly-2018-03-13 did not contain these two
    -- packages.
    isOldStackage (Left (major, _)) = major < 11
    isOldStackage (Right (toGregorian -> (year, _, _))) = year < 2018
snapshotDefFixes _ sd = sd
