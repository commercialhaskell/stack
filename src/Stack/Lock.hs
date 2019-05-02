{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stack.Lock
    ( lockCachedWanted
    , LockedLocation(..)
    , Locked(..)
    ) where

import Data.Aeson.Extended
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Yaml as Yaml
import Pantry
import Path (addFileExtension, parent)
import Path.IO (doesFileExist)
import RIO.Process
import Stack.Prelude
import Stack.SourceMap
import Stack.Types.Config
import Stack.Types.SourceMap

data LockedLocation a b = LockedLocation
    { llOriginal :: a
    , llCompleted :: b
    } deriving (Eq, Show)

instance (ToJSON a, ToJSON b) => ToJSON (LockedLocation a b) where
    toJSON ll =
        object [ "original" .= llOriginal ll, "completed" .= llCompleted ll ]

instance ( FromJSON (WithJSONWarnings (Unresolved a))
         , FromJSON (WithJSONWarnings (Unresolved b))
         ) =>
         FromJSON (WithJSONWarnings (Unresolved (LockedLocation a b))) where
    parseJSON =
        withObjectWarnings "LockedLocation" $ \o -> do
            original <- jsonSubWarnings $ o ..: "original"
            completed <- jsonSubWarnings $ o ..: "completed"
            pure $ LockedLocation <$> original <*> completed

-- Special wrapper extracting only 1 RawPackageLocationImmutable
-- serialization should not produce locations with multiple subdirs
-- so we should be OK using just a head element
newtype SingleRPLI = SingleRPLI { unSingleRPLI :: RawPackageLocationImmutable}

instance FromJSON (WithJSONWarnings (Unresolved SingleRPLI)) where
   parseJSON v =
     do
       WithJSONWarnings unresolvedRPLIs ws <- parseJSON v
       let withWarnings x = WithJSONWarnings x ws
       pure $ withWarnings $ SingleRPLI . NE.head <$> unresolvedRPLIs

data Locked = Locked
    { lckSnapshotLocaitons :: [LockedLocation RawSnapshotLocation SnapshotLocation]
    , lckPkgImmutableLocations :: [LockedLocation RawPackageLocationImmutable PackageLocationImmutable]
    } deriving (Eq, Show)

instance ToJSON Locked where
    toJSON Locked {..} =
        object
            [ "snapshots" .= lckSnapshotLocaitons
            , "packages" .= lckPkgImmutableLocations
            ]

instance FromJSON (WithJSONWarnings (Unresolved Locked)) where
    parseJSON = withObjectWarnings "Locked" $ \o -> do
      snapshots <- jsonSubWarningsT $ o ..: "snapshots"
      packages <- jsonSubWarningsT $ o ..: "packages"
      let unwrap ll = ll { llOriginal = unSingleRPLI (llOriginal ll) }
      pure $ Locked <$> sequenceA snapshots <*> (map unwrap <$> sequenceA packages)

loadYamlThrow
    :: HasLogFunc env
    => (Value -> Yaml.Parser (WithJSONWarnings a)) -> Path Abs File -> RIO env a
loadYamlThrow parser path = do
    val <- liftIO $ Yaml.decodeFileThrow (toFilePath path)
    case Yaml.parseEither parser val of
        Left err -> throwIO $ Yaml.AesonException err
        Right (WithJSONWarnings res warnings) -> do
            logJSONWarnings (toFilePath path) warnings
            return res

lockCachedWanted ::
       (HasPantryConfig env, HasProcessContext env, HasLogFunc env)
    => Path Abs File
    -> RawSnapshotLocation
    -> (Map RawPackageLocationImmutable PackageLocationImmutable
        -> WantedCompiler
        -> Map PackageName (Bool -> RIO env DepPackage)
        -> RIO env ( SMWanted, [CompletedPLI]))
    -> RIO env SMWanted
lockCachedWanted stackFile resolver fillWanted = do
    lockFile <- liftIO $ addFileExtension "lock" stackFile
    lockExists <- doesFileExist lockFile
    locked <-
        if not lockExists
        then do
            logDebug "Lock file doesn't exist"
            pure $ Locked [] []
        else do
            logDebug "Using package location completions from a lock file"
            unresolvedLocked <- loadYamlThrow parseJSON lockFile
            resolvePaths (Just $ parent stackFile) unresolvedLocked
    let toMap :: Ord a => [LockedLocation a b] -> Map a b
        toMap =  Map.fromList . map (\ll -> (llOriginal ll, llCompleted ll))
        slocCache = toMap $ lckSnapshotLocaitons locked
        pkgLocCache = toMap $ lckPkgImmutableLocations locked
    (snap, slocCompleted, pliCompleted) <-
        loadAndCompleteSnapshotRaw resolver slocCache pkgLocCache
    let compiler = snapshotCompiler snap
        snPkgs = Map.mapWithKey (\n p h -> snapToDepPackage h n p) (snapshotPackages snap)
    (wanted, prjCompleted) <- fillWanted Map.empty compiler snPkgs
    let lockLocations = map (uncurry LockedLocation)
        newLocked = Locked { lckSnapshotLocaitons = lockLocations slocCompleted
                           , lckPkgImmutableLocations =
                             lockLocations $ pliCompleted <> prjCompleted
                           }
    when (newLocked /= locked) $
      writeFileBinary (toFilePath lockFile) $
        header <>
        Yaml.encode newLocked
    pure wanted
  where
    header =
      "# This file was autogenerated by Stack.\n\
      \# You should not edit this file by hand.\n\
      \# For more information, please see the documentation at:\n\
      \#   https://docs.haskellstack.org/en/stable/lock_files\n\n"
