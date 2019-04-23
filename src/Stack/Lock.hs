{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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

data LockedLocation =
    LockedLocation RawPackageLocationImmutable
                   PackageLocationImmutable
    deriving (Show, Eq)

instance ToJSON LockedLocation where
    toJSON (LockedLocation o c) =
        object [ "original" .= o, "completed" .= c ]

instance FromJSON (WithJSONWarnings (Unresolved LockedLocation)) where
    parseJSON =
        withObjectWarnings "LockedLocation" $ \o -> do
            original <- jsonSubWarnings $ o ..: "original"
            completed <- jsonSubWarnings $ o ..: "completed"
            pure $ (\single c -> LockedLocation (unSingleRPLI single) c) <$> original <*> completed

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

newtype Locked = Locked [LockedLocation]

instance FromJSON (Unresolved Locked) where
    parseJSON v = do
      locs <- unWarningParser $ jsonSubWarningsT (lift $  parseJSON v)
      pure $ Locked <$> sequenceA locs

loadYamlThrow
    :: HasLogFunc env
    => (Value -> Yaml.Parser a) -> Path Abs File -> RIO env a
loadYamlThrow parser path = do
    val <- liftIO $ Yaml.decodeFileThrow (toFilePath path)
    case Yaml.parseEither parser val of
        Left err -> throwIO $ Yaml.AesonException err
        Right res -> return res

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
    pkgLocCache <- if not lockExists
                   then do
                       logDebug "Lock file doesn't exist"
                       pure Map.empty
                   else do
                       logDebug "Using package location completions from a lock file"
                       unresolvedLocked <- loadYamlThrow parseJSON lockFile
                       Locked locked0 <- resolvePaths (Just $ parent stackFile) unresolvedLocked
                       pure $ Map.fromList [(orig, compl) | LockedLocation orig compl <- locked0]
     
    (snap, completed) <-
        loadAndCompleteSnapshotRaw resolver pkgLocCache
    let compiler = snapshotCompiler snap
        snPkgs = Map.mapWithKey (\n p h -> snapToDepPackage h n p) (snapshotPackages snap)
    (wanted, prjCompleted) <- fillWanted Map.empty compiler snPkgs
    liftIO $ Yaml.encodeFile (toFilePath lockFile) $
        map (uncurry LockedLocation) $
        prjCompleted <> completed
    pure wanted
