{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stack.Lock
    ( lockCachedWanted
    , LockedLocation(..)
    , Locked(..)
    ) where

import Pantry.Internal.AesonExtended
import Data.ByteString.Builder (byteString)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Yaml as Yaml
import Path (parent)
import Path.Extended (addExtension)
import Path.IO (doesFileExist)
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
    { lckSnapshotLocations :: [LockedLocation RawSnapshotLocation SnapshotLocation]
    , lckPkgImmutableLocations :: [LockedLocation RawPackageLocationImmutable PackageLocationImmutable]
    } deriving (Eq, Show)

instance ToJSON Locked where
    toJSON Locked {..} =
        object
            [ "snapshots" .= lckSnapshotLocations
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
    eVal <- liftIO $ Yaml.decodeFileEither (toFilePath path)
    case eVal of
        Left parseException -> throwIO $
            ParseConfigFileException path parseException
        Right val -> case Yaml.parseEither parser val of
            Left err -> throwIO $ Yaml.AesonException err
            Right (WithJSONWarnings res warnings) -> do
                logJSONWarnings (toFilePath path) warnings
                pure res

lockCachedWanted ::
       (HasPantryConfig env, HasRunner env)
    => Path Abs File
    -> RawSnapshotLocation
    -> (Map RawPackageLocationImmutable PackageLocationImmutable
        -> WantedCompiler
        -> Map PackageName (Bool -> RIO env DepPackage)
        -> RIO env ( SMWanted, [CompletedPLI]))
    -> RIO env SMWanted
lockCachedWanted stackFile resolver fillWanted = do
    lockFile <- liftIO $ addExtension ".lock" stackFile
    let getLockExists = doesFileExist lockFile
    lfb <- view lockFileBehaviorL
    readLockFile <-
      case lfb of
        LFBIgnore -> pure False
        LFBReadWrite -> getLockExists
        LFBReadOnly -> getLockExists
        LFBErrorOnWrite -> getLockExists
    locked <-
        if readLockFile
        then do
            logDebug "Using package location completions from a lock file"
            unresolvedLocked <- loadYamlThrow parseJSON lockFile
            resolvePaths (Just $ parent stackFile) unresolvedLocked
        else do
            logDebug "Not reading lock file"
            pure $ Locked [] []
    let toMap :: Ord a => [LockedLocation a b] -> Map a b
        toMap =  Map.fromList . map (\ll -> (llOriginal ll, llCompleted ll))
        slocCache = toMap $ lckSnapshotLocations locked
        pkgLocCache = toMap $ lckPkgImmutableLocations locked
    debugRSL <- view rslInLogL
    (snap, slocCompleted, pliCompleted) <-
        loadAndCompleteSnapshotRaw' debugRSL resolver slocCache pkgLocCache
    let compiler = snapshotCompiler snap
        snPkgs = Map.mapWithKey (\n p h -> snapToDepPackage h n p) (snapshotPackages snap)
    (wanted, prjCompleted) <- fillWanted pkgLocCache compiler snPkgs
    let lockLocations = map (\(CompletedPLI r c) -> LockedLocation r c)
        differentSnapLocs (CompletedSL raw complete)
          | raw == toRawSL complete = Nothing
          | otherwise = Just $ LockedLocation raw complete
        newLocked = Locked { lckSnapshotLocations = mapMaybe differentSnapLocs slocCompleted
                           , lckPkgImmutableLocations =
                             lockLocations $ pliCompleted <> prjCompleted
                           }
    when (newLocked /= locked) $ do
      case lfb of
        LFBReadWrite ->
          writeBinaryFileAtomic lockFile $
            header <>
            byteString (Yaml.encode newLocked)
        LFBErrorOnWrite -> do
          logError "You indicated that Stack should error out on writing a lock file"
          logError $
            "I just tried to write the following lock file contents to " <>
            fromString (toFilePath lockFile)
          logError $ display $ decodeUtf8With lenientDecode $ Yaml.encode newLocked
          exitFailure
        LFBIgnore -> pure ()
        LFBReadOnly -> pure ()
    pure wanted
  where
    header =
      "# This file was autogenerated by Stack.\n\
      \# You should not edit this file by hand.\n\
      \# For more information, please see the documentation at:\n\
      \#   https://docs.haskellstack.org/en/stable/lock_files\n\n"
