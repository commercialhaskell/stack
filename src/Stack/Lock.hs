{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Stack.Lock
  ( lockCachedWanted
  , LockedLocation (..)
  , Locked (..)
  ) where

import           Data.Aeson.Types ( FromJSON (..), ToJSON, Value, (.=), object )
import           Data.Aeson.WarningParser
                   ( WithJSONWarnings (..), (..:), jsonSubWarnings
                   , jsonSubWarningsT, logJSONWarnings, withObjectWarnings
                   )
import           Data.ByteString.Builder ( byteString )
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified RIO.NonEmpty as NE
import           Path ( parent )
import           Path.Extended ( addExtension )
import           Path.IO ( doesFileExist )
import           Stack.Prelude
import           Stack.SourceMap ( snapToDepPackage )
import           Stack.Types.Config.Exception ( ConfigPrettyException (..) )
import           Stack.Types.LockFileBehavior ( LockFileBehavior (..) )
import           Stack.Types.Runner ( HasRunner, lockFileBehaviorL, rslInLogL )
import           Stack.Types.SourceMap ( DepPackage, SMWanted )

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "Stack.Lock" module.
data LockPrettyException
  = WritingLockFileError (Path Abs File) Locked
  deriving (Show, Typeable)

instance Pretty LockPrettyException where
  pretty (WritingLockFileError lockFile newLocked) =
    "[S-1353]"
    <> line
    <> flow "Stack is configured to report an error on writing a lock file."
    <> blankLine
    <> fillSep
         [ flow "Stack just tried to write the following lock file content to"
         , pretty lockFile <> ":"
         ]
    <> blankLine
    <> string newLocked'
   where
    newLocked' = T.unpack . decodeUtf8With lenientDecode $ Yaml.encode newLocked

instance Exception LockPrettyException

data LockedLocation a b = LockedLocation
  { original :: a
  , completed :: b
  }
  deriving (Eq, Show)

instance (ToJSON a, ToJSON b) => ToJSON (LockedLocation a b) where
  toJSON ll =
      object [ "original" .= ll.original, "completed" .= ll.completed ]

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
newtype SingleRPLI
  = SingleRPLI { singleRPLI :: RawPackageLocationImmutable}

instance FromJSON (WithJSONWarnings (Unresolved SingleRPLI)) where
  parseJSON v =
    do
      WithJSONWarnings unresolvedRPLIs ws <- parseJSON v
      let withWarnings x = WithJSONWarnings x ws
      pure $ withWarnings $ SingleRPLI . NE.head <$> unresolvedRPLIs

data Locked = Locked
  { snapshotLocations :: [LockedLocation RawSnapshotLocation SnapshotLocation]
  , pkgImmutableLocations :: [LockedLocation RawPackageLocationImmutable PackageLocationImmutable]
  }
  deriving (Eq, Show)

instance ToJSON Locked where
  toJSON lck =
    object
      [ "snapshots" .= lck.snapshotLocations
      , "packages" .= lck.pkgImmutableLocations
      ]

instance FromJSON (WithJSONWarnings (Unresolved Locked)) where
  parseJSON = withObjectWarnings "Locked" $ \o -> do
    snapshots <- jsonSubWarningsT $ o ..: "snapshots"
    packages <- jsonSubWarningsT $ o ..: "packages"
    let unwrap :: LockedLocation SingleRPLI b -> LockedLocation RawPackageLocationImmutable b
        unwrap ll = ll { original = ll.original.singleRPLI }
    pure $ Locked <$> sequenceA snapshots <*> (map unwrap <$> sequenceA packages)

loadYamlThrow ::
     HasLogFunc env
  => (Value -> Yaml.Parser (WithJSONWarnings a))
  -> Path Abs File
  -> RIO env a
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
  -> (  Map RawPackageLocationImmutable PackageLocationImmutable
     -> WantedCompiler
     -> Map PackageName (Bool -> RIO env DepPackage)
     -> RIO env ( SMWanted, [CompletedPLI])
     )
  -> RIO env SMWanted
lockCachedWanted stackFile snapshot fillWanted = do
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
      toMap =  Map.fromList . map ((.original) &&& (.completed))
      slocCache = toMap locked.snapshotLocations
      pkgLocCache = toMap locked.pkgImmutableLocations
  debugRSL <- view rslInLogL
  (snap, slocCompleted, pliCompleted) <-
    loadAndCompleteSnapshotRaw' debugRSL snapshot slocCache pkgLocCache
  let compiler = snapshotCompiler snap
      snPkgs = Map.mapWithKey
                 (\n p h -> snapToDepPackage h n p)
                 (snapshotPackages snap)
  (wanted, prjCompleted) <- fillWanted pkgLocCache compiler snPkgs
  let lockLocations = map (\(CompletedPLI r c) -> LockedLocation r c)
      differentSnapLocs (CompletedSL raw complete)
        | raw == toRawSL complete = Nothing
        | otherwise = Just $ LockedLocation raw complete
      newLocked = Locked
        { snapshotLocations = mapMaybe differentSnapLocs slocCompleted
        , pkgImmutableLocations =
          lockLocations $ pliCompleted <> prjCompleted
        }
  when (newLocked /= locked) $
    case lfb of
      LFBReadWrite ->
        writeBinaryFileAtomic lockFile $
          header <>
          byteString (Yaml.encode newLocked)
      LFBErrorOnWrite ->
        prettyThrowIO $ WritingLockFileError lockFile newLocked
      LFBIgnore -> pure ()
      LFBReadOnly -> pure ()
  pure wanted
 where
  header =
    "# This file was autogenerated by Stack.\n\
    \# You should not edit this file by hand.\n\
    \# For more information, please see the documentation at:\n\
    \#   https://docs.haskellstack.org/en/stable/topics/lock_files\n\n"
