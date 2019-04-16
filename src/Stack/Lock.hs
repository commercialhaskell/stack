{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stack.Lock
    ( lockCachedWanted
    , LockedLocation(..)
    , LockedPackage(..)
    , Locked(..)
    ) where

import Data.Aeson.Extended
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Yaml as Yaml
import Pantry
import Pantry.Internal (Unresolved(..))
import qualified Pantry.SHA256 as SHA256
import Path (addFileExtension, parent)
import Path.IO (doesFileExist, getModificationTime, resolveFile)
import qualified RIO.ByteString as B
import RIO.Process
import qualified RIO.Text as T
import RIO.Time (UTCTime)
import Stack.Prelude
import Stack.SourceMap
import Stack.Types.Config
import Stack.Types.SourceMap

data CompletedSnapshotLocation
    = CSLFilePath !(ResolvedPath File)
                  !SHA256
                  !FileSize
    | CSLCompiler !WantedCompiler
    | CSLUrl !Text !BlobKey
    deriving (Show, Eq)

instance ToJSON CompletedSnapshotLocation where
    toJSON (CSLFilePath fp sha size) =
        object [ "file" .= resolvedRelative fp
               , "sha" .= sha
               , "size" .= size
               ]
    toJSON (CSLCompiler c) =
        object ["compiler" .= toJSON c]
    toJSON (CSLUrl url (BlobKey sha size)) =
        object [ "url" .= url
               , "sha" .= sha
               , "size" .= size
               ]

instance FromJSON (WithJSONWarnings (Unresolved CompletedSnapshotLocation)) where
    parseJSON v = file v <|> url v <|> compiler v
      where
        file = withObjectWarnings "CSLFilepath" $ \o -> do
           ufp <- o ..: "file"
           sha <- o ..: "sha"
           size <- o ..: "size"
           pure $ Unresolved $ \mdir ->
             case mdir of
               Nothing -> throwIO $ InvalidFilePathSnapshot ufp
               Just dir -> do
                 absolute <- resolveFile dir (T.unpack ufp)
                 let fp = ResolvedPath (RelFilePath ufp) absolute
                 pure $ CSLFilePath fp sha size
        url = withObjectWarnings "CSLUrl" $ \o -> do
          url' <- o ..: "url"
          sha <- o ..: "sha"
          size <- o ..: "size"
          pure $ Unresolved $ \_ -> pure $ CSLUrl url' (BlobKey sha size)
        compiler = withObjectWarnings "CSLCompiler" $ \o -> do
          c <- o ..: "compiler"
          pure $ Unresolved $ \_ -> pure $ CSLCompiler c

data LockedLocation a b = LockedLocation
    { llOriginal :: a
    , llCompleted :: b
    } deriving (Show, Eq)

instance (ToJSON a, ToJSON b) => ToJSON (LockedLocation a b) where
    toJSON LockedLocation{..} =
        object [ "original" .= llOriginal, "completed" .= llCompleted ]

instance ( FromJSON (WithJSONWarnings (Unresolved a))
         , FromJSON (WithJSONWarnings (Unresolved b))) =>
         FromJSON (WithJSONWarnings (Unresolved (LockedLocation a b))) where
    parseJSON =
        withObjectWarnings "LockedLocation" $ \o -> do
            llOriginal <- jsonSubWarnings $ o ..: "original"
            llCompleted <- jsonSubWarnings $ o ..: "completed"
            pure $ LockedLocation <$> llOriginal <*> llCompleted

data LockedPackage = LockedPackage
    { lpLocation :: !(LockedLocation RawPackageLocationImmutable PackageLocationImmutable)
    , lpFlags :: !(Map FlagName Bool)
    , lpHidden :: !Bool
    , lpGhcOptions :: ![Text]
    , lpFromSnapshot :: !FromSnapshot
    } deriving (Show, Eq)

instance ToJSON LockedPackage where
    toJSON LockedPackage {..} =
        let toBoolean FromSnapshot = True
            toBoolean NotFromSnapshot = False
         in object $ concat
                [ ["location" .= lpLocation]
                , if Map.null lpFlags then [] else ["flags" .= toCabalStringMap lpFlags]
                , if lpFromSnapshot == FromSnapshot then [] else ["from-snapshot" .= toBoolean lpFromSnapshot]
                , if not lpHidden then [] else ["hidden" .= lpHidden]
                , if null lpGhcOptions then [] else ["ghc-options" .= lpGhcOptions]
                ]

-- Special wrapper extracting only 1 RawPackageLocationImmutable
-- serialization should not produce locations with multiple subdirs
-- so we should be OK using just a head element
newtype SingleRPLI = SingleRPLI { unSingleRPLI :: RawPackageLocationImmutable}

instance FromJSON (WithJSONWarnings (Unresolved SingleRPLI)) where
   parseJSON v =
     do
       WithJSONWarnings unresolvedRPLIs ws <- parseJSON v
       let withWarnings x = WithJSONWarnings x ws
       pure $ withWarnings $ Unresolved $ \mdir -> do
         rpli <- NE.head <$> resolvePaths mdir unresolvedRPLIs
         pure $ SingleRPLI rpli

instance FromJSON (WithJSONWarnings (Unresolved LockedPackage)) where
    parseJSON = withObjectWarnings "LockedPackage" $ \o -> do
        let unwrap (LockedLocation single c) = LockedLocation (unSingleRPLI single) c
        location <- jsonSubWarnings $ o ..: "location"
        lpFlags <- fmap unCabalStringMap $ o ..:? "flags" ..!= Map.empty
        lpHidden <- o ..:? "hidden" ..!= False
        lpGhcOptions <- o ..:? "ghc-options" ..!= []
        let fromBoolean True = FromSnapshot
            fromBoolean False = NotFromSnapshot
        lpFromSnapshot <- fmap fromBoolean $ o ..:? "from-snapshot" ..!= True
        pure $ (\lpLocation -> LockedPackage {..}) <$> fmap unwrap location

data Locked = Locked
    { lckStackSha :: !SHA256
    , lckStackSize :: !FileSize
    , lckCompiler :: WantedCompiler
    , lckSnapshots :: NE.NonEmpty (LockedLocation RawSnapshotLocation CompletedSnapshotLocation)
    , lckPackages :: Map PackageName LockedPackage
    }
    deriving (Show, Eq)

instance FromJSON (WithJSONWarnings (Unresolved Locked)) where
    parseJSON = withObjectWarnings "Locked" $ \o -> do
      stackYaml <- o ..: "stack-yaml"
      lckStackSha <- stackYaml ..: "sha256"
      lckStackSize <- stackYaml ..: "size"
      lckCompiler <- o ..: "compiler"
      snapshots <- jsonSubWarningsT $ o ..: "snapshots"
      packages <- fmap unCabalStringMap $ jsonSubWarningsT $ o ..: "packages"
      pure $ (\lckSnapshots lckPackages -> Locked {..}) <$> sequenceA snapshots <*> sequenceA packages

instance ToJSON Locked where
    toJSON Locked {..} =
        object
            [ "stack-yaml" .= object ["sha256" .= lckStackSha, "size" .= lckStackSize]
            , "compiler" .= lckCompiler
            , "snapshots" .= lckSnapshots
            , "packages" .= toCabalStringMap lckPackages
            ]

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
    if not lockExists
        then do
            (snap, slocs, completed) <-
                loadAndCompleteSnapshotRaw resolver Map.empty
            let compiler = snapshotCompiler snap
                snPkgs = Map.mapWithKey (\n p h -> snapToDepPackage h n p) (snapshotPackages snap)
            (wanted, prjCompleted) <- fillWanted Map.empty compiler snPkgs
            (stackSha, stackSize) <- shaSize stackFile
            let pkgs = mapMaybe (uncurry $ maybeWantedLockedPackage wanted)
                                (completed <> prjCompleted)
            snapshots <- for slocs $ \(orig, sloc) -> do
                compl <- case sloc of
                    SLFilePath fp -> do
                        (sha, size) <- shaSize (resolvedAbsolute fp)
                        pure $ CSLFilePath fp sha size
                    SLCompiler c ->
                        pure $ CSLCompiler c
                    SLUrl url blobKey ->
                        pure $ CSLUrl url blobKey
                pure $ LockedLocation orig compl
            liftIO $ Yaml.encodeFile (toFilePath lockFile) $
                Locked { lckStackSha = stackSha
                       , lckStackSize = stackSize
                       , lckCompiler = smwCompiler wanted
                       , lckSnapshots = snapshots
                       , lckPackages = Map.fromList pkgs
                       }
            pure wanted
        else do
            lmt <- liftIO $ getModificationTime lockFile
            unresolvedLocked <- loadYamlThrow parseJSON lockFile
            locked0 <- resolvePaths (Just $ parent stackFile) unresolvedLocked
            let pkgLocCache = Map.fromList
                    [ (llOriginal ll, llCompleted ll)
                    | ll <- map lpLocation $ Map.elems (lckPackages locked0) ]
                sha0 = lckStackSha locked0
                size0 = lckStackSize locked0
            result <- liftIO $ checkOutdated stackFile lmt size0 sha0
            let (syOutdated, sySha, sySize) =
                    case result of
                        Right () -> (False, sha0, size0)
                        Left (sha, sz) -> (True, sha, sz)
            let lockedSnapshots = Map.fromList
                    [ (orig, compl)
                    | LockedLocation orig compl <- NE.toList (lckSnapshots locked0)
                    ]
            layers <- readSnapshotLayers resolver
            (outdated, valid) <-
                fmap partitionEithers . forM (NE.toList layers) $ \(rsloc, sloc) -> liftIO $
                    let outdatedLoc = Left . LockedLocation rsloc
                        validLoc =  Right . LockedLocation rsloc
                    in case Map.lookup rsloc lockedSnapshots of
                        Nothing ->
                            case sloc of
                                SLFilePath fp -> do
                                    (sha, size) <- shaSize $ resolvedAbsolute fp
                                    pure $ outdatedLoc (CSLFilePath fp sha size)
                                SLCompiler c ->
                                    pure $ outdatedLoc (CSLCompiler c)
                                SLUrl u bk ->
                                    pure $ outdatedLoc (CSLUrl u bk)
                        Just loc@(CSLFilePath fp sha size) -> do
                            result' <- checkOutdated (resolvedAbsolute fp) lmt size sha
                            case result' of
                                Right () -> pure $ validLoc loc
                                Left (sha', size') ->
                                    pure $ outdatedLoc (CSLFilePath fp sha' size')
                        Just immutable ->
                             pure $ validLoc immutable
            let lockIsUpToDate = not syOutdated && null outdated
            if lockIsUpToDate
                then do
                    let compiler = lckCompiler locked0
                        pkgs = flip Map.mapWithKey (lckPackages locked0) $ \nm lp haddocks -> do
                            run <- askRunInIO
                            let location = llCompleted (lpLocation lp)
                                common = CommonPackage
                                    { cpName = nm
                                    , cpGPD = run $ loadCabalFileImmutable location
                                    , cpFlags = lpFlags lp
                                    , cpGhcOptions = lpGhcOptions lp
                                    , cpHaddocks = haddocks
                                    }
                            pure $ DepPackage{ dpLocation = PLImmutable location
                                             , dpCommon = common
                                             , dpHidden = lpHidden lp
                                             , dpFromSnapshot = lpFromSnapshot lp
                                             }
                    (wanted, _prjCompleted) <- fillWanted pkgLocCache compiler pkgs
                    pure wanted
                else do
                    (snap, _slocs, completed) <-
                        loadAndCompleteSnapshotRaw resolver pkgLocCache
                    let compiler = snapshotCompiler snap
                        snPkgs = Map.mapWithKey (\n p h -> snapToDepPackage h n p) (snapshotPackages snap)
                    (wanted, prjCompleted) <- fillWanted pkgLocCache compiler snPkgs
                    let pkgs = mapMaybe (uncurry $  maybeWantedLockedPackage wanted)
                                        (completed <> prjCompleted)
                    liftIO $ Yaml.encodeFile (toFilePath lockFile) $
                        Locked { lckStackSha = sySha
                               , lckStackSize = sySize
                               , lckCompiler = smwCompiler wanted
                               , lckSnapshots = NE.fromList $ outdated ++ valid
                               , lckPackages = Map.fromList pkgs
                               }
                    pure wanted              
  where
    maybeWantedLockedPackage wanted rpli pli = do
        let name = pkgName (packageLocationIdent pli)
        dp <- Map.lookup name (smwDeps wanted)
        let common = dpCommon dp
        pure ( name
             , LockedPackage { lpFlags = cpFlags common
                             , lpFromSnapshot = dpFromSnapshot dp
                             , lpGhcOptions = cpGhcOptions common
                             , lpHidden = dpHidden dp
                             , lpLocation = LockedLocation rpli pli
                             }
             )
    shaSize fp = do
        bs <- B.readFile $ toFilePath fp
        let size = FileSize . fromIntegral $ B.length bs
            sha = SHA256.hashBytes bs
        return (sha, size)

checkOutdated ::
       Path Abs File
    -> UTCTime
    -> FileSize
    -> SHA256
    -> IO (Either (SHA256, FileSize) ())
checkOutdated fp dt size sha = do
    mt <- getModificationTime fp
    if mt < dt
        then pure $ Right ()
        else do
            bs <- B.readFile $ toFilePath fp
            let newSize = FileSize . fromIntegral $ B.length bs
                newSha = SHA256.hashBytes bs
            if newSize /= size || sha /= newSha
                then pure $ Left (newSha, newSize)
                else pure $ Right ()
