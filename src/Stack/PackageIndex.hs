{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Dealing with the 01-index file and all its cabal files.
module Stack.PackageIndex
    ( updateAllIndices
    , getPackageCaches
    , getPackageCachesIO
    , getPackageVersions
    , getPackageVersionsIO
    , lookupPackageVersions
    ) where

import qualified Codec.Archive.Tar as Tar
import           Control.Exception (Exception)
import           Control.Exception.Safe (tryIO)
import           Control.Monad (unless, when, liftM, void, guard)
import           Control.Monad.Catch (throwM)
import qualified Control.Monad.Catch as C
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (logDebug, logInfo, logWarn)
import           Control.Monad.Trans.Control
import           Crypto.Hash as Hash (hashlazy, Digest, SHA1)
import           Data.Aeson.Extended
import qualified Data.ByteArray.Encoding as Mem (convertToBase, Base(Base16))
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Conduit (($$), (=$), (.|), runConduitRes)
import           Data.Conduit.Binary (sinkHandle, sourceHandle, sourceFile, sinkFile)
import           Data.Conduit.Zlib (ungzip)
import           Data.Foldable (forM_)
import           Data.IORef
import           Data.Int (Int64)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Store.Version
import           Data.Store.VersionTagged
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Unsafe (unsafeTail)
import           Data.Time (getCurrentTime)
import           Data.Traversable (forM)
import           Data.Typeable (Typeable)
import qualified Hackage.Security.Client as HS
import qualified Hackage.Security.Client.Repository.Cache as HS
import qualified Hackage.Security.Client.Repository.Remote as HS
import qualified Hackage.Security.Client.Repository.HttpLib.HttpClient as HS
import qualified Hackage.Security.Util.Path as HS
import qualified Hackage.Security.Util.Pretty as HS
import           Network.HTTP.Client.TLS (getGlobalManager)
import           Network.HTTP.Download
import           Network.URI (parseURI)
import           Path (toFilePath, parseAbsFile)
import           Path.IO
import           Prelude -- Fix AMP warning
import           Stack.Types.BuildPlan (GitSHA1 (..))
import           Stack.Types.Config
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageIndex
import           Stack.Types.PackageName
import           Stack.Types.StackT
import           Stack.Types.StringError
import           Stack.Types.Version
import qualified System.Directory as D
import           System.FilePath ((<.>))
import           System.IO (IOMode (ReadMode, WriteMode), withBinaryFile)

-- | Populate the package index caches and return them.
populateCache
    :: (StackMiniM env m, HasConfig env)
    => PackageIndex
    -> m PackageCacheMap
populateCache index = do
    requireIndex index
    -- This uses full on lazy I/O instead of ResourceT to provide some
    -- protections. Caveat emptor
    path <- configPackageIndex (indexName index)
    let loadPIS = do
            $logSticky "Populating index cache ..."
            lbs <- liftIO $ L.readFile $ Path.toFilePath path
            loop 0 (Map.empty, HashMap.empty) (Tar.read lbs)
    (pis, gitPIs) <- loadPIS `C.catch` \e -> do
        $logWarn $ "Exception encountered when parsing index tarball: "
                <> T.pack (show (e :: Tar.FormatError))
        $logWarn "Automatically updating index and trying again"
        updateIndex index
        loadPIS

    when (indexRequireHashes index) $ forM_ (Map.toList pis) $ \(ident, pc) ->
        case pcDownload pc of
            Just _ -> return ()
            Nothing -> throwM $ MissingRequiredHashes (indexName index) ident

    $logStickyDone "Populated index cache."

    return $ PackageCacheMap pis gitPIs
  where
    loop !blockNo (!m, !hm) (Tar.Next e es) =
        loop (blockNo + entrySizeInBlocks e) (goE blockNo m hm e) es
    loop _ (m, hm) Tar.Done = return (m, hm)
    loop _ _ (Tar.Fail e) = throwM e

    goE blockNo m hm e =
        case Tar.entryContent e of
            Tar.NormalFile lbs size ->
                case parseNameVersionSuffix $ Tar.entryPath e of
                    Just (ident, ".cabal") -> addCabal lbs ident size
                    Just (ident, ".json") -> (addJSON id ident lbs, hm)
                    _ ->
                        case parsePackageJSON $ Tar.entryPath e of
                            Just ident -> (addJSON unHSPackageDownload ident lbs, hm)
                            Nothing -> (m, hm)
            _ -> (m, hm)
      where
        addCabal lbs ident size =
            ( Map.insertWith
                (\_ pcOld -> pcNew { pcDownload = pcDownload pcOld })
                ident
                pcNew
                m
            , HashMap.insert gitSHA1 offsetSize hm
            )
          where
            pcNew = PackageCache
                { pcOffsetSize = offsetSize
                , pcDownload = Nothing
                }
            offsetSize = OffsetSize
                    ((blockNo + 1) * 512)
                    size

            -- Calculate the Git SHA1 of the contents. This uses the
            -- Git algorithm of prepending "blob <size>\0" to the raw
            -- contents. We use this to be able to share the same SHA
            -- information between the Git and tarball backends.
            gitSHA1 = GitSHA1 $ Mem.convertToBase Mem.Base16 $ hashSHA1 $ L.fromChunks
                $ "blob "
                : S8.pack (show $ L.length lbs)
                : "\0"
                : L.toChunks lbs

        hashSHA1 :: L.ByteString -> Hash.Digest Hash.SHA1
        hashSHA1 = Hash.hashlazy

        addJSON :: FromJSON a
                => (a -> PackageDownload)
                -> PackageIdentifier
                -> L.ByteString
                -> Map PackageIdentifier PackageCache
        addJSON unwrap ident lbs =
            case decode lbs of
                Nothing -> m
                Just (unwrap -> pd) -> Map.insertWith
                    (\_ pc -> pc { pcDownload = Just pd })
                    ident
                    PackageCache
                        { pcOffsetSize = OffsetSize 0 0
                        , pcDownload = Just pd
                        }
                    m

    breakSlash x
        | T.null z = Nothing
        | otherwise = Just (y, unsafeTail z)
      where
        (y, z) = T.break (== '/') x

    parseNameVersion t1 = do
        (p', t3) <- breakSlash
                  $ T.map (\c -> if c == '\\' then '/' else c)
                  $ T.pack t1
        p <- parsePackageName p'
        (v', t5) <- breakSlash t3
        v <- parseVersion v'
        return (p', p, v, t5)

    parseNameVersionSuffix t1 = do
        (p', p, v, t5) <- parseNameVersion t1
        let (t6, suffix) = T.break (== '.') t5
        guard $ t6 == p'
        return (PackageIdentifier p v, suffix)

    parsePackageJSON t1 = do
        (_, p, v, t5) <- parseNameVersion t1
        guard $ t5 == "package.json"
        return $ PackageIdentifier p v

data PackageIndexException
  = GitNotAvailable IndexName
  | MissingRequiredHashes IndexName PackageIdentifier
  deriving Typeable
instance Exception PackageIndexException
instance Show PackageIndexException where
    show (GitNotAvailable name) = concat
        [ "Package index "
        , T.unpack $ indexNameText name
        , " only provides Git access, and you do not have"
        , " the git executable on your PATH"
        ]
    show (MissingRequiredHashes name ident) = concat
        [ "Package index "
        , T.unpack $ indexNameText name
        , " is configured to require package hashes, but no"
        , " hash is available for "
        , packageIdentifierString ident
        ]

-- | Require that an index be present, updating if it isn't.
requireIndex :: (StackMiniM env m, HasConfig env) => PackageIndex -> m ()
requireIndex index = do
    tarFile <- configPackageIndex $ indexName index
    exists <- doesFileExist tarFile
    unless exists $ updateIndex index

-- | Update all of the package indices
updateAllIndices :: (StackMiniM env m, HasConfig env) => m ()
updateAllIndices = do
    clearPackageCaches
    view packageIndicesL >>= mapM_ updateIndex

-- | Update the index tarball
updateIndex :: (StackMiniM env m, HasConfig env) => PackageIndex -> m ()
updateIndex index =
  do let name = indexName index
         url = indexLocation index
     $logSticky $ "Updating package index "
               <> indexNameText (indexName index)
               <> " (mirrored at "
               <> url
               <> ") ..."
     case indexType index of
       ITVanilla -> updateIndexHTTP name url
       ITHackageSecurity hs -> updateIndexHackageSecurity name url hs

     -- Copy to the 00-index.tar filename for backwards
     -- compatibility. First wipe out the cache file if present.
     tarFile <- configPackageIndex name
     oldTarFile <- configPackageIndexOld name
     oldCacheFile <- configPackageIndexCacheOld name
     ignoringAbsence (removeFile oldCacheFile)
     runConduitRes $ sourceFile (toFilePath tarFile) .| sinkFile (toFilePath oldTarFile)

-- | Update the index tarball via HTTP
updateIndexHTTP :: (StackMiniM env m, HasConfig env)
                => IndexName
                -> Text -- ^ url
                -> m ()
updateIndexHTTP indexName' url = do
    req <- parseRequest $ T.unpack url
    $logInfo ("Downloading package index from " <> url)
    gz <- configPackageIndexGz indexName'
    tar <- configPackageIndex indexName'
    wasDownloaded <- redownload req gz
    toUnpack <-
        if wasDownloaded
            then return True
            else not `liftM` doesFileExist tar

    when toUnpack $ do
        let tmp = toFilePath tar <.> "tmp"
        tmpPath <- parseAbsFile tmp

        deleteCache indexName'

        liftIO $ do
            withBinaryFile (toFilePath gz) ReadMode $ \input ->
                withBinaryFile tmp WriteMode $ \output ->
                    sourceHandle input
                    $$ ungzip
                    =$ sinkHandle output
            renameFile tmpPath tar

-- | Update the index tarball via Hackage Security
updateIndexHackageSecurity
    :: (StackMiniM env m, HasConfig env)
    => IndexName
    -> Text -- ^ base URL
    -> HackageSecurity
    -> m ()
updateIndexHackageSecurity indexName' url (HackageSecurity keyIds threshold) = do
    baseURI <-
        case parseURI $ T.unpack url of
            Nothing -> errorString $ "Invalid Hackage Security base URL: " ++ T.unpack url
            Just x -> return x
    manager <- liftIO getGlobalManager
    root <- configPackageIndexRoot indexName'
    logTUF <- embed_ ($logInfo . T.pack . HS.pretty)
    let withRepo = HS.withRepository
            (HS.makeHttpLib manager)
            [baseURI]
            HS.defaultRepoOpts
            HS.Cache
                { HS.cacheRoot = HS.fromAbsoluteFilePath $ toFilePath root
                , HS.cacheLayout = HS.cabalCacheLayout
                    -- Have Hackage Security write to a temporary file
                    -- to avoid invalidating the cache... continued
                    -- below at case didUpdate
                    { HS.cacheLayoutIndexTar = HS.rootPath $ HS.fragment "01-index.tar-tmp"
                    }
                }
            HS.hackageRepoLayout
            HS.hackageIndexLayout
            logTUF
    didUpdate <- liftIO $ withRepo $ \repo -> HS.uncheckClientErrors $ do
        needBootstrap <- HS.requiresBootstrap repo
        when needBootstrap $ do
            HS.bootstrap
                repo
                (map (HS.KeyId . T.unpack) keyIds)
                (HS.KeyThreshold (fromIntegral threshold))
        now <- getCurrentTime
        HS.checkForUpdates repo (Just now)

    case didUpdate of
        HS.HasUpdates -> do
            -- The index actually updated. Delete the old cache, and
            -- then move the temporary unpacked file to its real
            -- location
            tar <- configPackageIndex indexName'
            deleteCache indexName'
            liftIO $ D.renameFile (toFilePath tar ++ "-tmp") (toFilePath tar)
            $logInfo "Updated package list downloaded"
        HS.NoUpdates -> $logInfo "No updates to your package list were found"

-- | Delete the package index cache
deleteCache
    :: (StackMiniM env m, HasConfig env)
    => IndexName -> m ()
deleteCache indexName' = do
    fp <- configPackageIndexCache indexName'
    eres <- liftIO $ tryIO $ removeFile fp
    case eres of
        Left e -> $logDebug $ "Could not delete cache: " <> T.pack (show e)
        Right () -> $logDebug $ "Deleted index cache at " <> T.pack (toFilePath fp)

-- | Lookup a package's versions from 'IO'.
getPackageVersionsIO
    :: (StackMiniM env m, HasConfig env)
    => m (PackageName -> IO (Set Version))
getPackageVersionsIO = do
    getCaches <- getPackageCachesIO
    return $ \name ->
        fmap (lookupPackageVersions name . fst) getCaches

-- | Get the known versions for a given package from the package caches.
--
-- See 'getPackageCaches' for performance notes.
getPackageVersions
    :: (StackMiniM env m, HasConfig env)
    => PackageName
    -> m (Set Version)
getPackageVersions pkgName =
    fmap (lookupPackageVersions pkgName . fst) getPackageCaches

lookupPackageVersions :: PackageName -> Map PackageIdentifier a -> Set Version
lookupPackageVersions pkgName pkgCaches =
    Set.fromList [v | PackageIdentifier n v <- Map.keys pkgCaches, n == pkgName]

-- | Access the package caches from 'IO'.
--
-- FIXME: This is a temporary solution until a better solution
-- to access the package caches from Stack.Build.ConstructPlan
-- has been found.
getPackageCachesIO
    :: (StackMiniM env m, HasConfig env)
    => m (IO ( Map PackageIdentifier (PackageIndex, PackageCache)
             , HashMap GitSHA1 (PackageIndex, OffsetSize)))
getPackageCachesIO = toIO getPackageCaches
  where
    toIO :: (MonadIO m, MonadBaseControl IO m) => m a -> m (IO a)
    toIO m = do
        runInBase <- liftBaseWith $ \run -> return (void . run)
        return $ do
            i <- newIORef (error "Impossible evaluation in toIO")
            runInBase $ do
                x <- m
                liftIO $ writeIORef i x
            readIORef i

-- | Load the package caches, or create the caches if necessary.
--
-- This has two levels of caching: in memory, and the on-disk cache. So,
-- feel free to call this function multiple times.
getPackageCaches
    :: (StackMiniM env m, HasConfig env)
    => m ( Map PackageIdentifier (PackageIndex, PackageCache)
         , HashMap GitSHA1 (PackageIndex, OffsetSize)
         )
getPackageCaches = do
    config <- view configL
    mcached <- liftIO $ readIORef (configPackageCaches config)
    case mcached of
        Just cached -> return cached
        Nothing -> do
            result <- liftM mconcat $ forM (configPackageIndices config) $ \index -> do
                fp <- configPackageIndexCache (indexName index)
                PackageCacheMap pis' gitPIs <-
                    $(versionedDecodeOrLoad (storeVersionConfig "pkg-v2" "WlAvAaRXlIMkjSmg5G3dD16UpT8="
                                             :: VersionConfig PackageCacheMap))
                    fp
                    (populateCache index)
                return (fmap (index,) pis', fmap (index,) gitPIs)
            liftIO $ writeIORef (configPackageCaches config) (Just result)
            return result

-- | Clear the in-memory hackage index cache. This is needed when the
-- hackage index is updated.
clearPackageCaches :: (StackMiniM env m, HasConfig env) => m ()
clearPackageCaches = do
    cacheRef <- view packageCachesL
    liftIO $ writeIORef cacheRef Nothing

--------------- Lifted from cabal-install, Distribution.Client.Tar:
-- | Return the number of blocks in an entry.
entrySizeInBlocks :: Tar.Entry -> Int64
entrySizeInBlocks entry = 1 + case Tar.entryContent entry of
  Tar.NormalFile     _   size -> bytesToBlocks size
  Tar.OtherEntryType _ _ size -> bytesToBlocks size
  _                           -> 0
  where
    bytesToBlocks s = 1 + ((fromIntegral s - 1) `div` 512)
