{-# LANGUAGE NoImplicitPrelude #-}
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
    , getPackageVersions
    , lookupPackageVersions
    ) where

import qualified Codec.Archive.Tar as Tar
import           Stack.Prelude
import           Data.Aeson.Extended
import qualified Data.ByteString.Lazy as L
import           Data.Conduit.Binary (sinkHandle, sourceHandle, sourceFile, sinkFile)
import           Data.Conduit.Zlib (ungzip)
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import           Data.Store.Version
import           Data.Store.VersionTagged
import qualified Data.Text as T
import           Data.Text.Unsafe (unsafeTail)
import           Data.Time (getCurrentTime)
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
import           Stack.Types.Config
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageIndex
import           Stack.Types.PackageName
import           Stack.Types.Version
import qualified System.Directory as D
import           System.FilePath ((<.>))

-- | Populate the package index caches and return them.
populateCache :: HasConfig env => PackageIndex -> RIO env (PackageCache ())
populateCache index = do
    requireIndex index
    -- This uses full on lazy I/O instead of ResourceT to provide some
    -- protections. Caveat emptor
    path <- configPackageIndex (indexName index)
    let loadPIS = withBinaryFile (Path.toFilePath path) ReadMode $ \h -> do
            $logSticky "Populating index cache ..."
            lbs <- liftIO $ L.hGetContents h
            loop 0 HashMap.empty (Tar.read lbs)
    pis0 <- loadPIS `catch` \e -> do
        $logWarn $ "Exception encountered when parsing index tarball: "
                <> T.pack (show (e :: Tar.FormatError))
        $logWarn "Automatically updating index and trying again"
        updateIndex index
        loadPIS

    when (indexRequireHashes index) $ forM_ (HashMap.toList pis0) $ \(ident, (_, mpd, _)) ->
        case mpd :: Maybe PackageDownload of
            Just _ -> return ()
            Nothing -> throwM $ MissingRequiredHashes (indexName index) ident

    cache <- fmap mconcat $ mapM convertPI $ HashMap.toList pis0

    $logStickyDone "Populated index cache."

    return cache
  where
    convertPI :: MonadIO m
              => (PackageIdentifier, ((), Maybe PackageDownload, Endo [([CabalHash], OffsetSize)]))
              -> m (PackageCache ())
    convertPI (ident@(PackageIdentifier name version), ((), mpd, Endo front)) =
      case NE.nonEmpty $ front [] of
        Nothing -> throwString $ "Missing cabal file info for: " ++ show ident
        Just files -> return
                    $ PackageCache
                    $ HashMap.singleton name
                    $ HashMap.singleton version
                      ((), mpd, files)

    loop :: MonadThrow m
         => Int64
         -> HashMap PackageIdentifier ((), Maybe PackageDownload, Endo [([CabalHash], OffsetSize)])
         -> Tar.Entries Tar.FormatError
         -> m (HashMap PackageIdentifier ((), Maybe PackageDownload, Endo [([CabalHash], OffsetSize)]))
    loop !blockNo !m (Tar.Next e es) =
        loop (blockNo + entrySizeInBlocks e) (goE blockNo m e) es
    loop _ m Tar.Done = return m
    loop _ _ (Tar.Fail e) = throwM e

    goE :: Int64
        -> HashMap PackageIdentifier ((), Maybe PackageDownload, Endo [([CabalHash], OffsetSize)])
        -> Tar.Entry
        -> HashMap PackageIdentifier ((), Maybe PackageDownload, Endo [([CabalHash], OffsetSize)])
    goE blockNo m e =
        case Tar.entryContent e of
            Tar.NormalFile lbs size ->
                case parseNameVersionSuffix $ Tar.entryPath e of
                    Just (ident, ".cabal") -> addCabal lbs ident size
                    Just (ident, ".json") -> addJSON id ident lbs
                    _ ->
                        case parsePackageJSON $ Tar.entryPath e of
                            Just ident -> addJSON unHSPackageDownload ident lbs
                            Nothing -> m
            _ -> m
      where
        addCabal lbs ident size =
            HashMap.insert ident
            (case HashMap.lookup ident m of
              Nothing -> ((), Nothing, newEndo)
              Just ((), mpd, oldEndo) -> ((), mpd, oldEndo <> newEndo))
            m
          where
            cabalHash = computeCabalHash lbs

            -- Some older Stackage snapshots ended up with slightly
            -- modified cabal files, in particular having DOS-style
            -- line endings (CRLF) converted to Unix-style (LF). As a
            -- result, we track both hashes with and without CR
            -- characters stripped for compatibility with these older
            -- snapshots.
            cr = 13
            cabalHashes
              | cr `L.elem` lbs = [cabalHash, computeCabalHash (L.filter (/= cr) lbs)]
              | otherwise = [cabalHash]
            offsetSize = OffsetSize ((blockNo + 1) * 512) size
            newPair = (cabalHashes, offsetSize)
            newEndo = Endo (newPair:)

        addJSON :: FromJSON a
                => (a -> PackageDownload)
                -> PackageIdentifier
                -> L.ByteString
                -> HashMap PackageIdentifier ((), Maybe PackageDownload, Endo [([CabalHash], OffsetSize)])
        addJSON unwrap ident lbs =
            case decode lbs of
                Nothing -> m
                Just (unwrap -> pd) ->
                  HashMap.insert ident
                  (case HashMap.lookup ident m of
                    Nothing -> ((), Just pd, mempty)
                    Just ((), Just oldPD, _)
                      | oldPD /= pd -> error $ concat
                        [ "Conflicting package hash information discovered for "
                        , packageIdentifierString ident
                        , "\nFound both: \n- "
                        , show oldPD
                        , "\n- "
                        , show pd
                        , "\n\nThis should not happen. See: https://github.com/haskell/hackage-security/issues/189"
                        ]
                    Just ((), _, files) -> ((), Just pd, files))
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
requireIndex :: HasConfig env => PackageIndex -> RIO env ()
requireIndex index = do
    tarFile <- configPackageIndex $ indexName index
    exists <- doesFileExist tarFile
    unless exists $ updateIndex index

-- | Update all of the package indices
updateAllIndices :: HasConfig env => RIO env ()
updateAllIndices = do
    clearPackageCaches
    view packageIndicesL >>= mapM_ updateIndex

-- | Update the index tarball
updateIndex :: HasConfig env => PackageIndex -> RIO env ()
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
     $logStickyDone "Update complete"

     -- Copy to the 00-index.tar filename for backwards
     -- compatibility. First wipe out the cache file if present.
     tarFile <- configPackageIndex name
     oldTarFile <- configPackageIndexOld name
     oldCacheFile <- configPackageIndexCacheOld name
     liftIO $ ignoringAbsence (removeFile oldCacheFile)
     liftIO $ runConduitRes $ sourceFile (toFilePath tarFile) .| sinkFile (toFilePath oldTarFile)

-- | Update the index tarball via HTTP
updateIndexHTTP :: HasConfig env
                => IndexName
                -> Text -- ^ url
                -> RIO env ()
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
                withBinaryFile tmp WriteMode $ \output -> runConduit
                  $ sourceHandle input
                 .| ungzip
                 .| sinkHandle output
            renameFile tmpPath tar

-- | Update the index tarball via Hackage Security
updateIndexHackageSecurity
    :: HasConfig env
    => IndexName
    -> Text -- ^ base URL
    -> HackageSecurity
    -> RIO env ()
updateIndexHackageSecurity indexName' url (HackageSecurity keyIds threshold) = do
    baseURI <-
        case parseURI $ T.unpack url of
            Nothing -> throwString $ "Invalid Hackage Security base URL: " ++ T.unpack url
            Just x -> return x
    manager <- liftIO getGlobalManager
    root <- configPackageIndexRoot indexName'
    run <- askRunInIO
    let logTUF = run . $logInfo . T.pack . HS.pretty
        withRepo = HS.withRepository
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
deleteCache :: HasConfig env => IndexName -> RIO env ()
deleteCache indexName' = do
    fp <- configPackageIndexCache indexName'
    eres <- liftIO $ tryIO $ removeFile fp
    case eres of
        Left e -> $logDebug $ "Could not delete cache: " <> T.pack (show e)
        Right () -> $logDebug $ "Deleted index cache at " <> T.pack (toFilePath fp)

-- | Get the known versions for a given package from the package caches.
--
-- See 'getPackageCaches' for performance notes.
getPackageVersions :: HasConfig env => PackageName -> RIO env (Set Version)
getPackageVersions pkgName = fmap (lookupPackageVersions pkgName) getPackageCaches

lookupPackageVersions :: PackageName -> PackageCache index -> Set Version
lookupPackageVersions pkgName (PackageCache m) =
    maybe Set.empty (Set.fromList . HashMap.keys) $ HashMap.lookup pkgName m

-- | Load the package caches, or create the caches if necessary.
--
-- This has two levels of caching: in memory, and the on-disk cache. So,
-- feel free to call this function multiple times.
getPackageCaches :: HasConfig env => RIO env (PackageCache PackageIndex)
getPackageCaches = do
    config <- view configL
    mcached <- liftIO $ readIORef (configPackageCache config)
    case mcached of
        Just cached -> return cached
        Nothing -> do
            result <- liftM mconcat $ forM (configPackageIndices config) $ \index -> do
                fp <- configPackageIndexCache (indexName index)
                PackageCache pis <-
                    $(versionedDecodeOrLoad (storeVersionConfig "pkg-v5" "A607WaDwhg5VVvZTxNgU9g52DO8="
                                             :: VersionConfig (PackageCache ())))
                    fp
                    (populateCache index)
                return $ PackageCache ((fmap.fmap) (\((), mpd, files) -> (index, mpd, files)) pis)
            liftIO $ writeIORef (configPackageCache config) (Just result)
            return result

-- | Clear the in-memory hackage index cache. This is needed when the
-- hackage index is updated.
clearPackageCaches :: HasConfig env => RIO env ()
clearPackageCaches = do
    cacheRef <- view $ configL.to configPackageCache
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
