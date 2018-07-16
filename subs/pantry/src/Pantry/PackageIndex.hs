{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
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
    , CabalLoader (..)
    , HasCabalLoader (..)
    , configPackageIndex
    , configPackageIndexRoot
    ) where

import qualified Codec.Archive.Tar as Tar
import           Stack.Prelude
import           Data.Aeson.Extended
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import           Data.Conduit.Zlib (ungzip)
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HashMap
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
import           Network.HTTP.StackClient (getGlobalManager)
import           Network.HTTP.Download
import           Network.URI (parseURI)
import           Path (toFilePath, parseAbsFile, mkRelDir, mkRelFile, (</>), parseRelDir)
import           Path.Extra (tryGetModificationTime)
import           Path.IO
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageIndex
import           Stack.Types.PackageName
import           Stack.Types.Runner (HasRunner)
import           Stack.Types.Version
import qualified System.Directory as D
import           System.FilePath ((<.>))

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
requireIndex :: HasCabalLoader env => PackageIndex -> RIO env ()
requireIndex index = do
    tarFile <- configPackageIndex $ indexName index
    exists <- doesFileExist tarFile
    unless exists $ updateIndex index

-- | Update all of the package indices
updateAllIndices :: HasCabalLoader env => RIO env ()
updateAllIndices = do
    clearPackageCaches
    cl <- view cabalLoaderL
    mapM_ updateIndex (clIndices cl)

-- | Update the index tarball
updateIndex :: HasCabalLoader env => PackageIndex -> RIO env ()
updateIndex index =
  do let name = indexName index
         url = indexLocation index
     logSticky $ "Updating package index "
               <> display (indexNameText (indexName index))
               <> " (mirrored at "
               <> display url
               <> ") ..."
     case indexType index of
       ITVanilla -> updateIndexHTTP name url
       ITHackageSecurity hs -> updateIndexHackageSecurity name url hs
     logStickyDone "Update complete"

     -- Copy to the 00-index.tar filename for backwards
     -- compatibility. First wipe out the cache file if present.
     tarFile <- configPackageIndex name
     oldTarFile <- configPackageIndexOld name
     oldCacheFile <- configPackageIndexCacheOld name
     liftIO $ ignoringAbsence (removeFile oldCacheFile)
     withSourceFile (toFilePath tarFile) $ \src ->
       withSinkFile (toFilePath oldTarFile) $ \sink ->
       runConduit $ src .| sink

-- | Update the index tarball via HTTP
updateIndexHTTP :: HasCabalLoader env
                => IndexName
                -> Text -- ^ url
                -> RIO env ()
updateIndexHTTP indexName' url = do
    req <- parseRequest $ T.unpack url
    logInfo ("Downloading package index from " <> display url)
    gz <- configPackageIndexGz indexName'
    tar <- configPackageIndex indexName'
    wasDownloaded <- redownload req gz
    shouldUnpack <-
        if wasDownloaded
            then return True
            else not `liftM` doesFileExist tar

    if not shouldUnpack
        then packageIndexNotUpdated indexName'
        else do
            let tmp = toFilePath tar <.> "tmp"
            tmpPath <- parseAbsFile tmp

            deleteCache indexName'

            liftIO $ do
                withSourceFile (toFilePath gz) $ \input ->
                  withSinkFile tmp $ \output -> -- FIXME use withSinkFileCautious
                  runConduit $ input .| ungzip .| output
                renameFile tmpPath tar

-- | Update the index tarball via Hackage Security
updateIndexHackageSecurity
    :: HasCabalLoader env
    => IndexName
    -> Text -- ^ base URL
    -> HackageSecurity
    -> RIO env ()
updateIndexHackageSecurity indexName' url (HackageSecurity keyIds threshold) = do

-- If the index is newer than the cache, delete it so that
-- the next 'getPackageCaches' call recomputes it. This
-- could happen if a prior run of stack updated the index,
-- but exited before deleting the cache.
--
-- See https://github.com/commercialhaskell/stack/issues/3033
packageIndexNotUpdated :: HasCabalLoader env => IndexName -> RIO env ()
packageIndexNotUpdated indexName' = do
    mindexModTime <- tryGetModificationTime =<< configPackageIndex indexName'
    mcacheModTime <- tryGetModificationTime =<< configPackageIndexCache indexName'
    case (mindexModTime, mcacheModTime) of
        (Right indexModTime, Right cacheModTime) | cacheModTime < indexModTime -> do
            deleteCache indexName'
            logInfo "No updates to your package index were found, but clearing the index cache as it is older than the index."
        (Left _, _) -> do
            deleteCache indexName'
            logError "Error: No updates to your package index were found, but downloaded index is missing."
        _ -> logInfo "No updates to your package index were found"

-- | Delete the package index cache
deleteCache :: HasCabalLoader env => IndexName -> RIO env ()
deleteCache indexName' = do
    fp <- configPackageIndexCache indexName'
    eres <- liftIO $ tryIO $ removeFile fp
    case eres of
        Left e -> logDebug $ "Could not delete cache: " <> displayShow e
        Right () -> logDebug $ "Deleted index cache at " <> fromString (toFilePath fp)

-- | Get the known versions for a given package from the package caches.
--
-- See 'getPackageCaches' for performance notes.
getPackageVersions :: HasCabalLoader env => PackageName -> RIO env (HashMap Version (Maybe CabalHash))
getPackageVersions pkgName = lookupPackageVersions pkgName <$> getPackageCaches

lookupPackageVersions :: PackageName -> PackageCache index -> HashMap Version (Maybe CabalHash)
lookupPackageVersions pkgName (PackageCache m) =
    maybe HashMap.empty (HashMap.map extractOrigRevHash) $ HashMap.lookup pkgName m
  where
    -- Extract the original cabal file hash (the first element of the one or two
    -- element list currently representing the cabal file hashes).
    extractOrigRevHash (_,_, neRevHashesAndOffsets) =
      listToMaybe $ fst (NE.last neRevHashesAndOffsets)

-- | Load the package caches, or create the caches if necessary.
--
-- This has two levels of caching: in memory, and the on-disk cache. So,
-- feel free to call this function multiple times.
getPackageCaches :: HasCabalLoader env => RIO env (PackageCache PackageIndex)
getPackageCaches = do
    cl <- view cabalLoaderL
    mcached <- readIORef (clCache cl)
    case mcached of
        Just cached -> return cached
        Nothing -> do
            result <- liftM mconcat $ forM (clIndices cl) $ \index -> do
                fp <- configPackageIndexCache (indexName index)
                PackageCache pis <-
#if MIN_VERSION_template_haskell(2,13,0)
                    $(versionedDecodeOrLoad (storeVersionConfig "pkg-v5" "LLL6OCcimOqRm3r0JmsSlLHcaLE="
#else
                    $(versionedDecodeOrLoad (storeVersionConfig "pkg-v5" "A607WaDwhg5VVvZTxNgU9g52DO8="
#endif
                                             :: VersionConfig (PackageCache ())))
                    fp
                    (populateCache index)
                return $ PackageCache ((fmap.fmap) (\((), mpd, files) -> (index, mpd, files)) pis)
            liftIO $ writeIORef (clCache cl) (Just result)
            return result

-- | Clear the in-memory hackage index cache. This is needed when the
-- hackage index is updated.
clearPackageCaches :: HasCabalLoader env => RIO env ()
clearPackageCaches = do
  cl <- view cabalLoaderL
  writeIORef (clCache cl) Nothing

-- | Root for a specific package index
configPackageIndexRoot :: HasCabalLoader env => IndexName -> RIO env (Path Abs Dir)
configPackageIndexRoot (IndexName name) = do
    cl <- view cabalLoaderL
    let root = clStackRoot cl
    dir <- parseRelDir $ B8.unpack name
    return (root </> $(mkRelDir "indices") </> dir)

-- | Location of the 01-index.tar file
configPackageIndex :: HasCabalLoader env => IndexName -> RIO env (Path Abs File)
configPackageIndex name = (</> $(mkRelFile "01-index.tar")) <$> configPackageIndexRoot name

-- | Location of the 01-index.cache file
configPackageIndexCache :: HasCabalLoader env => IndexName -> RIO env (Path Abs File)
configPackageIndexCache name = (</> $(mkRelFile "01-index.cache")) <$> configPackageIndexRoot name

-- | Location of the 00-index.cache file
configPackageIndexCacheOld :: HasCabalLoader env => IndexName -> RIO env (Path Abs File)
configPackageIndexCacheOld = liftM (</> $(mkRelFile "00-index.cache")) . configPackageIndexRoot

-- | Location of the 00-index.tar file. This file is just a copy of
-- the 01-index.tar file, provided for tools which still look for the
-- 00-index.tar file.
configPackageIndexOld :: HasCabalLoader env => IndexName -> RIO env (Path Abs File)
configPackageIndexOld = liftM (</> $(mkRelFile "00-index.tar")) . configPackageIndexRoot

-- | Location of the 01-index.tar.gz file
configPackageIndexGz :: HasCabalLoader env => IndexName -> RIO env (Path Abs File)
configPackageIndexGz = liftM (</> $(mkRelFile "01-index.tar.gz")) . configPackageIndexRoot

--------------- Lifted from cabal-install, Distribution.Client.Tar:
-- | Return the number of blocks in an entry.
entrySizeInBlocks :: Tar.Entry -> Int64
entrySizeInBlocks entry = 1 + case Tar.entryContent entry of
  Tar.NormalFile     _   size -> bytesToBlocks size
  Tar.OtherEntryType _ _ size -> bytesToBlocks size
  _                           -> 0
  where
    bytesToBlocks s = 1 + ((fromIntegral s - 1) `div` 512)
