{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Dealing with the 00-index file and all its cabal files.
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
import           Control.Exception.Enclosed (tryIO)
import           Control.Monad (unless, when, liftM, void)
import           Control.Monad.Catch (MonadThrow, throwM, MonadCatch)
import qualified Control.Monad.Catch as C
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger                  (MonadLogger, logDebug,
                                                        logInfo, logWarn)
import           Control.Monad.Reader (asks)
import           Control.Monad.Trans.Control

import           Data.Aeson.Extended
import           Data.Store.VersionTagged
import qualified Data.ByteString.Lazy as L
import           Data.Conduit (($$), (=$))
import           Data.Conduit.Binary                   (sinkHandle,
                                                        sourceHandle)
import           Data.Conduit.Zlib (ungzip)
import           Data.Foldable (forM_)
import           Data.IORef
import           Data.Int (Int64)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Unsafe (unsafeTail)

import           Data.Traversable (forM)

import           Data.Typeable (Typeable)

import           Network.HTTP.Download
import           Path                                  (mkRelDir, parent,
                                                        parseRelDir, toFilePath,
                                                        parseAbsFile, (</>))
import           Path.IO
import           Prelude -- Fix AMP warning
import           Stack.Types
import           Stack.Types.StackT
import           System.FilePath (takeBaseName, (<.>))
import           System.IO                             (IOMode (ReadMode, WriteMode),
                                                        withBinaryFile)
import           System.Process.Read         (EnvOverride,
                                              doesExecutableExist, readInNull,
                                              tryProcessStdout)
import           System.Process.Run          (Cmd(..), callProcessInheritStderrStdout)
import           Data.Streaming.Process      (ProcessExitedUnsuccessfully(..))

-- | Populate the package index caches and return them.
populateCache
    :: (MonadIO m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
    => EnvOverride
    -> PackageIndex
    -> m (Map PackageIdentifier PackageCache)
populateCache menv index = do
    requireIndex menv index
    -- This uses full on lazy I/O instead of ResourceT to provide some
    -- protections. Caveat emptor
    path <- configPackageIndex (indexName index)
    let loadPIS = do
            $logSticky "Populating index cache ..."
            lbs <- liftIO $ L.readFile $ Path.toFilePath path
            loop 0 Map.empty (Tar.read lbs)
    pis <- loadPIS `C.catch` \e -> do
        $logWarn $ "Exception encountered when parsing index tarball: "
                <> T.pack (show (e :: Tar.FormatError))
        $logWarn "Automatically updating index and trying again"
        updateIndex menv index
        loadPIS

    when (indexRequireHashes index) $ forM_ (Map.toList pis) $ \(ident, pc) ->
        case pcDownload pc of
            Just _ -> return ()
            Nothing -> throwM $ MissingRequiredHashes (indexName index) ident

    $logStickyDone "Populated index cache."

    return pis
  where
    loop !blockNo !m (Tar.Next e es) =
        loop (blockNo + entrySizeInBlocks e) (goE blockNo m e) es
    loop _ m Tar.Done = return m
    loop _ _ (Tar.Fail e) = throwM e

    goE blockNo m e =
        case Tar.entryContent e of
            Tar.NormalFile lbs size ->
                case parseNameVersion $ Tar.entryPath e of
                    Just (ident, ".cabal") -> addCabal ident size
                    Just (ident, ".json") -> addJSON ident lbs
                    _ -> m
            _ -> m
      where
        addCabal ident size = Map.insertWith
            (\_ pcOld -> pcNew { pcDownload = pcDownload pcOld })
            ident
            pcNew
            m
          where
            pcNew = PackageCache
                { pcOffset = (blockNo + 1) * 512
                , pcSize = size
                , pcDownload = Nothing
                }
        addJSON ident lbs =
            case decode lbs of
                Nothing -> m
                Just !pd -> Map.insertWith
                    (\_ pc -> pc { pcDownload = Just pd })
                    ident
                    PackageCache
                        { pcOffset = 0
                        , pcSize = 0
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
        let (t6, suffix) = T.break (== '.') t5
        if t6 == p'
            then return (PackageIdentifier p v, suffix)
            else Nothing

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
requireIndex :: (MonadIO m,MonadLogger m
                ,MonadReader env m,HasHttpManager env
                ,HasConfig env,MonadBaseControl IO m,MonadCatch m)
             => EnvOverride
             -> PackageIndex
             -> m ()
requireIndex menv index = do
    tarFile <- configPackageIndex $ indexName index
    exists <- doesFileExist tarFile
    unless exists $ updateIndex menv index

-- | Update all of the package indices
updateAllIndices
    :: (MonadIO m,MonadLogger m
       ,MonadReader env m,HasHttpManager env
       ,HasConfig env,MonadBaseControl IO m, MonadCatch m)
    => EnvOverride
    -> m ()
updateAllIndices menv = do
    clearPackageCaches
    asks (configPackageIndices . getConfig) >>= mapM_ (updateIndex menv)

-- | Update the index tarball
updateIndex :: (MonadIO m,MonadLogger m
               ,MonadReader env m,HasHttpManager env
               ,HasConfig env,MonadBaseControl IO m, MonadCatch m)
            => EnvOverride
            -> PackageIndex
            -> m ()
updateIndex menv index =
  do let name = indexName index
         logUpdate mirror = $logSticky $ "Updating package index " <> indexNameText (indexName index) <> " (mirrored at " <> mirror  <> ") ..."
     git <- isGitInstalled menv
     case (git, indexLocation index) of
        (True, ILGit url) -> logUpdate url >> updateIndexGit menv name index url
        (True, ILGitHttp url _) -> logUpdate url >> updateIndexGit menv name index url
        (_, ILHttp url) -> logUpdate url >> updateIndexHTTP name index url
        (False, ILGitHttp _ url) -> logUpdate url >> updateIndexHTTP name index url
        (False, ILGit url) -> logUpdate url >> throwM (GitNotAvailable name)

-- | Update the index Git repo and the index tarball
updateIndexGit :: (MonadIO m,MonadLogger m,MonadReader env m,HasConfig env,MonadBaseControl IO m, MonadCatch m)
               => EnvOverride
               -> IndexName
               -> PackageIndex
               -> Text -- ^ Git URL
               -> m ()
updateIndexGit menv indexName' index gitUrl = do
     tarFile <- configPackageIndex indexName'
     let idxPath = parent tarFile
     ensureDir idxPath
     do
            repoName <- parseRelDir $ takeBaseName $ T.unpack gitUrl
            let cloneArgs =
                  ["clone"
                  ,T.unpack gitUrl
                  ,toFilePath repoName
                  ,"--depth"
                  ,"1"
                  ,"-b" --
                  ,"display"]
            sDir <- configPackageIndexRoot indexName'
            let suDir =
                  sDir </>
                  $(mkRelDir "git-update")
                acfDir = suDir </> repoName
            repoExists <- doesDirExist acfDir
            unless repoExists
                   (readInNull suDir "git" menv cloneArgs Nothing)
            $logSticky "Fetching package index ..."
            let runFetch = callProcessInheritStderrStdout
                    (Cmd (Just acfDir) "git" menv ["fetch","--tags","--depth=1"])
            runFetch `C.catch` \(ex :: ProcessExitedUnsuccessfully) -> do
                -- we failed, so wipe the directory and try again, see #1418
                $logWarn (T.pack (show ex))
                $logStickyDone "Failed to fetch package index, retrying."
                removeDirRecur acfDir
                readInNull suDir "git" menv cloneArgs Nothing
                $logSticky "Fetching package index ..."
                runFetch
            $logStickyDone "Fetched package index."

            when (indexGpgVerify index)
                (readInNull acfDir "git" menv ["tag","-v","current-hackage"]
                    (Just (T.unlines ["Signature verification failed. "
                                     ,"Please ensure you've set up your"
                                     ,"GPG keychain to accept the D6CF60FD signing key."
                                     ,"For more information, see:"
                                     ,"https://github.com/fpco/stackage-update#readme"])))

            -- generate index archive when commit id differs from cloned repo
            tarId <- getTarCommitId (toFilePath tarFile)
            cloneId <- getCloneCommitId acfDir
            unless (tarId `equals` cloneId)
                (generateArchive acfDir tarFile)
   where
     getTarCommitId fp =
         tryProcessStdout Nothing menv "sh" ["-c","git get-tar-commit-id < "++fp]

     getCloneCommitId dir =
         tryProcessStdout (Just dir) menv "git" ["rev-parse","current-hackage^{}"]

     equals (Right cid1) (Right cid2) = cid1 == cid2
     equals _ _ = False

     generateArchive acfDir tarFile = do
         ignoringAbsence (removeFile tarFile)
         deleteCache indexName'
         $logDebug ("Exporting a tarball to " <> (T.pack . toFilePath) tarFile)
         let tarFileTmp = toFilePath tarFile ++ ".tmp"
         readInNull acfDir
             "git" menv ["archive","--format=tar","-o",tarFileTmp,"current-hackage"]
             Nothing
         tarFileTmpPath <- parseAbsFile tarFileTmp
         renameFile tarFileTmpPath tarFile

-- | Update the index tarball via HTTP
updateIndexHTTP :: (MonadIO m,MonadLogger m
                   ,MonadThrow m,MonadReader env m,HasHttpManager env,HasConfig env)
                => IndexName
                -> PackageIndex
                -> Text -- ^ url
                -> m ()
updateIndexHTTP indexName' index url = do
    req <- parseUrl $ T.unpack url
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

    when (indexGpgVerify index)
        $ $logWarn
        $ "You have enabled GPG verification of the package index, " <>
          "but GPG verification only works with Git downloading"

-- | Is the git executable installed?
isGitInstalled :: MonadIO m
               => EnvOverride
               -> m Bool
isGitInstalled = flip doesExecutableExist "git"

-- | Delete the package index cache
deleteCache :: (MonadIO m, MonadReader env m, HasConfig env, MonadLogger m, MonadThrow m) => IndexName -> m ()
deleteCache indexName' = do
    fp <- configPackageIndexCache indexName'
    eres <- liftIO $ tryIO $ removeFile fp
    case eres of
        Left e -> $logDebug $ "Could not delete cache: " <> T.pack (show e)
        Right () -> $logDebug $ "Deleted index cache at " <> T.pack (toFilePath fp)

-- | Lookup a package's versions from 'IO'.
getPackageVersionsIO
    :: (MonadIO m, MonadReader env m, HasConfig env, MonadLogger m, HasHttpManager env, MonadBaseControl IO m, MonadCatch m)
    => m (PackageName -> IO (Set Version))
getPackageVersionsIO = do
    getCaches <- getPackageCachesIO
    return $ \name ->
        fmap (lookupPackageVersions name) getCaches

-- | Get the known versions for a given package from the package caches.
--
-- See 'getPackageCaches' for performance notes.
getPackageVersions
    :: (MonadIO m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m, MonadCatch m)
    => PackageName
    -> m (Set Version)
getPackageVersions pkgName =
    fmap (lookupPackageVersions pkgName) getPackageCaches

lookupPackageVersions :: PackageName -> Map PackageIdentifier a -> Set Version
lookupPackageVersions pkgName pkgCaches =
    Set.fromList [v | PackageIdentifier n v <- Map.keys pkgCaches, n == pkgName]

-- | Access the package caches from 'IO'.
--
-- FIXME: This is a temporary solution until a better solution
-- to access the package caches from Stack.Build.ConstructPlan
-- has been found.
getPackageCachesIO
    :: (MonadIO m, MonadReader env m, HasConfig env, MonadLogger m, HasHttpManager env, MonadBaseControl IO m, MonadCatch m)
    => m (IO (Map PackageIdentifier (PackageIndex, PackageCache)))
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
    :: (MonadIO m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m, MonadCatch m)
    => m (Map PackageIdentifier (PackageIndex, PackageCache))
getPackageCaches = do
    menv <- getMinimalEnvOverride
    config <- askConfig
    mcached <- liftIO $ readIORef (configPackageCaches config)
    case mcached of
        Just cached -> return cached
        Nothing -> do
            result <- liftM mconcat $ forM (configPackageIndices config) $ \index -> do
                fp <- configPackageIndexCache (indexName index)
                PackageCacheMap pis' <- taggedDecodeOrLoad fp $ liftM PackageCacheMap $ populateCache menv index
                return (fmap (index,) pis')
            liftIO $ writeIORef (configPackageCaches config) (Just result)
            return result

-- | Clear the in-memory hackage index cache. This is needed when the
-- hackage index is updated.
clearPackageCaches :: (MonadIO m, MonadLogger m, MonadReader env m, HasConfig env, MonadThrow m, HasHttpManager env, MonadBaseControl IO m, MonadCatch m)
                   => m ()
clearPackageCaches = do
    cacheRef <- asks (configPackageCaches . getConfig)
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
