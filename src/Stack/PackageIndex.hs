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

-- | Dealing with the 00-index file and all its cabal files.
module Stack.PackageIndex
    ( updateAllIndices
    , PackageDownload (..)
    , PackageCache (..)
    , getPackageCaches
    ) where

import qualified Codec.Archive.Tar as Tar
import           Control.Exception (Exception)
import           Control.Exception.Enclosed (tryIO)
import           Control.Monad (unless, when, liftM, mzero)
import           Control.Monad.Catch (MonadThrow, throwM, MonadCatch)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger                  (MonadLogger, logDebug,
                                                        logInfo, logWarn)
import           Control.Monad.Reader (asks)
import           Control.Monad.Trans.Control

import           Data.Aeson.Extended
import qualified Data.Binary as Binary
import           Data.Binary.VersionTagged (taggedDecodeOrLoad)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Conduit (($$), (=$), yield, Producer)
import           Data.Conduit.Binary                   (sinkHandle,
                                                        sourceHandle)
import qualified Data.Conduit.List as CL
import           Data.Conduit.Zlib (ungzip)
import           Data.Foldable (forM_)
import           Data.Int (Int64)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T


import           Data.Text.Encoding (encodeUtf8)

import           Data.Traversable (forM)

import           Data.Typeable (Typeable)

import           Data.Word (Word64)



import           GHC.Generics (Generic)

import           Network.HTTP.Download
import           Path                                  (mkRelDir, parent,
                                                        parseRelDir, toFilePath,
                                                        (</>))
import           Path.IO
import           Prelude -- Fix AMP warning
import           Stack.Types
import           Stack.Types.StackT
import           System.Directory
import           System.FilePath (takeBaseName, (<.>))
import           System.IO                             (IOMode (ReadMode, WriteMode),
                                                        withBinaryFile)
import           System.Process.Read (readInNull, EnvOverride, doesExecutableExist)

data PackageCache = PackageCache
    { pcOffset :: !Int64
    -- ^ offset in bytes into the 00-index.tar file for the .cabal file contents
    , pcSize :: !Int64
    -- ^ size in bytes of the .cabal file
    , pcDownload :: !(Maybe PackageDownload)
    }
    deriving Generic
instance Binary.Binary PackageCache

-- | Stream all of the cabal files from the 00-index tar file.
withSourcePackageIndex
    :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
    => EnvOverride
    -> PackageIndex
    -> (Producer m (PackageIdentifier, Either PackageCache L.ByteString) -> m a)
    -> m a
withSourcePackageIndex menv index cont = do
    requireIndex menv index
    -- This uses full on lazy I/O instead of ResourceT to provide some
    -- protections. Caveat emptor
    cont $ do
        path <- configPackageIndex (indexName index)
        lbs <- liftIO $ L.readFile $ Path.toFilePath path
        loop 0 (Tar.read lbs)
  where
    loop blockNo (Tar.Next e es) = do
        goE blockNo e
        loop blockNo' es
      where
        blockNo' = blockNo + entrySizeInBlocks e
    loop _ Tar.Done = return ()
    loop _ (Tar.Fail e) = throwM e

    goE blockNo e =
        case Tar.entryContent e of
            Tar.NormalFile lbs size ->
                case parseNameVersion $ T.pack $ Tar.entryPath e of
                    Just (ident, ".cabal") ->
                        yield (ident, Left PackageCache
                            { pcOffset = (blockNo + 1) * 512
                            , pcSize = size
                            , pcDownload = Nothing
                            })
                    Just (ident, ".json") -> yield (ident, Right lbs)
                    _ -> return ()
            _ -> return ()

    parseNameVersion t1 = do
        let (p', t2) = T.break (== '/') $ T.replace "\\" "/" t1
        p <- parsePackageNameFromString $ T.unpack p'
        t3 <- maybe (throwM $ InvalidCabalPath t1 "no slash") return
            $ T.stripPrefix "/" t2
        let (v', t4) = T.break (== '/') t3
        v <- parseVersionFromString $ T.unpack v'
        t5 <- T.stripPrefix "/" t4
        let (t6, suffix) = T.break (== '.') t5
        when (t6 /= p') $ throwM $ InvalidCabalPath t1 $ "Expected at end: " <> p'
        return (PackageIdentifier p v, suffix)

data PackageIndexException
  = InvalidCabalPath Text Text
  | GitNotAvailable IndexName
  | MissingRequiredHashes IndexName PackageIdentifier
  deriving Typeable
instance Exception PackageIndexException
instance Show PackageIndexException where
    show (InvalidCabalPath x y) =
        "Invalid cabal path " ++ T.unpack x ++ ": " ++ T.unpack y
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
                ,MonadThrow m,MonadReader env m,HasHttpManager env
                ,HasConfig env,MonadBaseControl IO m,MonadCatch m)
             => EnvOverride
             -> PackageIndex
             -> m ()
requireIndex menv index = do
    tarFile <- configPackageIndex $ indexName index
    exists <- fileExists tarFile
    unless exists $ updateIndex menv index

-- | Update all of the package indices
updateAllIndices
    :: (MonadIO m,MonadLogger m
       ,MonadThrow m,MonadReader env m,HasHttpManager env
       ,HasConfig env,MonadBaseControl IO m, MonadCatch m)
    => EnvOverride
    -> m ()
updateAllIndices menv =
    asks (configPackageIndices . getConfig) >>= mapM_ (updateIndex menv)

-- | Update the index tarball
updateIndex :: (MonadIO m,MonadLogger m
               ,MonadThrow m,MonadReader env m,HasHttpManager env
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
        (False, ILGit url) -> logUpdate url >> (throwM $ GitNotAvailable name)

-- | Update the index Git repo and the index tarball
updateIndexGit :: (MonadIO m,MonadLogger m,MonadThrow m,MonadReader env m,HasConfig env,MonadBaseControl IO m, MonadCatch m)
               => EnvOverride
               -> IndexName
               -> PackageIndex
               -> Text -- ^ Git URL
               -> m ()
updateIndexGit menv indexName' index gitUrl = do
     tarFile <- configPackageIndex indexName'
     let idxPath = parent tarFile
     liftIO (createDirectoryIfMissing True (toFilePath idxPath))
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
            repoExists <-
              liftIO (doesDirectoryExist (toFilePath acfDir))
            unless repoExists
                   (readInNull suDir "git" menv cloneArgs Nothing)
            $logSticky "Fetching package index ..."
            readInNull acfDir "git" menv ["fetch","--tags","--depth=1"] Nothing
            $logStickyDone "Fetched package index."
            _ <-
              (liftIO . tryIO) (removeFile (toFilePath tarFile))
            when (indexGpgVerify index)
                 (do readInNull acfDir
                                "git"
                                menv
                                ["tag","-v","current-hackage"]
                                (Just (T.unlines ["Signature verification failed. "
                                                 ,"Please ensure you've set up your"
                                                 ,"GPG keychain to accept the D6CF60FD signing key."
                                                 ,"For more information, see:"
                                                 ,"https://github.com/fpco/stackage-update#readme"])))
            $logDebug ("Exporting a tarball to " <>
                       (T.pack . toFilePath) tarFile)
            deleteCache indexName'
            let tarFileTmp = toFilePath tarFile ++ ".tmp"
            readInNull acfDir
                       "git"
                       menv
                       ["archive"
                       ,"--format=tar"
                       ,"-o"
                       ,tarFileTmp
                       ,"current-hackage"]
                       Nothing
            liftIO $ renameFile tarFileTmp (toFilePath tarFile)

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
            else liftM not $ fileExists tar

    when toUnpack $ do
        let tmp = toFilePath tar <.> "tmp"

        deleteCache indexName'

        liftIO $ do
            withBinaryFile (toFilePath gz) ReadMode $ \input ->
                withBinaryFile tmp WriteMode $ \output ->
                    sourceHandle input
                    $$ ungzip
                    =$ sinkHandle output
            renameFile tmp $ toFilePath tar

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
    fp <- liftM toFilePath $ configPackageIndexCache indexName'
    eres <- liftIO $ tryIO $ removeFile fp
    case eres of
        Left e -> $logDebug $ "Could not delete cache: " <> T.pack (show e)
        Right () -> $logDebug $ "Deleted index cache at " <> T.pack fp

data PackageDownload = PackageDownload
    { pdSHA512 :: !ByteString
    , pdUrl    :: !ByteString
    , pdSize   :: !Word64
    }
    deriving (Show, Generic)
instance Binary.Binary PackageDownload
instance FromJSON PackageDownload where
    parseJSON = withObject "Package" $ \o -> do
        hashes <- o .: "package-hashes"
        sha512 <- maybe mzero return (Map.lookup ("SHA512" :: Text) hashes)
        locs <- o .: "package-locations"
        url <-
            case reverse locs of
                [] -> mzero
                x:_ -> return x
        size <- o .: "package-size"
        return PackageDownload
            { pdSHA512 = encodeUtf8 sha512
            , pdUrl = encodeUtf8 url
            , pdSize = size
            }

-- | Load the cached package URLs, or created the cache if necessary.
getPackageCaches :: (MonadIO m, MonadLogger m, MonadReader env m, HasConfig env, MonadThrow m, HasHttpManager env, MonadBaseControl IO m, MonadCatch m)
                 => EnvOverride
                 -> m (Map PackageIdentifier (PackageIndex, PackageCache))
getPackageCaches menv = do
    config <- askConfig
    liftM mconcat $ forM (configPackageIndices config) $ \index -> do
        fp <- liftM toFilePath $ configPackageIndexCache (indexName index)
        pis' <- taggedDecodeOrLoad fp $ populateCache menv index

        return (fmap (index,) pis')

-- | Populate the package index caches and return them.
populateCache :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env, MonadLogger m, HasHttpManager env, MonadBaseControl IO m, MonadCatch m)
              => EnvOverride
              -> PackageIndex
              -> m (Map PackageIdentifier PackageCache)
populateCache menv index = do
    $logSticky "Populating index cache ..."
    let add m (ident, Left pcNew) =
            Map.insertWith
                (\_ pcOld -> pcNew { pcDownload = pcDownload pcOld })
                ident
                pcNew
                m
        add m (ident, Right lbs) =
            case decode lbs of
                Nothing -> m
                Just pd ->
                    Map.insertWith
                        (\_ pc -> pc { pcDownload = Just pd })
                        ident
                        PackageCache
                            { pcOffset = 0
                            , pcSize = 0
                            , pcDownload = Just pd
                            }
                        m

    withSourcePackageIndex menv index $ \source -> do
        pis <- source $$ CL.fold add Map.empty

        when (indexRequireHashes index) $ forM_ (Map.toList pis) $ \(ident, pc) ->
            case pcDownload pc of
                Just _ -> return ()
                Nothing -> throwM $ MissingRequiredHashes (indexName index) ident

        $logStickyDone "Populated index cache."

        return pis

--------------- Lifted from cabal-install, Distribution.Client.Tar:
-- | Return the number of blocks in an entry.
entrySizeInBlocks :: Tar.Entry -> Int64
entrySizeInBlocks entry = 1 + case Tar.entryContent entry of
  Tar.NormalFile     _   size -> bytesToBlocks size
  Tar.OtherEntryType _ _ size -> bytesToBlocks size
  _                           -> 0
  where
    bytesToBlocks s = 1 + ((fromIntegral s - 1) `div` 512)
