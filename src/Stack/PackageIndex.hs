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

import qualified Codec.Archive.Tar                     as Tar
import           Control.Applicative                   ((<$>), (<*>))
import           Control.Exception                     (Exception, toException)
import           Control.Exception.Enclosed            (tryIO)
import           Control.Monad                         (unless, when, liftM, mzero)
import           Control.Monad.Catch                   (MonadThrow, throwM)
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Logger                  (MonadLogger, logDebug,
                                                        logInfo, logWarn)
import           Control.Monad.Reader                  (asks)
import           Data.Aeson
import qualified Data.Binary                           as Binary
import           Data.Binary.Get                       (ByteOffset)
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString.Lazy                  as L
import           Data.Conduit                          (($$), (=$), yield, Producer, ZipSink (..))
import           Data.Conduit.Binary                   (sinkHandle,
                                                        sourceHandle)
import qualified Data.Conduit.List                     as CL
import           Data.Conduit.Zlib                     (ungzip)
import qualified Data.Foldable                         as F
import           Data.Int                              (Int64)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Monoid                           ((<>), mconcat)
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Encoding                    (encodeUtf8)
import           Data.Traversable                      (forM)
import           Data.Typeable                         (Typeable)
import           Data.Word                             (Word64)
import           Distribution.ParseUtils               (PError)
import qualified Distribution.Text                     as DT
import           GHC.Generics                          (Generic)
import           Network.HTTP.Download
import           Path                                  (mkRelDir, parent,
                                                        parseRelDir, toFilePath,
                                                        (</>))
import           Stack.Types
import           System.Directory
import           System.FilePath                       (takeBaseName, (<.>))
import           System.IO                             (IOMode (ReadMode, WriteMode),
                                                        withBinaryFile)
import           System.Process.Read                   (runIn, EnvOverride, doesExecutableExist)

-- | A cabal file with name and version parsed from the filepath, and the
-- package description itself ready to be parsed. It's left in unparsed form
-- for efficiency.
data UnparsedCabalFile = UnparsedCabalFile
    { ucfName    :: PackageName
    , ucfVersion :: Version
    , ucfOffset  :: !Int64
    -- ^ Byte offset into the 00-index.tar file for the entry contents
    , ucfSize    :: !Int64
    -- ^ Size of the entry contents, in bytes
    }

data PackageCache = PackageCache
    { pcOffset :: !Int64
    -- ^ offset in bytes into the 00-index.tar file for the .cabal file contents
    , pcSize :: !Int64
    -- ^ size in bytes of the .cabal file
    }
    deriving Generic
instance Binary.Binary PackageCache

newtype BinaryParseException = BinaryParseException (ByteOffset, String)
    deriving (Show, Typeable)
instance Exception BinaryParseException

-- | Stream all of the cabal files from the 00-index tar file.
sourcePackageIndex :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m)
                   => EnvOverride
                   -> PackageIndex
                   -> Producer m (Either UnparsedCabalFile (PackageIdentifier, L.ByteString))
sourcePackageIndex menv index = do
    requireIndex menv index
    -- This uses full on lazy I/O instead of ResourceT to provide some
    -- protections. Caveat emptor
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

    goE blockNo e
        | Just front <- T.stripSuffix ".cabal" $ T.pack $ Tar.entryPath e
        , Tar.NormalFile _ size <- Tar.entryContent e = do
            (fromCabalPackageName -> name, fromCabalVersion -> version) <- parseNameVersion front
            yield $ Left UnparsedCabalFile
                { ucfName = name
                , ucfVersion = version
                , ucfOffset = (blockNo + 1) * 512
                , ucfSize = size
                }
        | Just front <- T.stripSuffix ".json" $ T.pack $ Tar.entryPath e
        , Tar.NormalFile lbs _size <- Tar.entryContent e = do
            (fromCabalPackageName -> name, fromCabalVersion -> version) <- parseNameVersion front
            yield $ Right (PackageIdentifier name version, lbs)
        | otherwise = return ()

    parseNameVersion t1 = do
        let (p', t2) = T.break (== '/') $ T.replace "\\" "/" t1
        p <- simpleParse p'
        t3 <- maybe (throwM $ InvalidCabalPath t1 "no slash") return
            $ T.stripPrefix "/" t2
        let (v', t4) = T.break (== '/') t3
        v <- simpleParse v'
        when (t4 /= T.cons '/' p') $ throwM $ InvalidCabalPath t1 $ "Expected at end: " <> p'
        return (p, v)

data InvalidCabalPath = InvalidCabalPath Text Text
    deriving (Show, Typeable)
instance Exception InvalidCabalPath

data CabalParseException
  = CabalParseException FilePath
                        PError
  | MismatchedNameVersion FilePath
                          PackageName
                          PackageName
                          Version
                          Version
  deriving (Show,Typeable)
instance Exception CabalParseException

-- | More generic simpleParse.
simpleParse :: (MonadThrow m,DT.Text a)
            => Text -> m a
simpleParse x =
  case DT.simpleParse (T.unpack x) of
    Nothing -> throwM (SimpleParseException x)
    Just x' -> return x'

-- | A simple parse exception.
newtype SimpleParseException = SimpleParseException Text
 deriving (Show,Typeable)
instance Exception SimpleParseException

data PackageIndexException =
  Couldn'tReadIndexTarball FilePath
                           Tar.FormatError
  | GitNotAvailable IndexName
  deriving (Show,Typeable)
instance Exception PackageIndexException

-- | Require that an index be present, updating if it isn't.
requireIndex :: (MonadIO m,MonadLogger m
                ,MonadThrow m,MonadReader env m,HasHttpManager env
                ,HasConfig env)
             => EnvOverride
             -> PackageIndex
             -> m ()
requireIndex menv index = do
    tarFile <- configPackageIndex $ indexName index
    exists <- liftIO $ doesFileExist $ toFilePath tarFile
    unless exists $ updateIndex menv index

-- | Update all of the package indices
updateAllIndices
    :: (MonadIO m,MonadLogger m
       ,MonadThrow m,MonadReader env m,HasHttpManager env
       ,HasConfig env)
    => EnvOverride
    -> m ()
updateAllIndices menv =
    asks (configPackageIndices . getConfig) >>= mapM_ (updateIndex menv)

-- | Update the index tarball
updateIndex :: (MonadIO m,MonadLogger m
               ,MonadThrow m,MonadReader env m,HasHttpManager env
               ,HasConfig env)
            => EnvOverride
            -> PackageIndex
            -> m ()
updateIndex menv index =
  do $logInfo $ "Updating package index " <> indexNameText (indexName index) <> "..."
     git <- isGitInstalled menv
     let name = indexName index
     case (git, indexLocation index) of
        (True, ILGit url) -> updateIndexGit menv name url
        (True, ILGitHttp url _) -> updateIndexGit menv name url
        (_, ILHttp url) -> updateIndexHTTP name url
        (False, ILGitHttp _ url) -> updateIndexHTTP name url
        (False, ILGit _) -> throwM $ GitNotAvailable name

-- | Update the index Git repo and the index tarball
updateIndexGit :: (MonadIO m,MonadLogger m,MonadThrow m,MonadReader env m,HasConfig env)
               => EnvOverride
               -> IndexName
               -> Text -- ^ Git URL
               -> m ()
updateIndexGit menv indexName' gitUrl = do
     config <- askConfig
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
                   (do $logInfo ("Cloning repository for first from " <> gitUrl)
                       runIn suDir "git" menv cloneArgs Nothing)
            runIn acfDir "git" menv ["fetch","--tags","--depth=1"] Nothing
            _ <-
              (liftIO . tryIO) (removeFile (toFilePath tarFile))
            when (configGpgVerifyIndex config)
                 (do runIn acfDir
                           "git"
                           menv
                           ["tag","-v","current-hackage"]
                           (Just (unlines ["Signature verification failed. "
                                          ,"Please ensure you've set up your"
                                          ,"GPG keychain to accept the D6CF60FD signing key."
                                          ,"For more information, see:"
                                          ,"https://github.com/fpco/stackage-update#readme"])))
            $logDebug ("Exporting a tarball to " <>
                       (T.pack . toFilePath) tarFile)
            deleteCache indexName'
            runIn acfDir
                  "git"
                  menv
                  ["archive"
                  ,"--format=tar"
                  ,"-o"
                  ,toFilePath tarFile
                  ,"current-hackage"]
                  Nothing

-- | Update the index tarball via HTTP
updateIndexHTTP :: (MonadIO m,MonadLogger m
                   ,MonadThrow m,MonadReader env m,HasHttpManager env,HasConfig env)
                => IndexName
                -> Text -- ^ url
                -> m ()
updateIndexHTTP indexName' url = do
    config <- askConfig
    req <- parseUrl $ T.unpack url
    $logInfo ("Downloading package index from " <> url)
    gz <- configPackageIndexGz indexName'
    tar <- configPackageIndex indexName'
    wasDownloaded <- redownload req gz
    toUnpack <-
        if wasDownloaded
            then return True
            else liftIO $ fmap not $ doesFileExist $ toFilePath tar

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

    when (configGpgVerifyIndex config)
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
deleteCache indexName' =
    F.forM_ [configPackageIndexCache, configPackageIndexUrls] $ \f -> do
        fp <- liftM toFilePath $ f indexName'
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
getPackageCaches :: (MonadIO m, MonadLogger m, MonadReader env m, HasConfig env, MonadThrow m, HasHttpManager env)
                 => EnvOverride
                 -> m ( Map PackageIdentifier (PackageIndex, PackageCache)
                      , Map PackageIdentifier (PackageIndex, PackageDownload)
                      )
getPackageCaches menv = do
    config <- askConfig
    liftM mconcat $ forM (configPackageIndices config) $ \index -> do
        fp1 <- liftM toFilePath $ configPackageIndexCache (indexName index)
        fp2 <- liftM toFilePath $ configPackageIndexUrls (indexName index)
        let load fp = do
                ebs <- liftIO $ tryIO $ Binary.decodeFileOrFail fp
                case ebs of
                    Left e -> return $ Left $ toException e
                    Right (Left e) -> return $ Left $ toException $ BinaryParseException e
                    Right (Right pis) -> return $ Right pis
        x <- load fp1
        case x of
            Left _ -> populateCaches menv index
            Right x' -> do
                y <- load fp2
                case y of
                    Left _ -> populateCaches menv index
                    Right y' -> return (fmap (index,) x', fmap (index,) y')

-- | Populate the package index caches and return them.
populateCaches :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env, MonadLogger m, HasHttpManager env)
               => EnvOverride
               -> PackageIndex
               -> m ( Map PackageIdentifier (PackageIndex, PackageCache)
                    , Map PackageIdentifier (PackageIndex, PackageDownload)
                    )
populateCaches menv index = do
    $logInfo "Populating index cache, may take a moment"
    let toIdent (Left ucf) = Just
            ( PackageIdentifier (ucfName ucf) (ucfVersion ucf)
            , PackageCache
                { pcOffset = ucfOffset ucf
                , pcSize = ucfSize ucf
                }
            )
        toIdent (Right _) = Nothing

        parseDownload (Left _) = Nothing
        parseDownload (Right (ident, lbs)) = do
            case decode lbs of
                Nothing -> Nothing
                Just pd -> Just (ident, pd)

    (pis, pds) <- sourcePackageIndex menv index $$ getZipSink ((,)
        <$> ZipSink (Map.fromList <$> (CL.mapMaybe toIdent =$ CL.consume))
        <*> ZipSink (Map.fromList <$> (CL.mapMaybe parseDownload =$ CL.consume)))

    fp1 <- configPackageIndexCache (indexName index)
    liftIO $ Binary.encodeFile (toFilePath fp1) pis

    fp2 <- configPackageIndexUrls (indexName index)
    liftIO $ Binary.encodeFile (toFilePath fp2) pds

    $logInfo "Done populating cache"

    return (fmap (index,) pis, fmap (index,) pds)

--------------- Lifted from cabal-install, Distribution.Client.Tar:
-- | Return the number of blocks in an entry.
entrySizeInBlocks :: Tar.Entry -> Int64
entrySizeInBlocks entry = 1 + case Tar.entryContent entry of
  Tar.NormalFile     _   size -> bytesToBlocks size
  Tar.OtherEntryType _ _ size -> bytesToBlocks size
  _                           -> 0
  where
    bytesToBlocks s = 1 + ((fromIntegral s - 1) `div` 512)
