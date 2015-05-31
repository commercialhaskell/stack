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
{-# LANGUAGE ViewPatterns               #-}

-- | Dealing with the 00-index file and all its cabal files.
module Stack.PackageIndex
    ( sourcePackageIndex
    , readPackageIdents
    , UnparsedCabalFile (..)
    , updateIndex
    , requireIndex
    , getPkgVersions
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
import           Data.Maybe                            (fromMaybe, mapMaybe)
import           Data.Monoid                           ((<>))
import           Data.Set                              (Set)
import qualified Data.Set                              as Set
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Encoding                    (encodeUtf8)
import           Data.Text.Encoding.Error              (lenientDecode)
import qualified Data.Text.Lazy                        as TL
import           Data.Text.Lazy.Encoding               (decodeUtf8With)
import           Data.Typeable                         (Typeable)
import           Data.Word                             (Word64)
import qualified Distribution.Package                  as Cabal
import           Distribution.PackageDescription       (package,
                                                        packageDescription)
import           Distribution.PackageDescription       as X (GenericPackageDescription)
import           Distribution.PackageDescription.Parse (ParseResult (..),
                                                        parsePackageDescription)
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
    , ucfParse   :: forall m. MonadThrow m => m GenericPackageDescription
    , ucfLbs     :: L.ByteString
    , ucfEntry   :: Tar.Entry
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

-- | Stream a list of all the package identifiers
readPackageIdents :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env, MonadLogger m, HasHttpManager env)
                  => EnvOverride
                  -> m (Map PackageIdentifier PackageCache)
readPackageIdents menv = do
    config <- askConfig
    let fp = toFilePath $ configPackageIndexCache config
        load = do
            ebs <- liftIO $ tryIO $ Binary.decodeFileOrFail fp
            case ebs of
                Left e -> return $ Left $ toException e
                Right (Left e) -> return $ Left $ toException $ BinaryParseException e
                Right (Right pis) -> return $ Right pis
    x <- load
    case x of
        Left e -> do
            $logDebug $ "Populate index cache, load failed with " <> T.pack (show e)
            liftM fst $ populateCaches menv
        Right y -> return y

newtype BinaryParseException = BinaryParseException (ByteOffset, String)
    deriving (Show, Typeable)
instance Exception BinaryParseException

-- | Stream all of the cabal files from the 00-index tar file.
sourcePackageIndex :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m)
                   => EnvOverride
                   -> Producer m (Either UnparsedCabalFile (PackageIdentifier, L.ByteString))
sourcePackageIndex menv = do
    requireIndex menv
    -- This uses full on lazy I/O instead of ResourceT to provide some
    -- protections. Caveat emptor
    config <- askConfig
    lbs <- liftIO $ L.readFile $ Path.toFilePath $ configPackageIndex config
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
        , Tar.NormalFile lbs size <- Tar.entryContent e = do
            (fromCabalPackageName -> name, fromCabalVersion -> version) <- parseNameVersion front
            yield $ Left UnparsedCabalFile
                { ucfName = name
                , ucfVersion = version
                , ucfParse = goContent (Tar.entryPath e) name version lbs
                , ucfLbs = lbs
                , ucfEntry = e
                , ucfOffset = (blockNo + 1) * 512
                , ucfSize = size
                }
        | Just front <- T.stripSuffix ".json" $ T.pack $ Tar.entryPath e
        , Tar.NormalFile lbs _size <- Tar.entryContent e = do
            (fromCabalPackageName -> name, fromCabalVersion -> version) <- parseNameVersion front
            yield $ Right (PackageIdentifier name version, lbs)
        | otherwise = return ()

    goContent :: String -> PackageName -> Version -> L.ByteString -> (forall m. MonadThrow m => m GenericPackageDescription)
    goContent fp' name version lbs =
        case parsePackageDescription $ TL.unpack $ dropBOM $ decodeUtf8With lenientDecode lbs of
            ParseFailed e -> throwM $ CabalParseException fp' e
            ParseOk _warnings gpd -> do
                let pd = packageDescription gpd
                    Cabal.PackageIdentifier (fromCabalPackageName -> name') (fromCabalVersion -> version') = package pd
                when (name /= name' || version /= version') $
                    throwM $ MismatchedNameVersion fp'
                        name name' version version'
                return gpd

    -- https://github.com/haskell/hackage-server/issues/351
    dropBOM t = fromMaybe t $ TL.stripPrefix "\xFEFF" t

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
  deriving (Show,Typeable)
instance Exception PackageIndexException

-- | Require that an index be present, updating if it isn't.
requireIndex :: (MonadIO m,MonadLogger m
                ,MonadThrow m,MonadReader env m,HasHttpManager env
                ,HasConfig env)
             => EnvOverride
             -> m ()
requireIndex menv = do
    config <- askConfig
    let tarFile = configPackageIndex config
    exists <- liftIO $ doesFileExist $ toFilePath tarFile
    unless exists (updateIndex menv)

-- | Update the index tarball
updateIndex :: (MonadIO m,MonadLogger m
               ,MonadThrow m,MonadReader env m,HasHttpManager env
               ,HasConfig env)
            => EnvOverride
            -> m ()
updateIndex menv =
  do $logInfo "Updating package index ..."
     git <- isGitInstalled menv
     if git
        then updateIndexGit menv
        else updateIndexHTTP

-- | Update the index Git repo and the index tarball
updateIndexGit :: (MonadIO m,MonadLogger m,MonadThrow m,MonadReader env m,HasConfig env)
               => EnvOverride
               -> m ()
updateIndexGit menv = do
     config <- askConfig
     let tarFile = configPackageIndex config
         idxPath = parent tarFile
     liftIO (createDirectoryIfMissing True (toFilePath idxPath))
     do
            gitUrl <- askPackageIndexGitUrl
            repoName <- parseRelDir $ takeBaseName $ T.unpack gitUrl
            let cloneArgs =
                  ["clone"
                  ,T.unpack gitUrl
                  ,toFilePath repoName
                  ,"--depth"
                  ,"1"
                  ,"-b" --
                  ,"display"]
            let sDir = configStackRoot config
            let suDir =
                  sDir </>
                  $(mkRelDir "update")
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
            deleteCache
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
                => m ()
updateIndexHTTP = do
    config <- askConfig
    url <- askPackageIndexHttpUrl
    req <- parseUrl $ T.unpack url
    $logInfo ("Downloading package index from " <> url)
    wasDownloaded <- redownload req (configPackageIndexGz config)
    toUnpack <-
        if wasDownloaded
            then return True
            else liftIO $ fmap not $ doesFileExist $ toFilePath $ configPackageIndex config

    when toUnpack $ do
        let gz = toFilePath $ configPackageIndexGz config
            tar = toFilePath $ configPackageIndex config
            tmp = tar <.> "tmp"

        deleteCache

        liftIO $ do
            withBinaryFile gz ReadMode $ \input ->
                withBinaryFile tmp WriteMode $ \output ->
                    sourceHandle input
                    $$ ungzip
                    =$ sinkHandle output
            renameFile tmp tar

    when (configGpgVerifyIndex config)
        $ $logWarn
        $ "You have enabled GPG verification of the package index, " <>
          "but GPG verification only works with Git downloading"

-- | Fetch all the package versions for a given package
getPkgVersions :: (MonadIO m,MonadLogger m,MonadThrow m,MonadReader env m,HasConfig env,HasHttpManager env)
               => EnvOverride -> PackageName -> m (Set Version)
getPkgVersions menv pkg = do
    idents <- readPackageIdents menv
    return $ Set.fromList $ mapMaybe go $ Map.keys idents
  where
    go (PackageIdentifier n v)
        | n == pkg = Just v
        | otherwise = Nothing

-- | Is the git executable installed?
isGitInstalled :: MonadIO m
               => EnvOverride
               -> m Bool
isGitInstalled = flip doesExecutableExist "git"

-- | Delete the package index cache
deleteCache :: (MonadIO m, MonadReader env m, HasConfig env, MonadLogger m) => m ()
deleteCache = do
    config <- askConfig
    F.forM_ [configPackageIndexCache, configPackageIndexUrls] $ \f -> do
        let fp = toFilePath $ f config
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
                 -> m ( Map PackageIdentifier PackageCache
                      , Map PackageIdentifier PackageDownload
                      )
getPackageCaches menv = do
    config <- askConfig
    let fp1 = toFilePath $ configPackageIndexCache config
        fp2 = toFilePath $ configPackageIndexUrls config
        load fp = do
            ebs <- liftIO $ tryIO $ Binary.decodeFileOrFail fp
            case ebs of
                Left e -> return $ Left $ toException e
                Right (Left e) -> return $ Left $ toException $ BinaryParseException e
                Right (Right pis) -> return $ Right pis
    x <- load fp1
    case x of
        Left _ -> populateCaches menv
        Right x' -> do
            y <- load fp2
            case y of
                Left _ -> populateCaches menv
                Right y' -> return (x', y')

-- | Populate the package index caches and return them.
populateCaches :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env, MonadLogger m, HasHttpManager env)
               => EnvOverride
               -> m ( Map PackageIdentifier PackageCache
                    , Map PackageIdentifier PackageDownload
                    )
populateCaches menv = do
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

    (pis, pds) <- sourcePackageIndex menv $$ getZipSink ((,)
        <$> ZipSink (Map.fromList <$> (CL.mapMaybe toIdent =$ CL.consume))
        <*> ZipSink (Map.fromList <$> (CL.mapMaybe parseDownload =$ CL.consume)))

    config <- askConfig

    liftIO $ Binary.encodeFile (toFilePath $ configPackageIndexCache config) pis
    liftIO $ Binary.encodeFile (toFilePath $ configPackageIndexUrls config) pds

    $logInfo "Done populating cache"

    return (pis, pds)

--------------- Lifted from cabal-install, Distribution.Client.Tar:
-- | Return the number of blocks in an entry.
entrySizeInBlocks :: Tar.Entry -> Int64
entrySizeInBlocks entry = 1 + case Tar.entryContent entry of
  Tar.NormalFile     _   size -> bytesToBlocks size
  Tar.OtherEntryType _ _ size -> bytesToBlocks size
  _                           -> 0
  where
    bytesToBlocks s = 1 + ((fromIntegral s - 1) `div` 512)
