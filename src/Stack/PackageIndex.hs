{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
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
    , findNewestVersions
    , UnparsedCabalFile (..)
    , getLatestDescriptions
    , updateIndex
    , requireIndex
    , getPkgVersions
    ) where

import qualified Codec.Archive.Tar                     as Tar
import           Control.Exception                     (Exception, toException)
import           Control.Exception.Enclosed            (tryIO)
import           Control.Monad                         (unless, when, liftM)
import           Control.Monad.Catch                   (MonadThrow, throwM)
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Logger                  (MonadLogger, logDebug,
                                                        logInfo, logWarn)
import qualified Data.Binary                           as Binary
import           Data.Binary.Get                       (ByteOffset)
import qualified Data.ByteString.Lazy                  as L
import           Data.Conduit                          (($$), (=$), yield, Producer)
import           Data.Conduit.Binary                   (sinkHandle,
                                                        sourceHandle)
import qualified Data.Conduit.List                     as CL
import           Data.Conduit.Zlib                     (ungzip)
import qualified Data.Foldable                         as F
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Maybe                            (fromMaybe, mapMaybe)
import           Data.Monoid                           (mempty, (<>))
import           Data.Set                              (Set)
import qualified Data.Set                              as Set
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Encoding.Error              (lenientDecode)
import qualified Data.Text.Lazy                        as TL
import           Data.Text.Lazy.Encoding               (decodeUtf8With)
import           Data.Traversable                      (forM)
import           Data.Typeable                         (Typeable)
import qualified Distribution.Package                  as Cabal
import           Distribution.PackageDescription       (package,
                                                        packageDescription)
import           Distribution.PackageDescription       as X (GenericPackageDescription)
import           Distribution.PackageDescription.Parse (ParseResult (..),
                                                        parsePackageDescription)
import           Distribution.ParseUtils               (PError)
import qualified Distribution.Text                     as DT
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
    }

-- | Stream a list of all the package identifiers
readPackageIdents :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env, MonadLogger m, HasHttpManager env)
                  => EnvOverride
                  -> m [PackageIdentifier]
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
            $logInfo "Populating index cache, may take a moment"
            let toIdent ucf = PackageIdentifier (ucfName ucf) (ucfVersion ucf)
            pis <- sourcePackageIndex menv $$ CL.map toIdent =$ CL.consume
            liftIO $ Binary.encodeFile fp pis
            return pis
        Right y -> return y

newtype BinaryParseException = BinaryParseException (ByteOffset, String)
    deriving (Show, Typeable)
instance Exception BinaryParseException

-- | Find the newest versions of all given package names.
findNewestVersions :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env, MonadLogger m, HasHttpManager env)
                   => EnvOverride
                   -> [PackageName]
                   -> m [PackageIdentifier]
findNewestVersions menv names0 = do
    (m, discovered) <- liftM (F.foldl' add (Map.empty, Set.empty)) (readPackageIdents menv)
    let missing = Set.difference names discovered
    if Set.null missing
        then return $ map fromTuple $ Map.toList m
        else throwM $ Couldn'tFindPackages missing
  where
    names = Set.fromList names0

    add orig@(m, discovered) (PackageIdentifier n v)
        | n `Set.member` names =
            let !m' = Map.insertWith max n v m
                !d' = Set.insert n discovered
             in (m', d')
        | otherwise = orig

-- | Stream all of the cabal files from the 00-index tar file.
sourcePackageIndex :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m)
                   => EnvOverride
                   -> Producer m UnparsedCabalFile
sourcePackageIndex menv = do
    requireIndex menv
    -- This uses full on lazy I/O instead of ResourceT to provide some
    -- protections. Caveat emptor
    config <- askConfig
    lbs <- liftIO $ L.readFile $ Path.toFilePath $ configPackageIndex config
    loop (Tar.read lbs)
  where
    loop (Tar.Next e es) = goE e >> loop es
    loop Tar.Done = return ()
    loop (Tar.Fail e) = throwM e

    goE e
        | Just front <- T.stripSuffix ".cabal" $ T.pack $ Tar.entryPath e
        , Tar.NormalFile lbs _size <- Tar.entryContent e = do
            (fromCabalPackageName -> name, fromCabalVersion -> version) <- parseNameVersion front
            yield UnparsedCabalFile
                { ucfName = name
                , ucfVersion = version
                , ucfParse = goContent (Tar.entryPath e) name version lbs
                , ucfLbs = lbs
                , ucfEntry = e
                }
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

-- | Get all of the latest descriptions for name/version pairs matching the
-- given criterion.
getLatestDescriptions :: (MonadIO m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, MonadThrow m)
                      => EnvOverride
                      -> (PackageName -> Version -> Bool)
                      -> (GenericPackageDescription -> IO desc)
                      -> m (Map PackageName desc)
getLatestDescriptions menv f parseDesc = do
    m <- sourcePackageIndex menv $$ CL.filter f' =$ CL.fold add mempty
    liftIO $ forM m $ \ucf -> ucfParse ucf >>= parseDesc
  where
    f' ucf = f (ucfName ucf) (ucfVersion ucf)
    add m ucf =
        case Map.lookup name m of
            Just ucf' | ucfVersion ucf < ucfVersion ucf' -> m
            _ -> Map.insert name ucf m
      where
        name = ucfName ucf

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
  | Couldn'tFindPackages (Set PackageName)
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
    return $ Set.fromList $ mapMaybe go idents
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
    let fp = toFilePath $ configPackageIndexCache config
    eres <- liftIO $ tryIO $ removeFile fp
    case eres of
        Left e -> $logDebug $ "Could not delete cache: " <> T.pack (show e)
        Right () -> $logDebug $ "Deleted index cache at " <> T.pack fp
