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
    , findNewestVersions
    , UnparsedCabalFile (..)
    , getLatestDescriptions
    , updateIndex
    , requireIndex
    , getPkgVersions
    ) where

import qualified Codec.Archive.Tar                     as Tar
import           Control.Exception                     (Exception)
import           Control.Exception.Enclosed            (tryIO)
import           Control.Monad                         (unless, when)
import           Control.Monad.Catch                   (MonadThrow, throwM)
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Logger                  (MonadLogger, logDebug,
                                                        logInfo, logWarn)
import           Control.Monad.Reader                  (runReaderT)
import           Control.Monad.Trans.Control           (MonadBaseControl)
import           Control.Monad.Trans.Resource          (runResourceT)
import qualified Data.ByteString.Lazy                  as L
import           Data.Conduit                          (($$), (=$), yield, Producer)
import           Data.Conduit.Binary                   (sinkHandle,
                                                        sourceHandle)
import qualified Data.Conduit.List                     as CL
import           Data.Conduit.Zlib                     (ungzip)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Maybe                            (fromMaybe, isJust)
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
                                                        parseAbsFile,
                                                        parseRelDir, toFilePath,
                                                        (</>))
import           Stack.Types
import           System.Directory
import           System.FilePath                       (takeBaseName, (<.>))
import           System.IO                             (IOMode (ReadMode, WriteMode),
                                                        withBinaryFile)
import           System.Process.Read                   (runIn)

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

-- | Find the newest versions of all given package names.
findNewestVersions :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env)
                   => [PackageName]
                   -> m [PackageIdentifier]
findNewestVersions names0 = do
    (m, discovered) <- sourcePackageIndex $$ CL.fold add (Map.empty, Set.empty)
    let missing = Set.difference names discovered
    if Set.null missing
        then return $ map fromTuple $ Map.toList m
        else throwM $ Couldn'tFindPackages missing
  where
    names = Set.fromList names0

    add orig@(m, discovered) ucf
        | n `Set.member` names =
            let !m' = Map.insertWith max n v m
                !d' = Set.insert n discovered
             in (m', d')
        | otherwise = orig
      where
        n = ucfName ucf
        v = ucfVersion ucf

-- | Stream all of the cabal files from the 00-index tar file.
sourcePackageIndex :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env)
                   => Producer m UnparsedCabalFile
sourcePackageIndex = do
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
getLatestDescriptions :: (MonadIO m, MonadReader env m, HasConfig env)
                      => (PackageName -> Version -> Bool)
                      -> (GenericPackageDescription -> IO desc)
                      -> m (Map PackageName desc)
getLatestDescriptions f parseDesc = do
    env <- ask
    liftIO $ do
        m <- flip runReaderT env
                $ runResourceT
                $ sourcePackageIndex $$ CL.filter f' =$ CL.fold add mempty
        forM m $ \ucf -> liftIO $ ucfParse ucf >>= parseDesc
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
requireIndex :: (MonadBaseControl IO m,MonadIO m,MonadLogger m
                ,MonadThrow m,MonadReader env m,HasHttpManager env
                ,HasConfig env)
             => m ()
requireIndex = do
    config <- askConfig
    let tarFile = configPackageIndex config
    exists <- liftIO $ doesFileExist $ toFilePath tarFile
    unless exists updateIndex

-- | Update the index tarball
updateIndex :: (MonadBaseControl IO m,MonadIO m,MonadLogger m
               ,MonadThrow m,MonadReader env m,HasHttpManager env
               ,HasConfig env)
            => m ()
updateIndex =
  do $logInfo "Updating package index ..."
     git <- isGitInstalled
     if git
        then updateIndexGit
        else updateIndexHTTP

-- | Update the index Git repo and the index tarball
updateIndexGit :: (MonadIO m,MonadLogger m,MonadThrow m,MonadReader env m,HasConfig env)
               => m ()
updateIndexGit = do
     config <- askConfig
     let tarFile = configPackageIndex config
         idxPath = parent tarFile
     liftIO (createDirectoryIfMissing True (toFilePath idxPath))
     path <- liftIO (findExecutable "git")
     case path of
       Nothing ->
         error "Please install git and provide the executable on your PATH"
       Just fp ->
         do gitPath <- parseAbsFile fp
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
                       runIn suDir gitPath cloneArgs Nothing)
            runIn acfDir gitPath ["fetch","--tags","--depth=1"] Nothing
            _ <-
              (liftIO . tryIO) (removeFile (toFilePath tarFile))
            when (configGpgVerifyIndex config)
                 (do runIn acfDir
                           gitPath
                           ["tag","-v","current-hackage"]
                           (Just (unlines ["Signature verification failed. "
                                          ,"Please ensure you've set up your"
                                          ,"GPG keychain to accept the D6CF60FD signing key."
                                          ,"For more information, see:"
                                          ,"https://github.com/fpco/stackage-update#readme"])))
            $logDebug ("Exporting a tarball to " <>
                       (T.pack . toFilePath) tarFile)
            runIn acfDir
                  gitPath
                  ["archive"
                  ,"--format=tar"
                  ,"-o"
                  ,toFilePath tarFile
                  ,"current-hackage"]
                  Nothing

-- | Update the index tarball via HTTP
updateIndexHTTP :: (MonadBaseControl IO m,MonadIO m,MonadLogger m
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
getPkgVersions :: (MonadIO m,MonadLogger m,MonadThrow m,MonadReader env m,HasConfig env)
               => PackageName -> m (Maybe (Set Version))
getPkgVersions pkg =
  do config <- askConfig
     let tarFilePath = toFilePath $ configPackageIndex config
     $logDebug ("Iterating through tarball " <> T.pack tarFilePath)
     liftIO (withBinaryFile
               tarFilePath
               ReadMode
               (\h ->
                  do lbs <- L.hGetContents h
                     vers <-
                       liftIO (iterateTarball tarFilePath
                                              (packageNameString pkg)
                                              Set.empty
                                              (Tar.read lbs))
                     case vers of
                       set
                         | Set.empty == set ->
                           return Nothing
                       set -> return (Just set)))
  where iterateTarball tarPath name vers (Tar.Next e es) =
          case (getNameAndVersion (Tar.entryPath e),Tar.entryContent e) of
            (Just (name',ver),_)
              | name' == name ->
                do parsedVer <- parseVersionFromString ver
                   iterateTarball tarPath
                                  name
                                  (Set.insert parsedVer vers)
                                  es
            _ ->
              iterateTarball tarPath name vers es
        iterateTarball tarPath _ _ (Tar.Fail e) =
          throwM (Couldn'tReadIndexTarball tarPath e)
        iterateTarball _ _ vers Tar.Done = return vers
        getNameAndVersion name =
          case T.splitOn "/" (T.pack name) of
            [n,v,fp]
              | T.stripSuffix ".json" fp ==
                  Just n ->
                Just (T.unpack n,T.unpack v)
            _ -> Nothing

-- | Is the git executable installed?
isGitInstalled :: MonadIO m
               => m Bool
isGitInstalled =
  return . isJust =<<
  liftIO (findExecutable "git")
