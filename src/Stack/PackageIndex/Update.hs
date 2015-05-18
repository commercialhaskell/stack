{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Package index handling.

module Stack.PackageIndex.Update
       (updateIndex,
        getPkgVersions)
       where

import qualified Codec.Archive.Tar as Tar
import           Control.Exception (Exception)
import           Control.Monad (unless, when)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger
       (MonadLogger, logWarn, logInfo, logDebug)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString.Lazy as L
import           Data.Conduit (($$), (=$))
import           Data.Conduit.Binary (sourceHandle, sinkHandle)
import           Data.Conduit.Zlib (ungzip)
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Network.HTTP.Download
import           Path
       (toFilePath, parseRelDir, parseAbsFile,
        mkRelDir, (</>), parent)
import           Control.Exception.Enclosed (tryIO)
import           Stack.Types
import           System.Process.Read (runIn)
import           Stack.Config
import           System.Directory
import           System.FilePath (takeBaseName, (<.>))
import           System.IO (IOMode(ReadMode, WriteMode), withBinaryFile)

data PackageIndexException =
  Couldn'tReadIndexTarball FilePath
                           Tar.FormatError
  deriving (Show,Typeable)
instance Exception PackageIndexException

-- | Update the index tarball
updateIndex :: (MonadBaseControl IO m,MonadIO m,MonadLogger m,MonadResource m
               ,MonadThrow m,MonadReader env m,HasHttpManager env
               ,HasConfig env)
            => m ()
updateIndex =
  do git <- isGitInstalled
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
            let sDir = configStackageRoot config
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
updateIndexHTTP :: (MonadBaseControl IO m,MonadIO m,MonadLogger m,MonadResource m
                   ,MonadThrow m,MonadReader env m,HasHttpManager env,HasConfig env)
                => m ()
updateIndexHTTP = do
    config <- askConfig
    url <- askPackageIndexHttpUrl
    req <- parseUrl $ T.unpack url
    $logDebug ("Downloading package index from " <> url)
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
