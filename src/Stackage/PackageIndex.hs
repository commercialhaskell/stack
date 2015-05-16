{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Package index handling.

module Stackage.PackageIndex
       (PackageIndex(..), getPkgIndex, loadPkgIndex, updateIndex,
        getPkgVersions)
       where

import qualified Codec.Archive.Tar as Tar
import Control.Exception (Exception)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
       (MonadLogger, logWarn, logInfo, logDebug)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString.Lazy as L
import Data.Conduit (($$+-),($$),($=))
import Data.Conduit.Binary (sourceLbs, sourceFile, sinkFile)
import qualified Data.Conduit.Binary as C
import Data.Conduit.Zlib (ungzip)
import Data.Foldable (forM_)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString(fromString))
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Network.HTTP.Conduit
       (Request(checkStatus, requestHeaders),
        Response(responseBody, responseHeaders, responseStatus), parseUrl,
        withManager, http)
import Network.HTTP.Types (status200)
import Path
       (Path, Abs, Dir, toFilePath, parseAbsDir, parseAbsFile, mkRelFile,
        mkRelDir, (</>))
import Stackage.IO (tryIO)
import Stackage.PackageName (PackageName, packageNameString)
import Stackage.PackageVersion
       (PackageVersion, parsePackageVersionFromString)
import Stackage.Process (runIn)
import System.Directory
       (removeFile, renameFile, getAppUserDataDirectory, findExecutable,
        doesFileExist, doesDirectoryExist)
import System.IO (IOMode(ReadMode), withBinaryFile)

data PackageIndexException =
  Couldn'tReadIndexTarball FilePath
                           Tar.FormatError
  deriving (Show,Typeable)
instance Exception PackageIndexException

-- | Wrapper to an existant package index.
newtype PackageIndex =
  PackageIndex (Path Abs Dir)

-- | Try to get the package index.
getPkgIndex :: (MonadIO m,MonadLogger m,MonadThrow m)
            => (Path Abs Dir) -> m (Maybe PackageIndex)
getPkgIndex dir =
  do exists <-
       (liftIO . doesDirectoryExist . toFilePath) dir
     return (if exists
                then Just (PackageIndex dir)
                else Nothing)

-- | Load the package index, if it does not exist, download it.
loadPkgIndex :: (MonadBaseControl IO m,MonadIO m,MonadLogger m,MonadResource m,MonadThrow m)
             => Path Abs Dir -> m PackageIndex
loadPkgIndex dir =
  do maybeIdx <- getPkgIndex dir
     case maybeIdx of
       Just idx -> return idx
       Nothing ->
         do let idx = (PackageIndex dir)
            updateIndex idx
            return idx

-- | Update the index tarball
updateIndex :: (MonadBaseControl IO m,MonadIO m,MonadLogger m,MonadResource m,MonadThrow m)
            => PackageIndex -> m ()
updateIndex idx =
  do git <- isGitInstalled
     if git
        then updateIndexGit idx
        else updateIndexHTTP idx

-- | Update the index Git repo and the index tarball
updateIndexGit :: (MonadIO m,MonadLogger m,MonadThrow m)
               => PackageIndex -> m ()
updateIndexGit (PackageIndex idxPath) =
  do path <- liftIO (findExecutable "git")
     case path of
       Nothing ->
         error "Please install git and provide the executable on your PATH"
       Just fp ->
         do gitPath <- parseAbsFile fp
            $logWarn "FIXME: USING LOCAL DEFAULTS FOR URL & PATH"
            let repoName =
                  $(mkRelDir "all-cabal-files")
                cloneArgs =
                  ["clone"
                  ,pkgIndexGitUriDefault
                  ,(toFilePath repoName)
                  ,"--depth"
                  ,"1"
                  ,"-b"
                  ,"display"]
            sDir <-
              liftIO (parseAbsDir =<<
                      getAppUserDataDirectory "stackage")
            let suDir =
                  sDir </>
                  $(mkRelDir "update")
                acfDir = suDir </> repoName
            repoExists <-
              liftIO (doesDirectoryExist (toFilePath acfDir))
            unless repoExists
                   (do $logInfo ("Cloning repository for first from " <>
                                 T.pack pkgIndexGitUriDefault)
                       runIn suDir gitPath cloneArgs Nothing)
            runIn acfDir gitPath ["fetch","--tags","--depth=1"] Nothing
            let tarFile =
                  idxPath </>
                  $(mkRelFile "00-index.tar")
            _ <-
              (liftIO . tryIO) (removeFile (toFilePath tarFile))
            $logWarn "FIXME: WE DONT YET HAVE FLAG|SETTING|DEFAULT FOR GIT GPG VALIDATION"
            when False
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
updateIndexHTTP :: (MonadBaseControl IO m,MonadIO m,MonadLogger m,MonadResource m,MonadThrow m)
                => PackageIndex -> m ()
updateIndexHTTP (PackageIndex idxPath) =
  do $logWarn "FIXME: USING LOCAL DEFAULTS FOR URL"
     let tarPath =
           idxPath </>
           $(mkRelFile "00-index.tar")
         tarFilePath = toFilePath tarPath
         tarGzPath =
           idxPath </>
           $(mkRelFile "00-index.tar.gz")
         tarGzFilePath = toFilePath tarGzPath
         tmpTarPath =
           idxPath </>
           $(mkRelFile "00-index.tar.gz.tmp")
         tmpTarFilePath = toFilePath tmpTarPath
         etagPath =
           idxPath </>
           $(mkRelFile "00-index.tar.gz.etag")
         etagFilePath = toFilePath etagPath
     req <- parseUrl pkgIndexHttpUriDefault
     $logDebug ("Downloading package index from " <>
                T.pack pkgIndexHttpUriDefault)
     etagFileExists <-
       liftIO (doesFileExist etagFilePath)
     if (etagFileExists)
        then do etag <-
                  sourceFile etagFilePath $$
                  C.take 512
                let req' =
                      req {requestHeaders =
                             requestHeaders req ++
                             [("If-None-Match",L.toStrict etag)]}
                download req' tmpTarFilePath tarGzFilePath tarFilePath etagFilePath
        else download req tmpTarFilePath tarGzFilePath tarFilePath etagFilePath
     $logWarn "FIXME: WE CAN'T RUN GIT GPG SIGNATURE VERIFICATION WITHOUT GIT"
  where download req tmpTarGzFp tarGzFp tarFp etagFP =
          withManager
            (\mgr ->
               do res <-
                    http (req {checkStatus = \_ _ _ -> Nothing}) mgr
                  when (responseStatus res == status200)
                       (do let etag =
                                 lookup "ETag" (responseHeaders res)
                           forM_ etag
                                 (\e ->
                                    sourceLbs (L.fromStrict e) $$
                                    sinkFile etagFP)
                           responseBody res $$+-
                             sinkFile (fromString tmpTarGzFp)
                           sourceFile (fromString tmpTarGzFp) $$
                             ungzip $=
                             sinkFile (fromString tarFp)
                           liftIO (renameFile tmpTarGzFp tarGzFp)))

-- | Fetch all the package versions for a given package
getPkgVersions :: (MonadIO m,MonadLogger m,MonadThrow m)
               => PackageIndex -> PackageName -> m (Maybe (Set PackageVersion))
getPkgVersions (PackageIndex idxPath) pkg =
  do let tarPath = idxPath </> $(mkRelFile "00-index.tar")
         tarFilePath = toFilePath tarPath
     $logWarn "FIXME: USING LOCAL DEFAULTS FOR URL & PATH"
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
                do parsedVer <- parsePackageVersionFromString ver
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

-- | Temporarily define the standard Git repo url locally here in this
-- module until we work out the Stackage.Config for it.
pkgIndexGitUriDefault :: String
pkgIndexGitUriDefault = "https://github.com/commercialhaskell/all-cabal-files.git"

-- | Temporarily define the standard HTTP tarball url locally here in
-- this module until we work out the Stackage.Config for it.
pkgIndexHttpUriDefault :: String
pkgIndexHttpUriDefault = "https://s3.amazonaws.com/hackage.fpcomplete.com/00-index.tar.gz"
