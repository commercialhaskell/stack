{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Stack.Sig.Sign
Description : Signing Packages
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Sign (sign, signTarBytes) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import           Control.Monad (when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy as L
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.UUID (toString)
import           Data.UUID.V4 (nextRandom)
import           Network.HTTP.Conduit
       (Response(..), RequestBody(..), Request(..), httpLbs, newManager,
        tlsManagerSettings)
import           Network.HTTP.Download
import           Network.HTTP.Types (status200, methodPut)
import           Path
import           Path.IO
import           Stack.Package
import qualified Stack.Sig.GPG as GPG
import           Stack.Types
import qualified System.FilePath as FP

-- | Sign a haskell package with the given url of the signature
-- service and a path to a tarball.
sign
    :: (MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadMask m, MonadLogger m, MonadThrow m, MonadReader env m, HasConfig env)
    => Maybe (Path Abs Dir) -> String -> Path Abs File -> m ()
sign Nothing _ _ = throwM SigNoProjectRootException
sign (Just projectRoot) url filePath = do
    withStackWorkTempDir
        projectRoot
        (\tempDir ->
              do bytes <-
                     liftIO
                         (fmap
                              GZip.decompress
                              (BS.readFile (toFilePath filePath)))
                 maybePath <- extractCabalFile tempDir (Tar.read bytes)
                 case maybePath of
                     Nothing -> throwM SigInvalidSDistTarBall
                     Just cabalPath -> do
                         pkg <- cabalFilePackageId (tempDir </> cabalPath)
                         signPackage url pkg filePath)
  where
    extractCabalFile tempDir (Tar.Next entry entries) = do
        case Tar.entryContent entry of
            (Tar.NormalFile lbs _) ->
                case FP.splitFileName (Tar.entryPath entry) of
                    (folder,file)
                      | length (FP.splitDirectories folder) == 1 &&
                            FP.takeExtension file == ".cabal" -> do
                          cabalFile <- parseRelFile file
                          liftIO
                              (BS.writeFile
                                   (toFilePath (tempDir </> cabalFile))
                                   lbs)
                          return (Just cabalFile)
                    (_,_) -> extractCabalFile tempDir entries
            _ -> extractCabalFile tempDir entries
    extractCabalFile _ _ = return Nothing

-- | Sign a haskell package with the given url to the signature
-- service, a package tarball path (package tarball name) and a lazy
-- bytestring of bytes that represent the tarball bytestream.  The
-- function will write the bytes to the path in a temp dir and sign
-- the tarball with GPG.
signTarBytes
    :: (MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadMask m, MonadLogger m, MonadThrow m, MonadReader env m, HasConfig env)
    => Maybe (Path Abs Dir) -> String -> Path Rel File -> L.ByteString -> m ()
signTarBytes Nothing _ _ _ = throwM SigNoProjectRootException
signTarBytes (Just projectRoot) url tarPath bs =
    withStackWorkTempDir
        projectRoot
        (\tempDir ->
              do let tempTarBall = tempDir </> tarPath
                 liftIO (L.writeFile (toFilePath tempTarBall) bs)
                 sign (Just projectRoot) url tempTarBall)

-- | Sign a haskell package given the url to the signature service, a
-- @PackageIdentifier@ and a file path to the package on disk.
signPackage
    :: (MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadMask m, MonadLogger m, MonadThrow m)
    => String -> PackageIdentifier -> Path Abs File -> m ()
signPackage url pkg filePath = do
    $logInfo ("GPG signing " <> T.pack (toFilePath filePath))
    sig@(Signature signature) <- GPG.signPackage filePath
    let (PackageIdentifier n v) = pkg
        name = show n
        version = show v
    verify <- GPG.verifyFile sig filePath
    fingerprint <- GPG.fullFingerprint verify
    req <-
        parseUrl
            (url <> "/upload/signature/" <> name <> "/" <> version <> "/" <>
             T.unpack (fingerprintSample fingerprint))
    let put =
            req
            { method = methodPut
            , requestBody = RequestBodyBS signature
            }
    mgr <- liftIO (newManager tlsManagerSettings)
    res <- liftIO (httpLbs put mgr)
    when
        (responseStatus res /= status200)
        (throwM (GPGSignException "unable to sign & upload package"))

withStackWorkTempDir
    :: (MonadCatch m, MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env)
    => Path Abs Dir -> (Path Abs Dir -> m ()) -> m ()
withStackWorkTempDir projectRoot f = do
    uuid <- liftIO nextRandom
    uuidPath <- parseRelDir (toString uuid)
    workDir <- getWorkDir
    let tempDir = projectRoot </> workDir </> $(mkRelDir "tmp") </> uuidPath
    bracket
        (ensureDir tempDir)
        (const (removeDirRecur tempDir))
        (const (f tempDir))
