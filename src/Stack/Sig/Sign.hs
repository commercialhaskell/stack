{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{-|
Module      : Stack.Sig.Sign
Description : Signing Packages
Copyright   : (c) 2015-2018, Stack contributors
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Sign (sign, signPackage, signTarBytes) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import           Stack.Prelude
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy as L
import           Network.HTTP.Client (RequestBody (RequestBodyBS))
import           Network.HTTP.Download
import           Network.HTTP.Simple (setRequestMethod, setRequestBody, getResponseStatusCode)
import           Network.HTTP.Types (methodPut)
import           Path
import           Stack.Package
import           Stack.Sig.GPG
import           Stack.Types.PackageIdentifier
import           Stack.Types.Sig
import qualified System.FilePath as FP

-- | Sign a haskell package with the given url of the signature
-- service and a path to a tarball.
sign
    :: HasLogFunc env
    => String -> Path Abs File -> RIO env Signature
sign url filePath =
    withRunInIO $ \run ->
    withSystemTempDir
        "stack"
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
                         run (signPackage url pkg filePath))
  where
    extractCabalFile tempDir (Tar.Next entry entries) =
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
    :: HasLogFunc env
    => String -> Path Rel File -> L.ByteString -> RIO env Signature
signTarBytes url tarPath bs =
    withSystemTempDir
        "stack"
        (\tempDir ->
              do let tempTarBall = tempDir </> tarPath
                 liftIO (L.writeFile (toFilePath tempTarBall) bs)
                 sign url tempTarBall)

-- | Sign a haskell package given the url to the signature service, a
-- @PackageIdentifier@ and a file path to the package on disk.
signPackage
    :: HasLogFunc env
    => String -> PackageIdentifier -> Path Abs File -> RIO env Signature
signPackage url pkg filePath = do
    sig@(Signature signature) <- gpgSign filePath
    let (PackageIdentifier name version) = pkg
    fingerprint <- gpgVerify sig filePath
    let fullUrl =
            url <> "/upload/signature/" <> show name <> "/" <> show version <>
            "/" <>
            show fingerprint
    req <- parseUrlThrow fullUrl
    let put = setRequestMethod methodPut
            $ setRequestBody (RequestBodyBS signature) req
    res <- liftIO (httpLbs put)
    when
        (getResponseStatusCode res /= 200)
        (throwM (GPGSignException "unable to sign & upload package"))
    logInfo ("Signature uploaded to " <> fromString fullUrl)
    return sig
