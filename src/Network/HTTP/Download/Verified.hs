{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RankNTypes            #-}
module Network.HTTP.Download.Verified where

import qualified Data.List as List
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Applicative
import Crypto.Hash
import Crypto.Hash.Conduit (sinkHash)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Binary (sourceHandle, sinkHandle)
import Data.Typeable (Typeable)
import Network.HTTP.Client.Conduit
import Network.HTTP.Types.Header (hContentLength, hContentMD5)
import Path
import System.FilePath((<.>))
import System.Directory
import System.IO

-- | A request together with the hash algorithm to use
-- to verify the response.
-- The type parameter specifies the algorithm.
data VerifiedRequest a = VerifiedRequest
    { vrHashAlgorithm :: a
    , vrExpectedHexDigest :: String
    , vrDownloadBytes :: Int
    , vrRequest :: Request
    }
  deriving Show

-- | An exception regarding verification of a download.
data VerifiedDownloadException
    = WrongContentLength
          Int -- expected
          ByteString -- actual (as listed in the header)
    | WrongDigest
          String -- algorithm
          String -- expected
          String -- actual
  deriving (Show, Typeable)
instance Exception VerifiedDownloadException

data VerifyFileException
    = WrongFileSize
          Int -- expected
          Integer -- actual (as listed by hFileSize)
  deriving (Show, Typeable)
instance Exception VerifyFileException

-- | Make sure that the hash digest for a finite stream of bytes
-- is as expected.
--
-- Throws WrongDigest (VerifiedDownloadException)
sinkCheckHash
    :: forall a m. (MonadThrow m, Show a, HashAlgorithm a)
    => a -- ^ The algorithm (e.g. MD5)
    -> String -- ^ The expected digest, rendered as a String (hexadecimal)
    -> Consumer ByteString m ()
sinkCheckHash a expectedDigestString = do
    (digest :: Digest a) <- sinkHash
    let actualDigestString = show digest
    when (actualDigestString /= expectedDigestString) $
        throwM $ WrongDigest (show a) expectedDigestString actualDigestString


-- | Copied and extended version of Network.HTTP.Download.download.
--
-- Has the following additional features:
-- * Verifies that response content-length header (if present)
--     matches expected length
-- * Only downloads expected length # of bytes
-- * Verifies md5 if response includes content-md5 header
-- * Verifies the expected hash
--
-- Further work ideas:
-- * Check existing file for the given length & hash
--     and redownload if it doesn't match
-- * Check the downloaded file isn't too small.
--    (Currently behavior only prevents it from being too large.)
-- * Add a "progress" hook so that long downloads don't look like they've hung.
--
-- Throws VerifiedDownloadException, and whatever else "download" throws.
verifiedDownload :: (HashAlgorithm a, Show a, MonadReader env m, HasHttpManager env, MonadIO m, MonadThrow m)
         => VerifiedRequest a
         -> Path Abs File -- ^ destination
         -> m Bool -- ^ Whether a download was performed
verifiedDownload VerifiedRequest{..} destpath = do
    let req = vrRequest
    env <- ask
    liftIO $ whenM' getShouldDownload $ do
        createDirectoryIfMissing True dir
        withBinaryFile fptmp WriteMode $ \h ->
            flip runReaderT env $
                withResponse req (go h)
        renameFile fptmp fp
  where
    whenM' mp m = do
        p <- mp
        if p then m >> return True else return False

    fp = toFilePath destpath
    fptmp = fp <.> "tmp"
    dir = toFilePath $ parent destpath

    getShouldDownload = do
        fileExists <- doesFileExist fp
        if fileExists
            -- only download if file does not match expectations
            then not <$> fileMatchesExpectations
            -- or if it doesn't exist yet
            else return True

    -- precondition: file exists
    -- TODO: add logging
    fileMatchesExpectations =
        (checkExpectations >> return True)
          `catch` \(_ :: VerifyFileException) -> return False
          `catch` \(_ :: VerifiedDownloadException) -> return False
      where
        checkExpectations = bracket (openFile fp ReadMode) hClose $ \h -> do
            fileSizeInteger <- hFileSize h
            when (fileSizeInteger > toInteger (maxBound :: Int)) $
              throwM $ WrongFileSize vrDownloadBytes fileSizeInteger
            let fileSize = fromInteger fileSizeInteger
            when (fileSize /= vrDownloadBytes) $
              throwM $ WrongFileSize vrDownloadBytes fileSizeInteger
            sourceHandle h $$ getZipSink sinkCheckGivenHash

    sinkCheckGivenHash :: MonadThrow m => ZipSink ByteString m ()
    sinkCheckGivenHash = ZipSink $
      sinkCheckHash vrHashAlgorithm vrExpectedHexDigest

    go h res = do
        let headers = responseHeaders res
        case List.lookup hContentLength headers of
            Just lengthBS -> do
              let lengthText = Text.strip $ Text.decodeUtf8 lengthBS
                  lengthStr = Text.unpack lengthText
              when (lengthStr /= show vrDownloadBytes) $
                throwM $ WrongContentLength vrDownloadBytes lengthBS
            _ -> return ()
        let checkHash = (case List.lookup hContentMD5 headers of
                Just md5BS ->
                    let md5ExpectedHexDigest =  BC.unpack (B64.decodeLenient md5BS)
                    in ZipSink (sinkCheckHash MD5 md5ExpectedHexDigest)
                Nothing ->
                    pure ()
                ) *> sinkCheckGivenHash

        responseBody res
            $= CB.isolate vrDownloadBytes
            -- TODO: $= progressHook
            $$ getZipSink (checkHash *> ZipSink (sinkHandle h))
