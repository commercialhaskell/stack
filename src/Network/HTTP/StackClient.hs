{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Wrapper functions of 'Network.HTTP.Simple' and 'Network.HTTP.Client' to
-- add the 'User-Agent' HTTP request header to each request.

module Network.HTTP.StackClient
  ( httpJSON
  , httpLbs
  , httpNoBody
  , httpSink
  , withResponse
  , setRequestMethod
  , setRequestHeader
  , addRequestHeader
  , setRequestBody
  , getResponseHeaders
  , getResponseBody
  , getResponseStatusCode
  , parseRequest
  , getUri
  , path
  , checkResponse
  , parseUrlThrow
  , requestHeaders
  , getGlobalManager
  , applyDigestAuth
  , displayDigestAuthException
  , Request
  , RequestBody(RequestBodyBS, RequestBodyLBS)
  , Response
  , HttpException
  , hAccept
  , hContentLength
  , hContentMD5
  , methodPut
  , formDataBody
  , partFileRequestBody
  , partBS
  , partLBS
  , setGithubHeaders
  , download
  , redownload
  , verifiedDownload
  , CheckHexDigest (..)
  , DownloadRequest (..)
  , drRetryPolicyDefault
  , DownloadException (..)
  , HashCheck (..)
  ) where

import           Data.Aeson (FromJSON)
import qualified Data.ByteString as Strict
import           Data.Conduit (ConduitM)
import           Data.Void (Void)
import           Network.HTTP.Client (Request, RequestBody(..), Response, parseRequest, getUri, path, checkResponse, parseUrlThrow)
import           Network.HTTP.Simple (setRequestMethod, setRequestBody, setRequestHeader, addRequestHeader, HttpException(..), getResponseBody, getResponseStatusCode, getResponseHeaders)
import           Network.HTTP.Types (hAccept, hContentLength, hContentMD5, methodPut)
import           Network.HTTP.Conduit (requestHeaders)
import           Network.HTTP.Client.TLS (getGlobalManager, applyDigestAuth, displayDigestAuthException)
import           Network.HTTP.Download hiding (download, redownload, verifiedDownload)
import qualified Network.HTTP.Download as Download
import qualified Network.HTTP.Simple
import           Network.HTTP.Client.MultipartFormData (formDataBody, partFileRequestBody, partBS, partLBS)
import           Path
import           RIO
import           RIO.PrettyPrint


setUserAgent :: Request -> Request
setUserAgent = setRequestHeader "User-Agent" ["The Haskell Stack"]


httpJSON :: (MonadIO m, FromJSON a) => Request -> m (Response a)
httpJSON = Network.HTTP.Simple.httpJSON . setUserAgent


httpLbs :: MonadIO m => Request -> m (Response LByteString)
httpLbs = Network.HTTP.Simple.httpLbs . setUserAgent


httpNoBody :: MonadIO m => Request -> m (Response ())
httpNoBody = Network.HTTP.Simple.httpNoBody . setUserAgent


httpSink
  :: MonadUnliftIO m
  => Request
  -> (Response () -> ConduitM Strict.ByteString Void m a)
  -> m a
httpSink = Network.HTTP.Simple.httpSink . setUserAgent


withResponse
  :: (MonadUnliftIO m, MonadIO n)
  => Request -> (Response (ConduitM i Strict.ByteString n ()) -> m a) -> m a
withResponse = Network.HTTP.Simple.withResponse . setUserAgent

-- | Set the user-agent request header
setGithubHeaders :: Request -> Request
setGithubHeaders = setRequestHeader "Accept" ["application/vnd.github.v3+json"]

-- | Download the given URL to the given location. If the file already exists,
-- no download is performed. Otherwise, creates the parent directory, downloads
-- to a temporary file, and on file download completion moves to the
-- appropriate destination.
--
-- Throws an exception if things go wrong
download :: HasTerm env
         => Request
         -> Path Abs File -- ^ destination
         -> RIO env Bool -- ^ Was a downloaded performed (True) or did the file already exist (False)?
download req dest = Download.download (setUserAgent req) dest

-- | Same as 'download', but will download a file a second time if it is already present.
--
-- Returns 'True' if the file was downloaded, 'False' otherwise
redownload :: HasTerm env
           => Request
           -> Path Abs File -- ^ destination
           -> RIO env Bool
redownload req dest = Download.redownload (setUserAgent req) dest

-- | Copied and extended version of Network.HTTP.Download.download.
--
-- Has the following additional features:
-- * Verifies that response content-length header (if present)
--     matches expected length
-- * Limits the download to (close to) the expected # of bytes
-- * Verifies that the expected # bytes were downloaded (not too few)
-- * Verifies md5 if response includes content-md5 header
-- * Verifies the expected hashes
--
-- Throws VerifiedDownloadException.
-- Throws IOExceptions related to file system operations.
-- Throws HttpException.
verifiedDownload
         :: HasTerm env
         => DownloadRequest
         -> Path Abs File -- ^ destination
         -> (Maybe Integer -> ConduitM ByteString Void (RIO env) ()) -- ^ custom hook to observe progress
         -> RIO env Bool -- ^ Whether a download was performed
verifiedDownload dr destpath progressSink =
    Download.verifiedDownload dr' destpath progressSink
  where
    dr' = dr {drRequest = setUserAgent (drRequest dr)}
