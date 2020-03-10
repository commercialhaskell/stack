{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
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
  , verifiedDownloadWithProgress
  , CheckHexDigest (..)
  , DownloadRequest
  , drRetryPolicyDefault
  , VerifiedDownloadException (..)
  , HashCheck (..)
  , mkDownloadRequest
  , setHashChecks
  , setLengthCheck
  , setRetryPolicy
  , setForceDownload
  ) where

import           Control.Monad.State (get, put, modify)
import           Data.Aeson (FromJSON)
import qualified Data.ByteString as Strict
import           Data.Conduit (ConduitM, ConduitT, awaitForever, (.|), yield, await)
import           Data.Conduit.Lift (evalStateC)
import qualified Data.Conduit.List as CL
import           Data.Monoid (Sum (..))
import qualified Data.Text as T
import           Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
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
import           Prelude (until, (!!))
import           RIO
import           RIO.PrettyPrint
import           Text.Printf (printf)


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
    dr' = modifyRequest setUserAgent dr

verifiedDownloadWithProgress
  :: HasTerm env
  => DownloadRequest
  -> Path Abs File
  -> Text
  -> Maybe Int
  -> RIO env Bool
verifiedDownloadWithProgress req destpath lbl msize =
  verifiedDownload req destpath (chattyDownloadProgress lbl msize)

chattyDownloadProgress
  :: ( HasLogFunc env
     , MonadIO m
     , MonadReader env m
     )
  => Text
  -> Maybe Int
  -> f
  -> ConduitT ByteString c m ()
chattyDownloadProgress label mtotalSize _ = do
    _ <- logSticky $ RIO.display label <> ": download has begun"
    CL.map (Sum . Strict.length)
      .| chunksOverTime 1
      .| go
  where
    go = evalStateC 0 $ awaitForever $ \(Sum size) -> do
        modify (+ size)
        totalSoFar <- get
        logSticky $ fromString $
            case mtotalSize of
                Nothing -> chattyProgressNoTotal totalSoFar
                Just 0 -> chattyProgressNoTotal totalSoFar
                Just totalSize -> chattyProgressWithTotal totalSoFar totalSize

    -- Example: ghc: 42.13 KiB downloaded...
    chattyProgressNoTotal totalSoFar =
        printf ("%s: " <> bytesfmt "%7.2f" totalSoFar <> " downloaded...")
                (T.unpack label)

    -- Example: ghc: 50.00 MiB / 100.00 MiB (50.00%) downloaded...
    chattyProgressWithTotal totalSoFar total =
      printf ("%s: " <>
              bytesfmt "%7.2f" totalSoFar <> " / " <>
              bytesfmt "%.2f" total <>
              " (%6.2f%%) downloaded...")
              (T.unpack label)
              percentage
      where percentage :: Double
            percentage = fromIntegral totalSoFar / fromIntegral total * 100

-- | Given a printf format string for the decimal part and a number of
-- bytes, formats the bytes using an appropriate unit and returns the
-- formatted string.
--
-- >>> bytesfmt "%.2" 512368
-- "500.359375 KiB"
bytesfmt :: Integral a => String -> a -> String
bytesfmt formatter bs = printf (formatter <> " %s")
                               (fromIntegral (signum bs) * dec :: Double)
                               (bytesSuffixes !! i)
  where
    (dec,i) = getSuffix (abs bs)
    getSuffix n = until p (\(x,y) -> (x / 1024, y+1)) (fromIntegral n,0)
      where p (n',numDivs) = n' < 1024 || numDivs == (length bytesSuffixes - 1)
    bytesSuffixes :: [String]
    bytesSuffixes = ["B","KiB","MiB","GiB","TiB","PiB","EiB","ZiB","YiB"]

-- Await eagerly (collect with monoidal append),
-- but space out yields by at least the given amount of time.
-- The final yield may come sooner, and may be a superfluous mempty.
-- Note that Integer and Float literals can be turned into NominalDiffTime
-- (these literals are interpreted as "seconds")
chunksOverTime :: (Monoid a, Semigroup a, MonadIO m) => NominalDiffTime -> ConduitM a a m ()
chunksOverTime diff = do
    currentTime <- liftIO getCurrentTime
    evalStateC (currentTime, mempty) go
  where
    -- State is a tuple of:
    -- * the last time a yield happened (or the beginning of the sink)
    -- * the accumulated awaits since the last yield
    go = await >>= \case
      Nothing -> do
        (_, acc) <- get
        yield acc
      Just a -> do
        (lastTime, acc) <- get
        let acc' = acc <> a
        currentTime <- liftIO getCurrentTime
        if diff < diffUTCTime currentTime lastTime
          then put (currentTime, mempty) >> yield acc'
          else put (lastTime,    acc')
        go
