{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell  #-}
module Network.HTTP.Download
    ( verifiedDownload
    , DownloadRequest(..)
    , drRetryPolicyDefault
    , HashCheck(..)
    , DownloadException(..)
    , CheckHexDigest(..)
    , LengthCheck
    , VerifiedDownloadException(..)

    , download
    , redownload
    , httpJSON
    , parseRequest
    , parseUrlThrow
    , setGithubHeaders
    ) where

import           Control.Exception           (Exception)
import           Control.Exception.Safe      (handleIO)
import           Control.Monad               (void)
import           Control.Monad.Catch         (throwM)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (MonadLogger, logDebug)
import qualified Data.ByteString.Lazy        as L
import           Data.Conduit                (runConduit, runConduitRes, (.|), yield)
import           Data.Conduit.Binary         (sourceHandle)
import qualified Data.Conduit.Binary         as CB
import           Data.Foldable               (forM_)
import           Data.Monoid                 ((<>))
import           Data.Text.Encoding.Error    (lenientDecode)
import           Data.Text.Encoding          (decodeUtf8With)
import           Data.Typeable               (Typeable)
import           Network.HTTP.Client         (Request, Response, path, checkResponse, parseUrlThrow, parseRequest)
import           Network.HTTP.Client.Conduit (requestHeaders)
import           Network.HTTP.Download.Verified
import           Network.HTTP.Simple         (httpJSON, withResponse, getResponseBody, getResponseHeaders, getResponseStatusCode,
                                              setRequestHeader)
import           Path                        (Abs, File, Path, toFilePath)
import           System.Directory            (createDirectoryIfMissing,
                                              removeFile)
import           System.FilePath             (takeDirectory, (<.>))
import           System.IO                   (IOMode (ReadMode),
                                              withBinaryFile)

-- | Download the given URL to the given location. If the file already exists,
-- no download is performed. Otherwise, creates the parent directory, downloads
-- to a temporary file, and on file download completion moves to the
-- appropriate destination.
--
-- Throws an exception if things go wrong
download :: (MonadIO m, MonadLogger m)
         => Request
         -> Path Abs File -- ^ destination
         -> m Bool -- ^ Was a downloaded performed (True) or did the file already exist (False)?
download req destpath = do
    let downloadReq = DownloadRequest
            { drRequest = req
            , drHashChecks = []
            , drLengthCheck = Nothing
            , drRetryPolicy = drRetryPolicyDefault
            }
    let progressHook _ = return ()
    verifiedDownload downloadReq destpath progressHook

-- | Same as 'download', but will download a file a second time if it is already present.
--
-- Returns 'True' if the file was downloaded, 'False' otherwise
redownload :: (MonadIO m, MonadLogger m)
           => Request
           -> Path Abs File -- ^ destination
           -> m Bool
redownload req0 dest = do
    $logDebug $ "Downloading " <> decodeUtf8With lenientDecode (path req0)
    let destFilePath = toFilePath dest
        etagFilePath = destFilePath <.> "etag"

    metag <- liftIO $ handleIO (const $ return Nothing) $ fmap Just $
        withBinaryFile etagFilePath ReadMode $ \h ->
            runConduit $ sourceHandle h .| CB.take 512

    let req1 =
            case metag of
                Nothing -> req0
                Just etag -> req0
                    { requestHeaders =
                        requestHeaders req0 ++
                        [("If-None-Match", L.toStrict etag)]
                    }
        req2 = req1 { checkResponse = \_ _ -> return () }
    liftIO $ recoveringHttp drRetryPolicyDefault $
      withResponse req2 $ \res -> case getResponseStatusCode res of
        200 -> do
          createDirectoryIfMissing True $ takeDirectory destFilePath

          -- Order here is important: first delete the etag, then write the
          -- file, then write the etag. That way, if any step fails, it will
          -- force the download to happen again.
          handleIO (const $ return ()) $ removeFile etagFilePath

          runConduitRes $ getResponseBody res .| CB.sinkFileCautious destFilePath

          forM_ (lookup "ETag" (getResponseHeaders res)) $ \e ->
            runConduitRes $ yield e .| CB.sinkFileCautious etagFilePath

          return True
        304 -> return False
        _ -> throwM $ RedownloadFailed req2 dest $ void res

data DownloadException = RedownloadFailed Request (Path Abs File) (Response ())
    deriving (Show, Typeable)
instance Exception DownloadException

-- | Set the user-agent request header
setGithubHeaders :: Request -> Request
setGithubHeaders = setRequestHeader "User-Agent" ["The Haskell Stack"]
                 . setRequestHeader "Accept" ["application/vnd.github.v3+json"]
