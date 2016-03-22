{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
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
    , downloadJSON
    , parseUrl
    , liftHTTP
    , ask
    , getHttpManager
    , MonadReader
    , HasHttpManager
    ) where

import           Control.Exception           (Exception)
import           Control.Exception.Enclosed  (handleIO)
import           Control.Monad               (void)
import           Control.Monad.Catch         (MonadThrow, MonadMask, throwM)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, ReaderT, ask,
                                              runReaderT)
import           Data.Aeson.Extended         (FromJSON, parseJSON)
import           Data.Aeson.Parser           (json')
import           Data.Aeson.Types            (parseEither)
import qualified Data.ByteString             as S
import qualified Data.ByteString.Lazy        as L
import           Data.Conduit                (($$))
import           Data.Conduit.Attoparsec     (sinkParser)
import           Data.Conduit.Binary         (sinkHandle, sourceHandle)
import qualified Data.Conduit.Binary         as CB
import           Data.Foldable               (forM_)
import           Data.Typeable               (Typeable)
import           Network.HTTP.Client.Conduit (HasHttpManager, Manager, Request,
                                              Response, checkStatus,
                                              getHttpManager, parseUrl,
                                              requestHeaders, responseBody,
                                              responseHeaders, responseStatus,
                                              withResponse)
import           Network.HTTP.Download.Verified
import           Network.HTTP.Types          (status200, status304)
import           Path                        (Abs, File, Path, toFilePath)
import           System.Directory            (createDirectoryIfMissing,
                                              removeFile,
                                              renameFile)
import           System.FilePath             (takeDirectory, (<.>))
import           System.IO                   (IOMode (ReadMode),
                                              IOMode (WriteMode),
                                              withBinaryFile)

-- | Download the given URL to the given location. If the file already exists,
-- no download is performed. Otherwise, creates the parent directory, downloads
-- to a temporary file, and on file download completion moves to the
-- appropriate destination.
--
-- Throws an exception if things go wrong
download :: (MonadReader env m, HasHttpManager env, MonadIO m)
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
redownload :: (MonadReader env m, HasHttpManager env, MonadIO m)
           => Request
           -> Path Abs File -- ^ destination
           -> m Bool
redownload req0 dest = do
    let destFilePath = toFilePath dest
        etagFilePath = destFilePath <.> "etag"

    metag <- liftIO $ handleIO (const $ return Nothing) $ fmap Just $
        withBinaryFile etagFilePath ReadMode $ \h ->
            sourceHandle h $$ CB.take 512

    let req1 =
            case metag of
                Nothing -> req0
                Just etag -> req0
                    { requestHeaders =
                        requestHeaders req0 ++
                        [("If-None-Match", L.toStrict etag)]
                    }
        req2 = req1 { checkStatus = \_ _ _ -> Nothing }
    env <- ask
    liftIO $ recoveringHttp drRetryPolicyDefault $ flip runReaderT env $
      withResponse req2 $ \res -> case () of
        ()
          | responseStatus res == status200 -> liftIO $ do
              createDirectoryIfMissing True $ takeDirectory destFilePath

              -- Order here is important: first delete the etag, then write the
              -- file, then write the etag. That way, if any step fails, it will
              -- force the download to happen again.
              handleIO (const $ return ()) $ removeFile etagFilePath

              let destFilePathTmp = destFilePath <.> "tmp"
              withBinaryFile destFilePathTmp WriteMode $ \h ->
                  responseBody res $$ sinkHandle h
              renameFile destFilePathTmp destFilePath

              forM_ (lookup "ETag" (responseHeaders res)) $ \e -> do
                  let tmp = etagFilePath <.> "tmp"
                  S.writeFile tmp e
                  renameFile tmp etagFilePath

              return True
          | responseStatus res == status304 -> return False
          | otherwise -> throwM $ RedownloadFailed req2 dest $ void res

-- | Download a JSON value and parse it using a 'FromJSON' instance.
downloadJSON :: (FromJSON a, MonadReader env m, HasHttpManager env, MonadIO m, MonadThrow m, MonadMask m)
             => Request
             -> m a
downloadJSON req = do
    val <- recoveringHttp drRetryPolicyDefault $
        liftHTTP $ withResponse req $ \res ->
            responseBody res $$ sinkParser json'
    case parseEither parseJSON val of
        Left e -> throwM $ DownloadJSONException req e
        Right x -> return x

data DownloadException
    = DownloadJSONException Request String
    | RedownloadFailed Request (Path Abs File) (Response ())
    deriving (Show, Typeable)
instance Exception DownloadException

-- | A convenience method for asking for the environment and then running an
-- action with its 'Manager'. Useful for avoiding a 'MonadBaseControl'
-- constraint.
liftHTTP :: (MonadIO m, MonadReader env m, HasHttpManager env)
         => ReaderT Manager IO a
         -> m a
liftHTTP inner = do
    env <- ask
    liftIO $ runReaderT inner $ getHttpManager env
