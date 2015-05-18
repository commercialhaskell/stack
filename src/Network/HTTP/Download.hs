{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Network.HTTP.Download
    ( download
    , downloadJSON
    , parseUrl
    , liftHTTP
    , ask
    , getHttpManager
    , MonadReader
    , HasHttpManager
    ) where

import           Control.Exception           (Exception)
import           Control.Monad.Catch         (MonadThrow, throwM)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, ReaderT, ask,
                                              runReaderT)
import           Data.Aeson                  (FromJSON, parseJSON)
import           Data.Aeson.Parser           (json')
import           Data.Aeson.Types            (parseEither)
import           Data.Conduit                (($$))
import           Data.Conduit.Attoparsec     (sinkParser)
import           Data.Conduit.Binary         (sinkHandle)
import           Data.Typeable               (Typeable)
import           Network.HTTP.Client.Conduit (HasHttpManager, Manager, Request,
                                              getHttpManager, parseUrl,
                                              responseBody, withResponse)
import           Path                        (Abs, File, Path, parent,
                                              toFilePath)
import           System.Directory            (createDirectoryIfMissing,
                                              doesFileExist, renameFile)
import           System.FilePath             ((<.>))
import           System.IO                   (IOMode (WriteMode),
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
         -> m ()
download req destpath = do
    env <- ask
    liftIO $ unlessM (doesFileExist fp) $ do
        createDirectoryIfMissing True dir
        withBinaryFile fptmp WriteMode $ \h ->
            flip runReaderT env $
            withResponse req $ \res ->
            responseBody res $$ sinkHandle h
        renameFile fptmp fp
  where
    unlessM mp m = do
        p <- mp
        if p then return () else m

    fp = toFilePath destpath
    fptmp = fp <.> "tmp"
    dir = toFilePath $ parent destpath

-- | Download a JSON value and parse it using a 'FromJSON' instance.
downloadJSON :: (FromJSON a, MonadReader env m, HasHttpManager env, MonadIO m, MonadThrow m)
             => Request
             -> m a
downloadJSON req = do
    val <- liftHTTP $ withResponse req $ \res ->
        responseBody res $$ sinkParser json'
    case parseEither parseJSON val of
        Left e -> throwM $ DownloadJSONException req e
        Right x -> return x

data DownloadJSONException = DownloadJSONException Request String
    deriving (Show, Typeable)
instance Exception DownloadJSONException

-- | A convenience method for asking for the environment and then running an
-- action with its 'Manager'. Useful for avoiding a 'MonadBaseControl'
-- constraint.
liftHTTP :: (MonadIO m, MonadReader env m, HasHttpManager env)
         => ReaderT Manager IO a
         -> m a
liftHTTP inner = do
    env <- ask
    liftIO $ runReaderT inner $ getHttpManager env
