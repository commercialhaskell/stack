{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Network.HTTP.Download.Verified
  ( verifiedDownload
  , recoveringHttp
  , DownloadRequest(..)
  , drRetryPolicyDefault
  , HashCheck(..)
  , CheckHexDigest(..)
  , LengthCheck
  , VerifiedDownloadException(..)
  ) where

import qualified    Data.List as List
import qualified    Data.ByteString as ByteString
import qualified    Data.ByteString.Base64 as B64
import qualified    Data.Conduit.Binary as CB
import qualified    Data.Conduit.List as CL
import qualified    Data.Text as Text
import qualified    Data.Text.Encoding as Text

import              Control.Monad
import              Control.Monad.Catch (Handler (..)) -- would be nice if retry exported this itself
import              Stack.Prelude hiding (Handler (..))
import              Control.Retry (recovering,limitRetries,RetryPolicy,constantDelay,RetryStatus(..))
import              Crypto.Hash
import              Crypto.Hash.Conduit (sinkHash)
import              Data.ByteArray as Mem (convert)
import              Data.ByteArray.Encoding as Mem (convertToBase, Base(Base16))
import              Data.ByteString.Char8 (readInteger)
import              Data.Conduit
import              Data.Conduit.Binary (sourceHandle, sinkHandle)
import              Data.Text.Encoding (decodeUtf8With)
import              Data.Text.Encoding.Error (lenientDecode)
import              GHC.IO.Exception (IOException(..),IOErrorType(..))
import              Network.HTTP.Client (getUri, path)
import              Network.HTTP.Simple (Request, HttpException, httpSink, getResponseHeaders)
import              Network.HTTP.Types.Header (hContentLength, hContentMD5)
import              Path
import              Stack.Types.Runner
import              Stack.PrettyPrint
import              System.Directory
import qualified    System.FilePath as FP ((<.>))
import              System.IO (hFileSize)

-- | A request together with some checks to perform.
data DownloadRequest = DownloadRequest
    { drRequest :: Request
    , drHashChecks :: [HashCheck]
    , drLengthCheck :: Maybe LengthCheck
    , drRetryPolicy :: RetryPolicy
    }

-- | Default to retrying thrice with a short constant delay.
drRetryPolicyDefault :: RetryPolicy
drRetryPolicyDefault = limitRetries 3 <> constantDelay onehundredMilliseconds
  where onehundredMilliseconds = 100000

data HashCheck = forall a. (Show a, HashAlgorithm a) => HashCheck
  { hashCheckAlgorithm :: a
  , hashCheckHexDigest :: CheckHexDigest
  }
deriving instance Show HashCheck

data CheckHexDigest
  = CheckHexDigestString String
  | CheckHexDigestByteString ByteString
  | CheckHexDigestHeader ByteString
  deriving Show
instance IsString CheckHexDigest where
  fromString = CheckHexDigestString

type LengthCheck = Int

-- | An exception regarding verification of a download.
data VerifiedDownloadException
    = WrongContentLength
          Request
          Int -- expected
          ByteString -- actual (as listed in the header)
    | WrongStreamLength
          Request
          Int -- expected
          Int -- actual
    | WrongDigest
          Request
          String -- algorithm
          CheckHexDigest -- expected
          String -- actual (shown)
  deriving (Typeable)
instance Show VerifiedDownloadException where
    show (WrongContentLength req expected actual) =
        "Download expectation failure: ContentLength header\n"
        ++ "Expected: " ++ show expected ++ "\n"
        ++ "Actual:   " ++ displayByteString actual ++ "\n"
        ++ "For: " ++ show (getUri req)
    show (WrongStreamLength req expected actual) =
        "Download expectation failure: download size\n"
        ++ "Expected: " ++ show expected ++ "\n"
        ++ "Actual:   " ++ show actual ++ "\n"
        ++ "For: " ++ show (getUri req)
    show (WrongDigest req algo expected actual) =
        "Download expectation failure: content hash (" ++ algo ++  ")\n"
        ++ "Expected: " ++ displayCheckHexDigest expected ++ "\n"
        ++ "Actual:   " ++ actual ++ "\n"
        ++ "For: " ++ show (getUri req)

instance Exception VerifiedDownloadException

-- This exception is always caught and never thrown outside of this module.
data VerifyFileException
    = WrongFileSize
          Int -- expected
          Integer -- actual (as listed by hFileSize)
  deriving (Show, Typeable)
instance Exception VerifyFileException

-- Show a ByteString that is known to be UTF8 encoded.
displayByteString :: ByteString -> String
displayByteString =
    Text.unpack . Text.strip . Text.decodeUtf8

-- Show a CheckHexDigest in human-readable format.
displayCheckHexDigest :: CheckHexDigest -> String
displayCheckHexDigest (CheckHexDigestString s) = s ++ " (String)"
displayCheckHexDigest (CheckHexDigestByteString s) = displayByteString s ++ " (ByteString)"
displayCheckHexDigest (CheckHexDigestHeader h) =
      show (B64.decodeLenient h) ++ " (Header. unencoded: " ++ show h ++ ")"


-- | Make sure that the hash digest for a finite stream of bytes
-- is as expected.
--
-- Throws WrongDigest (VerifiedDownloadException)
sinkCheckHash :: MonadThrow m
    => Request
    -> HashCheck
    -> Consumer ByteString m ()
sinkCheckHash req HashCheck{..} = do
    digest <- sinkHashUsing hashCheckAlgorithm
    let actualDigestString = show digest
    let actualDigestHexByteString = Mem.convertToBase Mem.Base16 digest
    let actualDigestBytes = Mem.convert digest

    let passedCheck = case hashCheckHexDigest of
          CheckHexDigestString s -> s == actualDigestString
          CheckHexDigestByteString b -> b == actualDigestHexByteString
          CheckHexDigestHeader b -> B64.decodeLenient b == actualDigestHexByteString
            || B64.decodeLenient b == actualDigestBytes
            -- A hack to allow hackage tarballs to download.
            -- They should really base64-encode their md5 header as per rfc2616#sec14.15.
            -- https://github.com/commercialhaskell/stack/issues/240
            || b == actualDigestHexByteString

    unless passedCheck $
        throwM $ WrongDigest req (show hashCheckAlgorithm) hashCheckHexDigest actualDigestString

assertLengthSink :: MonadThrow m
    => Request
    -> LengthCheck
    -> ZipSink ByteString m ()
assertLengthSink req expectedStreamLength = ZipSink $ do
  Sum actualStreamLength <- CL.foldMap (Sum . ByteString.length)
  when (actualStreamLength /= expectedStreamLength) $
    throwM $ WrongStreamLength req expectedStreamLength actualStreamLength

-- | A more explicitly type-guided sinkHash.
sinkHashUsing :: (Monad m, HashAlgorithm a) => a -> Consumer ByteString m (Digest a)
sinkHashUsing _ = sinkHash

-- | Turns a list of hash checks into a ZipSink that checks all of them.
hashChecksToZipSink :: MonadThrow m => Request -> [HashCheck] -> ZipSink ByteString m ()
hashChecksToZipSink req = traverse_ (ZipSink . sinkCheckHash req)

-- 'Control.Retry.recovering' customized for HTTP failures
recoveringHttp :: (MonadUnliftIO m, MonadLogger m, HasRunner env, MonadReader env m)
               => RetryPolicy -> m a -> m a
recoveringHttp retryPolicy =
#if MIN_VERSION_retry(0,7,0)
    helper $ \run -> recovering retryPolicy (handlers run) . const
#else
    helper $ \run -> recovering retryPolicy (handlers run)
#endif
  where
    helper :: (MonadUnliftIO m, HasRunner env, MonadReader env m) => (UnliftIO m -> IO a -> IO a) -> m a -> m a
    helper wrapper action = withUnliftIO $ \run -> wrapper run (unliftIO run action)

    handlers :: (MonadLogger m, HasRunner env, MonadReader env m) => UnliftIO m -> [RetryStatus -> Handler IO Bool]
    handlers run = [Handler . alwaysRetryHttp (unliftIO run),const $ Handler retrySomeIO]

    alwaysRetryHttp :: (MonadLogger m', Monad m, HasRunner env, MonadReader env m') => (m' () -> m ()) -> RetryStatus -> HttpException -> m Bool
    alwaysRetryHttp run rs _ = do
      run $
        prettyWarn $ vcat
          [ flow $ unwords
            [ "Retry number"
            , show (rsIterNumber rs)
            , "after a total delay of"
            , show (rsCumulativeDelay rs)
            , "us"
            ]
          , flow $ unwords
            [ "If you see this warning and stack fails to download,"
            , "but running the command again solves the problem,"
            , "please report here: https://github.com/commercialhaskell/stack/issues/3510"
            ]
          ]
      return True

    retrySomeIO :: Monad m => IOException -> m Bool
    retrySomeIO e = return $ case ioe_type e of
                               -- hGetBuf: resource vanished (Connection reset by peer)
                               ResourceVanished -> True
                               -- conservatively exclude all others
                               _ -> False

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
verifiedDownload :: (MonadUnliftIO m, MonadLogger m, HasRunner env, MonadReader env m)
         => DownloadRequest
         -> Path Abs File -- ^ destination
         -> (Maybe Integer -> Sink ByteString IO ()) -- ^ custom hook to observe progress
         -> m Bool -- ^ Whether a download was performed
verifiedDownload DownloadRequest{..} destpath progressSink = do
    let req = drRequest
    whenM' (liftIO getShouldDownload) $ do
        logDebug $ "Downloading " <> decodeUtf8With lenientDecode (path req)
        liftIO $ createDirectoryIfMissing True dir
        recoveringHttp drRetryPolicy $ liftIO $ 
            withBinaryFile fptmp WriteMode $ \h ->
                httpSink req (go h)
        liftIO $ renameFile fptmp fp
  where
    whenM' mp m = do
        p <- mp
        if p then m >> return True else return False

    fp = toFilePath destpath
    fptmp = fp FP.<.> "tmp"
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
        ((checkExpectations >> return True)
          `catch` \(_ :: VerifyFileException) -> return False)
          `catch` \(_ :: VerifiedDownloadException) -> return False

    checkExpectations = withBinaryFile fp ReadMode $ \h -> do
        for_ drLengthCheck $ checkFileSizeExpectations h
        sourceHandle h $$ getZipSink (hashChecksToZipSink drRequest drHashChecks)

    -- doesn't move the handle
    checkFileSizeExpectations h expectedFileSize = do
        fileSizeInteger <- hFileSize h
        when (fileSizeInteger > toInteger (maxBound :: Int)) $
            throwM $ WrongFileSize expectedFileSize fileSizeInteger
        let fileSize = fromInteger fileSizeInteger
        when (fileSize /= expectedFileSize) $
            throwM $ WrongFileSize expectedFileSize fileSizeInteger

    checkContentLengthHeader headers expectedContentLength =
        case List.lookup hContentLength headers of
            Just lengthBS -> do
              let lengthStr = displayByteString lengthBS
              when (lengthStr /= show expectedContentLength) $
                throwM $ WrongContentLength drRequest expectedContentLength lengthBS
            _ -> return ()

    go h res = do
        let headers = getResponseHeaders res
            mcontentLength = do
              hLength <- List.lookup hContentLength headers
              (i,_) <- readInteger hLength
              return i
        for_ drLengthCheck $ checkContentLengthHeader headers
        let hashChecks = (case List.lookup hContentMD5 headers of
                Just md5BS ->
                    [ HashCheck
                          { hashCheckAlgorithm = MD5
                          , hashCheckHexDigest = CheckHexDigestHeader md5BS
                          }
                    ]
                Nothing -> []
                ) ++ drHashChecks

        maybe id (\len -> (CB.isolate len =$=)) drLengthCheck
            $ getZipSink
                ( hashChecksToZipSink drRequest hashChecks
                  *> maybe (pure ()) (assertLengthSink drRequest) drLengthCheck
                  *> ZipSink (sinkHandle h)
                  *> ZipSink (progressSink mcontentLength))
