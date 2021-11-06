{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | Provide ability to upload tarballs to Hackage.
module Stack.Upload
    ( -- * Upload
      upload
    , uploadBytes
    , uploadRevision
      -- * Credentials
    , HackageCreds
    , loadAuth
    , writeFilePrivate
      -- * Internal
    , maybeGetHackageKey
    ) where

import           Stack.Prelude
import           Data.Aeson                            (FromJSON (..),
                                                        ToJSON (..),
                                                        decode', toEncoding, fromEncoding,
                                                        object, withObject,
                                                        (.:), (.=))
import           Data.ByteString.Builder               (lazyByteString)
import qualified Data.ByteString.Char8                 as S
import qualified Data.ByteString.Lazy                  as L
import qualified Data.Conduit.Binary                   as CB
import qualified Data.Text                             as T
import           Network.HTTP.StackClient              (Request,
                                                        RequestBody(RequestBodyLBS),
                                                        Response,
                                                        withResponse,
                                                        httpNoBody,
                                                        getGlobalManager,
                                                        getResponseStatusCode,
                                                        getResponseBody,
                                                        setRequestHeader,
                                                        parseRequest,
                                                        formDataBody, partFileRequestBody,
                                                        partBS, partLBS,
                                                        applyDigestAuth,
                                                        displayDigestAuthException)
import           Stack.Options.UploadParser
import           Stack.Types.Config
import           System.Directory                      (createDirectoryIfMissing,
                                                        removeFile, renameFile)
import           System.Environment                    (lookupEnv)
import           System.FilePath                       ((</>), takeFileName, takeDirectory)
import           System.PosixCompat.Files              (setFileMode)


newtype HackageKey = HackageKey Text

-- | Username and password to log into Hackage.
--
-- Since 0.1.0.0
data HackageCreds = HackageCreds
    { hcUsername :: !Text
    , hcPassword :: !Text
    , hcCredsFile :: !FilePath
    }
    deriving Show

data HackageAuth = HAKey HackageKey
                 | HACreds HackageCreds

instance ToJSON HackageCreds where
    toJSON (HackageCreds u p _) = object
        [ "username" .= u
        , "password" .= p
        ]
instance FromJSON (FilePath -> HackageCreds) where
    parseJSON = withObject "HackageCreds" $ \o -> HackageCreds
        <$> o .: "username"
        <*> o .: "password"

withEnvVariable :: Text -> IO Text -> IO Text
withEnvVariable varName fromPrompt = lookupEnv (T.unpack varName) >>= maybe fromPrompt (pure . T.pack)

maybeGetHackageKey :: RIO m (Maybe HackageKey)
maybeGetHackageKey = fmap (HackageKey . T.pack) <$> (liftIO $ lookupEnv (T.unpack "HACKAGE_KEY"))


loadAuth :: HasLogFunc m => Config -> RIO m HackageAuth
loadAuth config = do
  maybeHackageKey <- maybeGetHackageKey
  case maybeHackageKey of
    Just key -> do
      logInfo "HACKAGE_KEY found in env, using that for credentials."
      return $ HAKey key
    Nothing -> HACreds <$> loadUserAndPassword config

-- | Load Hackage credentials, either from a save file or the command
-- line.
--
-- Since 0.1.0.0
loadUserAndPassword :: HasLogFunc m => Config -> RIO m HackageCreds
loadUserAndPassword config = do
  fp <- liftIO $ credsFile config
  elbs <- liftIO $ tryIO $ L.readFile fp
  case either (const Nothing) Just elbs >>= \lbs -> (lbs, ) <$> decode' lbs of
    Nothing -> fromPrompt fp
    Just (lbs, mkCreds) -> do
      -- Ensure privacy, for cleaning up old versions of Stack that
      -- didn't do this
      writeFilePrivate fp $ lazyByteString lbs

      unless (configSaveHackageCreds config) $ do
        logWarn "WARNING: You've set save-hackage-creds to false"
        logWarn "However, credentials were found at:"
        logWarn $ "  " <> fromString fp
      return $ mkCreds fp
  where
    fromPrompt :: HasLogFunc m => FilePath -> RIO m HackageCreds
    fromPrompt fp = do
      username <- liftIO $ withEnvVariable "HACKAGE_USERNAME" (prompt "Hackage username: ")
      password <- liftIO $ withEnvVariable "HACKAGE_PASSWORD" (promptPassword "Hackage password: ")
      let hc = HackageCreds
            { hcUsername = username
            , hcPassword = password
            , hcCredsFile = fp
            }

      when (configSaveHackageCreds config) $ do
        shouldSave <- promptBool $ T.pack $
          "Save hackage credentials to file at " ++ fp ++ " [y/n]? "
        logInfo "NOTE: Avoid this prompt in the future by using: save-hackage-creds: false"
        when shouldSave $ do
          writeFilePrivate fp $ fromEncoding $ toEncoding hc
          logInfo "Saved!"
          hFlush stdout

      return hc

-- | Write contents to a file which is always private.
--
-- For history of this function, see:
--
-- * https://github.com/commercialhaskell/stack/issues/2159#issuecomment-477948928
--
-- * https://github.com/commercialhaskell/stack/pull/4665
writeFilePrivate :: MonadIO m => FilePath -> Builder -> m ()
writeFilePrivate fp builder = liftIO $ withTempFile (takeDirectory fp) (takeFileName fp) $ \fpTmp h -> do
  -- Temp file is created such that only current user can read and write it.
  -- See docs for openTempFile: https://www.stackage.org/haddock/lts-13.14/base-4.12.0.0/System-IO.html#v:openTempFile

  -- Write to the file and close the handle.
  hPutBuilder h builder
  hClose h

  -- Make sure the destination file, if present, is writeable
  void $ tryIO $ setFileMode fp 0o600

  -- And atomically move
  renameFile fpTmp fp

credsFile :: Config -> IO FilePath
credsFile config = do
    let dir = toFilePath (view stackRootL config) </> "upload"
    createDirectoryIfMissing True dir
    return $ dir </> "credentials.json"

addAPIKey :: HackageKey -> Request -> Request
addAPIKey (HackageKey key) req =
  setRequestHeader "Authorization" [fromString $ "X-ApiKey" ++ " " ++ T.unpack key] req

applyAuth :: HasLogFunc m => HackageAuth -> Request -> RIO m Request
applyAuth haAuth req0 = do
    case haAuth of
        HAKey key -> return (addAPIKey key req0)
        HACreds creds -> applyCreds creds req0

applyCreds :: HasLogFunc m => HackageCreds -> Request -> RIO m Request
applyCreds creds req0 = do
  manager <- liftIO $ getGlobalManager
  ereq <- liftIO $ applyDigestAuth
    (encodeUtf8 $ hcUsername creds)
    (encodeUtf8 $ hcPassword creds)
    req0
    manager
  case ereq of
      Left e -> do
          logWarn "WARNING: No HTTP digest prompt found, this will probably fail"
          case fromException e of
              Just e' -> logWarn $ fromString (displayDigestAuthException e')
              Nothing -> logWarn $ fromString $ displayException e
          return req0
      Right req -> return req

-- | Upload a single tarball with the given @Uploader@.  Instead of
-- sending a file like 'upload', this sends a lazy bytestring.
--
-- Since 0.1.2.1
uploadBytes :: HasLogFunc m
            => String -- ^ Hackage base URL
            -> HackageAuth
            -> String -- ^ tar file name
            -> UploadVariant
            -> L.ByteString -- ^ tar file contents
            -> RIO m ()
uploadBytes baseUrl auth tarName uploadVariant bytes = do
    let req1 = setRequestHeader "Accept" ["text/plain"]
               (fromString $ baseUrl
                          <> "packages/"
                          <> case uploadVariant of
                               Publishing -> ""
                               Candidate -> "candidates/"
               )
        formData = [partFileRequestBody "package" tarName (RequestBodyLBS bytes)]
    req2 <- liftIO $ formDataBody formData req1
    req3 <- applyAuth auth req2
    logInfo $ "Uploading " <> fromString tarName <> "... "
    hFlush stdout
    withRunInIO $ \runInIO -> withResponse req3 (runInIO . inner)
 where
    inner :: HasLogFunc m => Response (ConduitM () S.ByteString IO ()) -> RIO m ()
    inner res =
        case getResponseStatusCode res of
            200 -> logInfo "done!"
            401 -> do
                logError "authentication failure"
                case auth of
                  HACreds creds -> handleIO (const $ return ()) (liftIO $ removeFile (hcCredsFile creds))
                  _ -> pure ()
                throwString "Authentication failure uploading to server"
            403 -> do
                logError "forbidden upload"
                logError "Usually means: you've already uploaded this package/version combination"
                logError "Ignoring error and continuing, full message from Hackage below:\n"
                liftIO $ printBody res
            503 -> do
                logError "service unavailable"
                logError "This error some times gets sent even though the upload succeeded"
                logError "Check on Hackage to see if your pacakge is present"
                liftIO $ printBody res
            code -> do
                logError $ "unhandled status code: " <> fromString (show code)
                liftIO $ printBody res
                throwString $ "Upload failed on " <> fromString tarName

printBody :: Response (ConduitM () S.ByteString IO ()) -> IO ()
printBody res = runConduit $ getResponseBody res .| CB.sinkHandle stdout

-- | Upload a single tarball with the given @Uploader@.
--
-- Since 0.1.0.0
upload :: HasLogFunc m
       => String -- ^ Hackage base URL
       -> HackageAuth
       -> FilePath
       -> UploadVariant
       -> RIO m ()
upload baseUrl auth fp uploadVariant =
  uploadBytes baseUrl auth (takeFileName fp) uploadVariant =<< (liftIO $ L.readFile fp)

uploadRevision :: HasLogFunc m
               => String -- ^ Hackage base URL
               -> HackageAuth
               -> PackageIdentifier
               -> L.ByteString
               -> RIO m ()
uploadRevision baseUrl auth ident@(PackageIdentifier name _) cabalFile = do
  req0 <- parseRequest $ concat
    [ baseUrl
    , "package/"
    , packageIdentifierString ident
    , "/"
    , packageNameString name
    , ".cabal/edit"
    ]
  req1 <- formDataBody
    [ partLBS "cabalfile" cabalFile
    , partBS "publish" "on"
    ]
    req0
  req2 <- applyAuth auth req1
  void $ httpNoBody req2
