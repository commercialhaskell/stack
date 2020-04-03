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
    , loadCreds
    , writeFilePrivate
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
import           Network.HTTP.StackClient              (Request, RequestBody(RequestBodyLBS), Response, withResponse, httpNoBody, getGlobalManager, getResponseStatusCode,
                                                        getResponseBody,
                                                        setRequestHeader,
                                                        parseRequest,
                                                        formDataBody, partFileRequestBody,
                                                        partBS, partLBS,
                                                        applyDigestAuth,
                                                        displayDigestAuthException)
import           Stack.Types.Config
import           System.Directory                      (createDirectoryIfMissing,
                                                        removeFile, renameFile)
import           System.Environment                    (lookupEnv)
import           System.FilePath                       ((</>), takeFileName, takeDirectory)
import           System.IO                             (putStrLn, putStr, print) -- TODO remove putStrLn, use logInfo
import           System.PosixCompat.Files              (setFileMode)

-- | Username and password to log into Hackage.
--
-- Since 0.1.0.0
data HackageCreds = HackageCreds
    { hcUsername :: !Text
    , hcPassword :: !Text
    , hcCredsFile :: !FilePath
    }
    deriving Show

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

-- | Load Hackage credentials, either from a save file or the command
-- line.
--
-- Since 0.1.0.0
loadCreds :: Config -> IO HackageCreds
loadCreds config = do
  fp <- credsFile config
  elbs <- tryIO $ L.readFile fp
  case either (const Nothing) Just elbs >>= \lbs -> (lbs, ) <$> decode' lbs of
    Nothing -> fromPrompt fp
    Just (lbs, mkCreds) -> do
      -- Ensure privacy, for cleaning up old versions of Stack that
      -- didn't do this
      writeFilePrivate fp $ lazyByteString lbs

      unless (configSaveHackageCreds config) $ do
        putStrLn "WARNING: You've set save-hackage-creds to false"
        putStrLn "However, credentials were found at:"
        putStrLn $ "  " ++ fp
      return $ mkCreds fp
  where
    fromPrompt fp = do
      username <- withEnvVariable "HACKAGE_USERNAME" (prompt "Hackage username: ")
      password <- withEnvVariable "HACKAGE_PASSWORD" (promptPassword "Hackage password: ")
      let hc = HackageCreds
            { hcUsername = username
            , hcPassword = password
            , hcCredsFile = fp
            }

      when (configSaveHackageCreds config) $ do
        shouldSave <- promptBool $ T.pack $
          "Save hackage credentials to file at " ++ fp ++ " [y/n]? "
        putStrLn "NOTE: Avoid this prompt in the future by using: save-hackage-creds: false"
        when shouldSave $ do
          writeFilePrivate fp $ fromEncoding $ toEncoding hc
          putStrLn "Saved!"
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

applyCreds :: HackageCreds -> Request -> IO Request
applyCreds creds req0 = do
  manager <- getGlobalManager
  ereq <- applyDigestAuth
    (encodeUtf8 $ hcUsername creds)
    (encodeUtf8 $ hcPassword creds)
    req0
    manager
  case ereq of
      Left e -> do
          putStrLn "WARNING: No HTTP digest prompt found, this will probably fail"
          case fromException e of
              Just e' -> putStrLn $ displayDigestAuthException e'
              Nothing -> print e
          return req0
      Right req -> return req

-- | Upload a single tarball with the given @Uploader@.  Instead of
-- sending a file like 'upload', this sends a lazy bytestring.
--
-- Since 0.1.2.1
uploadBytes :: String -- ^ Hackage base URL
            -> HackageCreds
            -> String -- ^ tar file name
            -> L.ByteString -- ^ tar file contents
            -> IO ()
uploadBytes baseUrl creds tarName bytes = do
    let req1 = setRequestHeader "Accept" ["text/plain"]
               (fromString $ baseUrl <> "packages/")
        formData = [partFileRequestBody "package" tarName (RequestBodyLBS bytes)]
    req2 <- formDataBody formData req1
    req3 <- applyCreds creds req2
    putStr $ "Uploading " ++ tarName ++ "... "
    hFlush stdout
    withResponse req3 $ \res ->
        case getResponseStatusCode res of
            200 -> putStrLn "done!"
            401 -> do
                putStrLn "authentication failure"
                handleIO (const $ return ()) (removeFile (hcCredsFile creds))
                throwString "Authentication failure uploading to server"
            403 -> do
                putStrLn "forbidden upload"
                putStrLn "Usually means: you've already uploaded this package/version combination"
                putStrLn "Ignoring error and continuing, full message from Hackage below:\n"
                printBody res
            503 -> do
                putStrLn "service unavailable"
                putStrLn "This error some times gets sent even though the upload succeeded"
                putStrLn "Check on Hackage to see if your pacakge is present"
                printBody res
            code -> do
                putStrLn $ "unhandled status code: " ++ show code
                printBody res
                throwString $ "Upload failed on " ++ tarName

printBody :: Response (ConduitM () S.ByteString IO ()) -> IO ()
printBody res = runConduit $ getResponseBody res .| CB.sinkHandle stdout

-- | Upload a single tarball with the given @Uploader@.
--
-- Since 0.1.0.0
upload :: String -- ^ Hackage base URL
       -> HackageCreds
       -> FilePath
       -> IO ()
upload baseUrl creds fp = uploadBytes baseUrl creds (takeFileName fp) =<< L.readFile fp

uploadRevision :: String -- ^ Hackage base URL
               -> HackageCreds
               -> PackageIdentifier
               -> L.ByteString
               -> IO ()
uploadRevision baseUrl creds ident@(PackageIdentifier name _) cabalFile = do
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
  req2 <- applyCreds creds req1
  void $ httpNoBody req2
