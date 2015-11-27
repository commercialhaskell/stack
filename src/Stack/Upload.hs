{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Provide ability to upload tarballs to Hackage.
module Stack.Upload
    ( -- * Upload
      nopUploader
    , mkUploader
    , Uploader
    , upload
    , uploadBytes
    , UploadSettings
    , defaultUploadSettings
    , setUploadUrl
    , setGetManager
    , setCredsSource
    , setSaveCreds
      -- * Credentials
    , HackageCreds
    , loadCreds
    , saveCreds
    , FromFile
      -- ** Credentials source
    , HackageCredsSource
    , fromAnywhere
    , fromPrompt
    , fromFile
    , fromMemory
    ) where

import           Control.Applicative
import           Control.Exception                     (bracket)
import qualified Control.Exception                     as E
import           Control.Monad                         (when, unless)
import           Data.Aeson                            (FromJSON (..),
                                                        ToJSON (..),
                                                        eitherDecode', encode,
                                                        object, withObject,
                                                        (.:), (.=))
import qualified Data.ByteString.Char8                 as S
import qualified Data.ByteString.Lazy                  as L
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Encoding                    (encodeUtf8)
import qualified Data.Text.IO                          as TIO
import           Data.Typeable                         (Typeable)
import           Network.HTTP.Client                   (BodyReader, Manager,
                                                        Response,
                                                        RequestBody(RequestBodyLBS),
                                                        applyBasicAuth, brRead,
                                                        checkStatus, newManager,
                                                        parseUrl,
                                                        requestHeaders,
                                                        responseBody,
                                                        responseStatus,
                                                        withResponse)
import           Network.HTTP.Client.MultipartFormData (formDataBody, partFileRequestBody)
import           Network.HTTP.Client.TLS               (tlsManagerSettings)
import           Network.HTTP.Types                    (statusCode)
import           Path                                  (toFilePath)
import           Prelude -- Fix redundant import warnings
import           Stack.Types
import           System.Directory                      (createDirectoryIfMissing,
                                                        removeFile)
import           System.FilePath                       ((</>), takeFileName)
import           System.IO                             (hFlush, hGetEcho, hSetEcho,
                                                        stdin, stdout)

-- | Username and password to log into Hackage.
--
-- Since 0.1.0.0
data HackageCreds = HackageCreds
    { hcUsername :: !Text
    , hcPassword :: !Text
    }
    deriving Show

instance ToJSON HackageCreds where
    toJSON (HackageCreds u p) = object
        [ "username" .= u
        , "password" .= p
        ]
instance FromJSON HackageCreds where
    parseJSON = withObject "HackageCreds" $ \o -> HackageCreds
        <$> o .: "username"
        <*> o .: "password"

-- | A source for getting Hackage credentials.
--
-- Since 0.1.0.0
newtype HackageCredsSource = HackageCredsSource
    { getCreds :: IO (HackageCreds, FromFile)
    }

-- | Whether the Hackage credentials were loaded from a file.
--
-- This information is useful since, typically, you only want to save the
-- credentials to a file if it wasn't already loaded from there.
--
-- Since 0.1.0.0
type FromFile = Bool

-- | Load Hackage credentials from the given source.
--
-- Since 0.1.0.0
loadCreds :: HackageCredsSource -> IO (HackageCreds, FromFile)
loadCreds = getCreds

-- | Save the given credentials to the credentials file.
--
-- Since 0.1.0.0
saveCreds :: Config -> HackageCreds -> IO ()
saveCreds config creds = do
    fp <- credsFile config
    L.writeFile fp $ encode creds

-- | Load the Hackage credentials from the prompt, asking the user to type them
-- in.
--
-- Since 0.1.0.0
fromPrompt :: HackageCredsSource
fromPrompt = HackageCredsSource $ do
    putStr "Hackage username: "
    hFlush stdout
    username <- TIO.getLine
    password <- promptPassword
    return (HackageCreds
        { hcUsername = username
        , hcPassword = password
        }, False)

credsFile :: Config -> IO FilePath
credsFile config = do
    let dir = toFilePath (configStackRoot config) </> "upload"
    createDirectoryIfMissing True dir
    return $ dir </> "credentials.json"

-- | Load the Hackage credentials from the JSON config file.
--
-- Since 0.1.0.0
fromFile :: Config -> HackageCredsSource
fromFile config = HackageCredsSource $ do
    fp <- credsFile config
    lbs <- L.readFile fp
    case eitherDecode' lbs of
        Left e -> E.throwIO $ Couldn'tParseJSON fp e
        Right creds -> return (creds, True)

-- | Load the Hackage credentials from the given arguments.
--
-- Since 0.1.0.0
fromMemory :: Text -> Text -> HackageCredsSource
fromMemory u p = HackageCredsSource $ return (HackageCreds
    { hcUsername = u
    , hcPassword = p
    }, False)

data HackageCredsExceptions = Couldn'tParseJSON FilePath String
    deriving (Show, Typeable)
instance E.Exception HackageCredsExceptions

-- | Try to load the credentials from the config file. If that fails, ask the
-- user to enter them.
--
-- Since 0.1.0.0
fromAnywhere :: Config -> HackageCredsSource
fromAnywhere config = HackageCredsSource $
    getCreds (fromFile config) `E.catches`
        [ E.Handler $ \(_ :: E.IOException) -> getCreds fromPrompt
        , E.Handler $ \(_ :: HackageCredsExceptions) -> getCreds fromPrompt
        ]

-- | Lifted from cabal-install, Distribution.Client.Upload
promptPassword :: IO Text
promptPassword = do
  putStr "Hackage password: "
  hFlush stdout
  -- save/restore the terminal echoing status
  passwd <- bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
    hSetEcho stdin False  -- no echoing for entering the password
    fmap T.pack getLine
  putStrLn ""
  return passwd

nopUploader :: Config -> UploadSettings -> IO Uploader
nopUploader _ _ = return (Uploader nop)
  where nop :: String -> L.ByteString -> IO ()
        nop _ _ = return ()

-- | Turn the given settings into an @Uploader@.
--
-- Since 0.1.0.0
mkUploader :: Config -> UploadSettings -> IO Uploader
mkUploader config us = do
    manager <- usGetManager us
    (creds, fromFile') <- loadCreds $ usCredsSource us config
    when (not fromFile' && usSaveCreds us) $ saveCreds config creds
    req0 <- parseUrl $ usUploadUrl us
    let req1 = req0
            { requestHeaders = [("Accept", "text/plain")]
            , checkStatus = \_ _ _ -> Nothing
            }
    return Uploader
        { upload_ = \tarName bytes -> do
            let formData = [partFileRequestBody "package" tarName (RequestBodyLBS bytes)]
            req2 <- formDataBody formData req1
            let req3 = applyBasicAuth
                    (encodeUtf8 $ hcUsername creds)
                    (encodeUtf8 $ hcPassword creds)
                    req2
            putStr $ "Uploading " ++ tarName ++ "... "
            hFlush stdout
            withResponse req3 manager $ \res ->
                case statusCode $ responseStatus res of
                    200 -> putStrLn "done!"
                    401 -> do
                        putStrLn "authentication failure"
                        cfp <- credsFile config
                        handleIO (const $ return ()) (removeFile cfp)
                        error "Authentication failure uploading to server"
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
                        error $ "Upload failed on " ++ tarName
        }

printBody :: Response BodyReader -> IO ()
printBody res =
    loop
  where
    loop = do
        bs <- brRead $ responseBody res
        unless (S.null bs) $ do
            S.hPut stdout bs
            loop

-- | The computed value from a @UploadSettings@.
--
-- Typically, you want to use this with 'upload'.
--
-- Since 0.1.0.0
data Uploader = Uploader
    { upload_ :: !(String -> L.ByteString -> IO ())
    }

-- | Upload a single tarball with the given @Uploader@.
--
-- Since 0.1.0.0
upload :: Uploader -> FilePath -> IO ()
upload uploader fp = upload_ uploader (takeFileName fp) =<< L.readFile fp

-- | Upload a single tarball with the given @Uploader@.  Instead of
-- sending a file like 'upload', this sends a lazy bytestring.
--
-- Since 0.1.2.1
uploadBytes :: Uploader -> String -> L.ByteString -> IO ()
uploadBytes = upload_

-- | Settings for creating an @Uploader@.
--
-- Since 0.1.0.0
data UploadSettings = UploadSettings
    { usUploadUrl   :: !String
    , usGetManager  :: !(IO Manager)
    , usCredsSource :: !(Config -> HackageCredsSource)
    , usSaveCreds   :: !Bool
    }

-- | Default value for @UploadSettings@.
--
-- Use setter functions to change defaults.
--
-- Since 0.1.0.0
defaultUploadSettings :: UploadSettings
defaultUploadSettings = UploadSettings
    { usUploadUrl = "https://hackage.haskell.org/packages/"
    , usGetManager = newManager tlsManagerSettings
    , usCredsSource = fromAnywhere
    , usSaveCreds = True
    }

-- | Change the upload URL.
--
-- Default: "https://hackage.haskell.org/packages/"
--
-- Since 0.1.0.0
setUploadUrl :: String -> UploadSettings -> UploadSettings
setUploadUrl x us = us { usUploadUrl = x }

-- | How to get an HTTP connection manager.
--
-- Default: @newManager tlsManagerSettings@
--
-- Since 0.1.0.0
setGetManager :: IO Manager -> UploadSettings -> UploadSettings
setGetManager x us = us { usGetManager = x }

-- | How to get the Hackage credentials.
--
-- Default: @fromAnywhere@
--
-- Since 0.1.0.0
setCredsSource :: (Config -> HackageCredsSource) -> UploadSettings -> UploadSettings
setCredsSource x us = us { usCredsSource = x }

-- | Save new credentials to the config file.
--
-- Default: @True@
--
-- Since 0.1.0.0
setSaveCreds :: Bool -> UploadSettings -> UploadSettings
setSaveCreds x us = us { usSaveCreds = x }

handleIO :: (E.IOException -> IO a) -> IO a -> IO a
handleIO = E.handle
