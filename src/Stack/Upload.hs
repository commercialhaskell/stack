{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Provide ability to upload tarballs to Hackage.
module Stack.Upload
    ( -- * Upload
      upload
    , uploadBytes
    , uploadRevision
      -- * Credentials
    , HackageCreds
    , loadCreds
    ) where

import           Stack.Prelude
import           Data.Aeson                            (FromJSON (..),
                                                        ToJSON (..),
                                                        decode', encode,
                                                        object, withObject,
                                                        (.:), (.=))
import qualified Data.ByteString.Char8                 as S
import qualified Data.ByteString.Lazy                  as L
import qualified Data.Conduit.Binary                   as CB
import qualified Data.Text                             as T
import           Data.Text.Encoding                    (encodeUtf8)
import qualified Data.Text.IO                          as TIO
import           Network.HTTP.Client                   (Response,
                                                        RequestBody(RequestBodyLBS),
                                                        Request)
import           Network.HTTP.Simple                   (withResponse,
                                                        getResponseStatusCode,
                                                        getResponseBody,
                                                        setRequestHeader,
                                                        parseRequest,
                                                        httpNoBody)
import           Network.HTTP.Client.MultipartFormData (formDataBody, partFileRequestBody,
                                                        partBS, partLBS)
import           Network.HTTP.Client.TLS               (getGlobalManager,
                                                        applyDigestAuth,
                                                        displayDigestAuthException)
import           Stack.Types.Config
import           Stack.Types.PackageIdentifier         (PackageIdentifier, packageIdentifierString,
                                                        packageIdentifierName)
import           Stack.Types.PackageName               (packageNameString)
import           System.Directory                      (createDirectoryIfMissing,
                                                        removeFile)
import           System.FilePath                       ((</>), takeFileName)
import           System.IO                             (hFlush, stdout, putStrLn, putStr, getLine, print) -- TODO remove putStrLn, use logInfo
import           System.IO.Echo                        (withoutInputEcho)

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

-- | Load Hackage credentials, either from a save file or the command
-- line.
--
-- Since 0.1.0.0
loadCreds :: Config -> IO HackageCreds
loadCreds config = do
  fp <- credsFile config
  elbs <- tryIO $ L.readFile fp
  case either (const Nothing) Just elbs >>= decode' of
    Nothing -> fromPrompt fp
    Just mkCreds -> do
      unless (configSaveHackageCreds config) $ do
        putStrLn "WARNING: You've set save-hackage-creds to false"
        putStrLn "However, credentials were found at:"
        putStrLn $ "  " ++ fp
      return $ mkCreds fp
  where
    fromPrompt fp = do
      putStr "Hackage username: "
      hFlush stdout
      username <- TIO.getLine
      password <- promptPassword
      let hc = HackageCreds
            { hcUsername = username
            , hcPassword = password
            , hcCredsFile = fp
            }

      when (configSaveHackageCreds config) $ do
        let prompt = "Save hackage credentials to file at " ++ fp ++ " [y/n]? "
        putStr prompt
        input <- loopPrompt prompt
        putStrLn "NOTE: Avoid this prompt in the future by using: save-hackage-creds: false"
        hFlush stdout
        case input of
          "y" -> do
            L.writeFile fp (encode hc)
            putStrLn "Saved!"
            hFlush stdout
          _ -> return ()

      return hc

    loopPrompt :: String -> IO String
    loopPrompt p = do
      input <- TIO.getLine
      case input of
        "y" -> return "y"
        "n" -> return "n"
        _   -> do
          putStr p
          loopPrompt p

credsFile :: Config -> IO FilePath
credsFile config = do
    let dir = toFilePath (configStackRoot config) </> "upload"
    createDirectoryIfMissing True dir
    return $ dir </> "credentials.json"

-- | Lifted from cabal-install, Distribution.Client.Upload
promptPassword :: IO Text
promptPassword = do
  putStr "Hackage password: "
  hFlush stdout
  -- save/restore the terminal echoing status (no echoing for entering the password)
  passwd <- withoutInputEcho $ fmap T.pack getLine
  putStrLn ""
  return passwd

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
uploadBytes :: HackageCreds
            -> String -- ^ tar file name
            -> L.ByteString -- ^ tar file contents
            -> IO ()
uploadBytes creds tarName bytes = do
    let req1 = setRequestHeader "Accept" ["text/plain"]
               "https://hackage.haskell.org/packages/"
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
upload :: HackageCreds -> FilePath -> IO ()
upload creds fp = uploadBytes creds (takeFileName fp) =<< L.readFile fp

uploadRevision :: HackageCreds
               -> PackageIdentifier
               -> L.ByteString
               -> IO ()
uploadRevision creds ident cabalFile = do
  req0 <- parseRequest $ concat
    [ "https://hackage.haskell.org/package/"
    , packageIdentifierString ident
    , "/"
    , packageNameString $ packageIdentifierName ident
    , ".cabal/edit"
    ]
  req1 <- formDataBody
    [ partLBS "cabalfile" cabalFile
    , partBS "publish" "on"
    ]
    req0
  req2 <- applyCreds creds req1
  void $ httpNoBody req2
