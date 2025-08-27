{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Stack.Upload
Description : Types and functions related to Stack's @upload@ command.
License     : BSD-3-Clause

Types and functions related to Stack's @upload@ command.
-}

module Stack.Upload
  ( -- * Upload
    UploadOpts (..)
  , SDistOpts (..)
  , UploadContent (..)
  , UploadVariant (..)
  , uploadCmd
  , upload
  , uploadBytes
  , uploadRevision
    -- * Credentials
  , HackageCreds
  , HackageAuth (..)
  , HackageKey (..)
  , loadAuth
  , writeFilePrivate
    -- * Internal
  , maybeGetHackageKey
  ) where

import           Conduit ( mapOutput, sinkList )
import           Data.Aeson
                   ( FromJSON (..), ToJSON (..), (.:), (.=), decode'
                   , fromEncoding, object, toEncoding, withObject
                   )
import           Data.ByteString.Builder ( lazyByteString )
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import           Network.HTTP.StackClient
                   ( Request, RequestBody (RequestBodyLBS), Response
                   , applyDigestAuth, displayDigestAuthException, formDataBody
                   , getGlobalManager, getResponseBody, getResponseStatusCode
                   , httpNoBody, method, methodPost, methodPut, parseRequest
                   , partBS, partFileRequestBody, partLBS, requestBody
                   , setRequestHeader, setRequestHeaders, withResponse
                   )
import           Path ( (</>), addExtension, parseRelFile )
import           Path.IO ( resolveDir', resolveFile' )
import qualified Path.IO as Path
import           Stack.Constants ( isStackUploadDisabled )
import           Stack.Constants.Config ( distDirFromDir )
import           Stack.Prelude
import           Stack.Runners
                   ( ShouldReexec (..), withConfig, withDefaultEnvConfig )
import           Stack.SDist
                   ( SDistOpts (..), checkSDistTarball, checkSDistTarball'
                   , getSDistTarball, readLocalPackage
                   )
import           Stack.Types.Config ( Config (..), configL, stackRootL )
import qualified Stack.Types.Config as Config
import           Stack.Types.EnvConfig ( HasEnvConfig )
import           Stack.Types.Package ( LocalPackage (..), packageIdentifier )
import           Stack.Types.Runner ( Runner )
import           Stack.Types.UploadOpts ( UploadOpts (..), UploadVariant (..) )
import           System.Directory
                   ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
                   , removeFile, renameFile
                   )
import           System.Environment ( lookupEnv )
import qualified System.FilePath as FP
import           System.PosixCompat.Files ( setFileMode )

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "Stack.Upload" module.
data UploadPrettyException
  = AuthenticationFailure
  | ArchiveUploadFailure !Int ![String] !String
  | DocsTarballInvalid ![(String, Path Abs File)]
  | ItemsInvalid ![FilePath]
  | NoItemSpecified !String
  | PackageDirectoryInvalid ![FilePath]
  | PackageIdNotSpecifiedForDocsUploadBug
  | PackageIdSpecifiedForPackageUploadBug
  | TarGzFileNameInvalidBug !String
  deriving Show

instance Pretty UploadPrettyException where
  pretty AuthenticationFailure =
    "[S-2256]"
    <> line
    <> flow "authentification failure"
    <> line
    <> flow "Authentication failure uploading to server"
  pretty (ArchiveUploadFailure code res tarName) =
    "[S-6108]"
    <> line
    <> flow "unhandled status code:" <+> fromString (show code)
    <> line
    <> flow "Upload failed on" <+> style File (fromString tarName)
    <> line
    <> vsep (map string res)
  pretty (DocsTarballInvalid invalidItems) =
    "[S-2837]"
    <> line
    <> flow "Stack can't find:"
    <> line
    <> invalidList
   where
    invalidItem (pkgIdName, tarGzFile) = fillSep
      [ pretty tarGzFile
      , "for"
      , style Current (fromString pkgIdName) <> "."
      ]
    invalidList = bulletedList $ map invalidItem invalidItems
  pretty (ItemsInvalid invalidItems) =
    "[S-3179]"
    <> line
    <> flow "For package upload, Stack expects a list of relative paths to \
            \tosdist tarballs or package directories. Stack can't find:"
    <> line
    <> invalidList
   where
    invalidList = bulletedList $ map (style File . fromString) invalidItems
  pretty (NoItemSpecified subject) =
    "[S-3030]"
    <> line
    <> fillSep
         [ flow "An item must be specified. To upload"
         , flow subject
         , flow "please run"
         , style Shell "stack upload ."
         , flow "(with the period at the end)."
         ]
  pretty (PackageDirectoryInvalid invalidItems) =
    "[S-5908]"
    <> line
    <> flow "For documentation upload, Stack expects a list of relative paths \
            \to package directories. Stack can't find:"
    <> line
    <> invalidList
   where
    invalidList = bulletedList $ map (style Current . fromString) invalidItems
  pretty PackageIdNotSpecifiedForDocsUploadBug = bugPrettyReport "[S-7274]" $
    flow "uploadBytes: Documentation upload but package identifier not \
         \specified."
  pretty PackageIdSpecifiedForPackageUploadBug = bugPrettyReport "[S-5860]" $
    flow "uploadBytes: Package upload but package identifier specified."
  pretty (TarGzFileNameInvalidBug name) = bugPrettyReport "[S-5955]" $
    fillSep
      [ flow "uploadCmd: the name of the"
      , fromString name <> ".tar.gz"
      , flow "file could not be parsed."
      ]

instance Exception UploadPrettyException

-- | Type representing forms of content for upload to Hackage.
data UploadContent
  = SDist
    -- ^ Content in the form of an sdist tarball.
  | DocArchive
    -- ^ Content in the form of an archive file of package documentation.

-- | Function underlying the @stack upload@ command. Upload to Hackage.
uploadCmd :: UploadOpts -> RIO Runner ()
uploadCmd (UploadOpts [] uoDocumentation _ _ _ _ _ _) = do
  let subject = if uoDocumentation
        then "documentation for the current package,"
        else "the current package,"
  prettyThrowIO $ NoItemSpecified subject
uploadCmd uo = do
  let setSaveHackageCreds config =
        let saveHackageCreds = config.saveHackageCreds <> uo.saveHackageCreds
        in config { Config.saveHackageCreds = saveHackageCreds }
  withConfig YesReexec $ local setSaveHackageCreds $ withDefaultEnvConfig $ do
    config <- view configL
    let hackageUrl = T.unpack config.hackageBaseUrl
    if uo.documentation
      then do
        (dirs, invalid) <-
          liftIO $ partitionM doesDirectoryExist uo.itemsToWorkWith
        unless (null invalid) $
          prettyThrowIO $ PackageDirectoryInvalid invalid
        (failed, items) <- partitionEithers <$> forM dirs checkDocsTarball
        unless (null failed) $ do
          prettyThrowIO $ DocsTarballInvalid failed
        getCreds <- memoizeRef $ loadAuth config
        forM_ items $ \(pkgIdName, tarGzFile) -> do
          creds <- runMemoized getCreds
          upload
            hackageUrl
            creds
            DocArchive
            (Just pkgIdName)
            (toFilePath tarGzFile)
            uo.uploadVariant
      else do
        (files, nonFiles) <-
          liftIO $ partitionM doesFileExist uo.itemsToWorkWith
        (dirs, invalid) <- liftIO $ partitionM doesDirectoryExist nonFiles
        unless (null invalid) $ do
          prettyThrowIO $ ItemsInvalid invalid
        let sdistOpts = SDistOpts
              uo.itemsToWorkWith
              uo.pvpBounds
              uo.check
              uo.buildPackage
              uo.tarPath
        getCreds <- memoizeRef $ loadAuth config
        mapM_ (resolveFile' >=> checkSDistTarball sdistOpts) files
        forM_ files $ \file -> do
          tarFile <- resolveFile' file
          creds <- runMemoized getCreds
          upload
            hackageUrl
            creds
            SDist
            Nothing
            (toFilePath tarFile)
            uo.uploadVariant
        forM_ dirs $ \dir -> do
          pkgDir <- resolveDir' dir
          (tarName, tarBytes, mcabalRevision) <-
            getSDistTarball uo.pvpBounds pkgDir
          checkSDistTarball' sdistOpts tarName tarBytes
          creds <- runMemoized getCreds
          uploadBytes
            hackageUrl
            creds
            SDist
            Nothing
            tarName
            uo.uploadVariant
            tarBytes
          forM_ mcabalRevision $ uncurry $ uploadRevision hackageUrl creds
   where
    checkDocsTarball ::
         HasEnvConfig env
      => FilePath
      -> RIO env (Either (String, Path Abs File) (String, Path Abs File))
    checkDocsTarball dir = do
      pkgDir <- resolveDir' dir
      distDir <- distDirFromDir pkgDir
      lp <- readLocalPackage pkgDir
      let pkgId = packageIdentifier lp.package
          pkgIdName = packageIdentifierString pkgId
          name = pkgIdName <> "-docs"
      tarGzFileName <- maybe
        (prettyThrowIO $ TarGzFileNameInvalidBug name)
        pure
        ( do nameRelFile <- parseRelFile name
             addExtension ".gz" =<< addExtension ".tar" nameRelFile
        )
      let tarGzFile = distDir Path.</> tarGzFileName
      isFile <- Path.doesFileExist tarGzFile
      pure $ (if isFile then Right else Left) (pkgIdName, tarGzFile)
    partitionM _ [] = pure ([], [])
    partitionM f (x:xs) = do
      r <- f x
      (as, bs) <- partitionM f xs
      pure $ if r then (x:as, bs) else (as, x:bs)

-- | Type representing Hackage API authentification tokens.
newtype HackageKey = HackageKey Text
  deriving (Eq, Show)

-- | Username and password to log into Hackage.
--
-- Since 0.1.0.0
data HackageCreds = HackageCreds
  { username :: !Text
  , password :: !Text
  , credsFile :: !FilePath
  }
  deriving (Eq, Show)

-- | Type representing Hackage authentifications
data HackageAuth
  = HAKey HackageKey
    -- ^ With a Hackage API authentification token registered by a user.
  | HACreds HackageCreds
    -- ^ With a Hackage user's credentials.
  deriving (Eq, Show)

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
withEnvVariable varName fromPrompt =
  lookupEnv (T.unpack varName) >>= maybe fromPrompt (pure . T.pack)

-- | Optionally, load Hackage API authentification token from the @HACKAGE_KEY@
-- environment variable, if it exists.
maybeGetHackageKey :: RIO m (Maybe HackageKey)
maybeGetHackageKey =
  liftIO $ fmap (HackageKey . T.pack) <$> lookupEnv "HACKAGE_KEY"

-- | Load Hackage authentification from the environment, if applicable, or from
-- the given configuration.
loadAuth :: (HasLogFunc m, HasTerm m) => Config -> RIO m HackageAuth
loadAuth config = maybeGetHackageKey >>= \case
  Just key -> do
    prettyInfoL
      [ style Shell "HACKAGE_KEY"
      , flow "environment variable found, using that for credentials."
      ]
    pure $ HAKey key
  Nothing -> HACreds <$> loadUserAndPassword config

-- | Load Hackage credentials, either from a save file or the command
-- line.
--
-- Since 0.1.0.0
loadUserAndPassword :: HasTerm m => Config -> RIO m HackageCreds
loadUserAndPassword config = do
  fp <- liftIO $ credsFile config
  elbs <- liftIO $ tryIO $ L.readFile fp
  case either (const Nothing) Just elbs >>= \lbs -> (lbs, ) <$> decode' lbs of
    Nothing -> fromPrompt fp
    Just (lbs, mkCreds) -> do
      -- Ensure privacy, for cleaning up old versions of Stack that
      -- didn't do this
      writeFilePrivate fp $ lazyByteString lbs

      unless (fromFirstTrue config.saveHackageCreds) $ do
        prettyWarnL
          [ flow "You've set"
          , style Shell "save-hackage-creds"
          , "to"
          , style Shell "false" <> "."
          , flow "However, credentials were found at:"
          , style File (fromString fp) <> "."
          ]
      pure $ mkCreds fp
 where
  fromPrompt :: HasTerm m => FilePath -> RIO m HackageCreds
  fromPrompt fp = do
    username <- liftIO $ withEnvVariable "HACKAGE_USERNAME" (prompt "Hackage username: ")
    password <- liftIO $ withEnvVariable "HACKAGE_PASSWORD" (promptPassword "Hackage password: ")
    let hc = HackageCreds
          { username
          , password
          , credsFile = fp
          }

    when (fromFirstTrue config.saveHackageCreds) $ do
      shouldSave <- promptBool $ T.pack $
        "Save Hackage credentials to file at " ++ fp ++ " [y/n]? "
      prettyNoteL
        [ flow "Avoid this prompt in the future by using the configuration \
               \file option"
        , style Shell (flow "save-hackage-creds: false") <> "."
        ]
      when shouldSave $ do
        writeFilePrivate fp $ fromEncoding $ toEncoding hc
        prettyInfoS "Saved!"
        hFlush stdout

    pure hc

-- | Write contents to a file which is always private.
--
-- For history of this function, see:
--
-- * https://github.com/commercialhaskell/stack/issues/2159#issuecomment-477948928
--
-- * https://github.com/commercialhaskell/stack/pull/4665
writeFilePrivate :: MonadIO m => FilePath -> Builder -> m ()
writeFilePrivate fp builder =
  liftIO $ withTempFile (FP.takeDirectory fp) (FP.takeFileName fp) $ \fpTmp h -> do
    -- Temp file is created such that only current user can read and write it.
    -- See docs for openTempFile:
    -- https://www.stackage.org/haddock/lts-13.14/base-4.12.0.0/System-IO.html#v:openTempFile

    -- Write to the file and close the handle.
    hPutBuilder h builder
    hClose h

    -- Make sure the destination file, if present, is writeable
    void $ tryIO $ setFileMode fp 0o600

    -- And atomically move
    renameFile fpTmp fp

credsFile :: Config -> IO FilePath
credsFile config = do
  let dir = toFilePath (view stackRootL config) FP.</> "upload"
  createDirectoryIfMissing True dir
  pure $ dir FP.</> "credentials.json"

addAPIKey :: HackageKey -> Request -> Request
addAPIKey (HackageKey key) = setRequestHeader
  "Authorization"
  [fromString $ "X-ApiKey" ++ " " ++ T.unpack key]

applyAuth ::
     (HasLogFunc m, HasTerm m)
  => HackageAuth
  -> Request
  -> RIO m Request
applyAuth haAuth req0 =
  case haAuth of
    HAKey key -> pure (addAPIKey key req0)
    HACreds creds -> applyCreds creds req0

applyCreds ::
     (HasLogFunc m, HasTerm m)
  => HackageCreds
  -> Request
  -> RIO m Request
applyCreds creds req0 = do
  manager <- liftIO getGlobalManager
  ereq <- if isStackUploadDisabled
    then do
      debugRequest "applyCreds" req0
      pure (Left $ toException ExitSuccess )
    else
      liftIO $ applyDigestAuth
        (encodeUtf8 creds.username)
        (encodeUtf8 creds.password)
        req0
        manager
  case ereq of
    Left e -> do
      prettyWarn $
           flow "No HTTP digest prompt found, this will probably fail."
        <> blankLine
        <> string
             ( case fromException e of
                 Just e' -> displayDigestAuthException e'
                 Nothing -> displayException e
             )
      pure req0
    Right req -> pure req

-- | Upload a single tarball with the given @Uploader@. Instead of sending a
-- file like 'upload', this sends a lazy bytestring.
--
-- Since 0.1.2.1
uploadBytes ::
     HasTerm m
  => String -- ^ Hackage base URL
  -> HackageAuth
  -> UploadContent
     -- ^ Form of the content to be uploaded.
  -> Maybe String
     -- ^ Optional package identifier name, applies only to the upload of
     -- documentation.
  -> String -- ^ tar file name
  -> UploadVariant
  -> L.ByteString -- ^ tar file contents
  -> RIO m ()
uploadBytes baseUrl auth contentForm mPkgIdName tarName uploadVariant bytes = do
  (url, headers, uploadMethod) <- case contentForm of
    SDist -> do
      unless (isNothing mPkgIdName) $
        prettyThrowIO PackageIdSpecifiedForPackageUploadBug
      let variant = case uploadVariant of
            Publishing -> ""
            Candidate -> "candidates/"
      pure
        ( baseUrl <> "packages/" <> variant
        , [("Accept", "text/plain")]
        , methodPost
        )
    DocArchive -> case mPkgIdName of
      Nothing -> prettyThrowIO PackageIdNotSpecifiedForDocsUploadBug
      Just pkgIdName -> do
        let variant = case uploadVariant of
              Publishing -> ""
              Candidate -> "candidate/"
        pure
          ( baseUrl <> "package/" <> pkgIdName <> "/" <> variant <> "docs"
          , [ ("Content-Type", "application/x-tar")
            , ("Content-Encoding", "gzip")
            ]
          , methodPut
          )
  let req1 = setRequestHeaders headers (fromString url)
      reqData = RequestBodyLBS bytes
      formData = [partFileRequestBody "package" tarName reqData]

  req2 <- case contentForm of
    SDist -> liftIO $ formDataBody formData req1
    DocArchive -> pure $ req1 { requestBody = reqData }
  let req3 = req2 { method = uploadMethod }
  req4 <- applyAuth auth req3
  prettyInfoL
    [ "Uploading"
    , style Current (fromString tarName) <> "..."
    ]
  hFlush stdout
  if isStackUploadDisabled
    then
      debugRequest "uploadBytes" req4
    else
      withRunInIO $ \runInIO -> withResponse req4 (runInIO . inner)
 where
  inner :: HasTerm m => Response (ConduitM () S.ByteString IO ()) -> RIO m ()
  inner res =
    case getResponseStatusCode res of
      200 -> prettyInfoS "done!"
      401 -> do
        case auth of
          HACreds creds ->
            handleIO
              (const $ pure ())
              (liftIO $ removeFile creds.credsFile)
          _ -> pure ()
        prettyThrowIO AuthenticationFailure
      403 -> do
        prettyError $
          "[S-2804]"
          <> line
          <> flow "forbidden upload"
          <> line
          <> flow "Usually means: you've already uploaded this package/version \
                  \combination. Ignoring error and continuing. The full \
                  \message from Hackage is below:"
          <> blankLine
        liftIO $ printBody res
      503 -> do
        prettyError $
          "[S-4444]"
          <> line
          <> flow "service unavailable"
          <> line
          <> flow "This error some times gets sent even though the upload \
                  \succeeded. Check on Hackage to see if your package is \
                  \present. The full message form Hackage is below:"
          <> blankLine
        liftIO $ printBody res
      code -> do
        let resBody = mapOutput show (getResponseBody res)
        resBody' <- liftIO $ runConduit $ resBody .| sinkList
        prettyThrowIO (ArchiveUploadFailure code resBody' tarName)

printBody :: Response (ConduitM () S.ByteString IO ()) -> IO ()
printBody res = runConduit $ getResponseBody res .| CB.sinkHandle stdout

-- | Upload a single tarball with the given @Uploader@.
--
-- Since 0.1.0.0
upload ::
     (HasLogFunc m, HasTerm m)
  => String -- ^ Hackage base URL
  -> HackageAuth
  -> UploadContent
  -> Maybe String
     -- ^ Optional package identifier name, applies only to the upload of
     -- documentation.
  -> FilePath
     -- ^ Path to archive file.
  -> UploadVariant
  -> RIO m ()
upload baseUrl auth contentForm mPkgIdName fp uploadVariant =
  uploadBytes
    baseUrl auth contentForm mPkgIdName (FP.takeFileName fp) uploadVariant
      =<< liftIO (L.readFile fp)

-- | Upload a revised Cabal file for the given package.
uploadRevision ::
     (HasLogFunc m, HasTerm m)
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
  if isStackUploadDisabled
    then
      debugRequest "uploadRevision" req2
    else
      void $ httpNoBody req2

debugRequest :: HasTerm env => String -> Request -> RIO env ()
debugRequest callSite req = prettyInfo $
     fillSep
       [ fromString callSite <> ":"
       , flow "When enabled, would apply the following request:"
       ]
  <> line
  <> fromString (show req)
