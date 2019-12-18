{- stack script
    --resolver lts-11.22
    --install-ghc
    --ghc-options -Wall
    --package Cabal
    --package aeson
    --package bytestring
    --package case-insensitive
    --package conduit
    --package conduit-combinators
    --package cryptohash
    --package directory
    --package extra
    --package http-conduit
    --package http-types
    --package mime-types
    --package process
    --package resourcet
    --package shake
    --package tar
    --package text
    --package zip-archive
    --package zlib
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List
import Data.Maybe
import Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.System
import Distribution.Package
import Distribution.PackageDescription hiding (options)
import Distribution.Verbosity
import System.Console.GetOpt
import System.Environment
import System.Directory
import System.IO.Error
import System.Process

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.GZip as GZip
import Crypto.Hash (Digest, SHA256 (..), digestToHexByteString, hash)
import Data.Aeson
import qualified Data.CaseInsensitive as CI
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.List.Extra
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.Mime
import Prelude -- Silence AMP warning

-- | Entrypoint.
main :: IO ()
main =
    shakeArgsWith
        shakeOptions { shakeFiles = releaseDir
                     , shakeVerbosity = Chatty
                     , shakeChange = ChangeModtimeAndDigestInput }
        options $
        \flags args -> do
            -- build the default value of type Global, with predefined constants

            -- 'stack build --dry-run' just ensures that 'stack.cabal' is generated from hpack
            _ <- readProcess "stack" ["build", "--dry-run"] ""
            gStackPackageDescription <-
                packageDescription <$> readGenericPackageDescription silent "stack.cabal"
            gGpgKey <- maybe defaultGpgKey Just <$> lookupEnv gpgKeyEnvVar
            gGithubAuthToken <- lookupEnv githubAuthTokenEnvVar
            gGitRevCount <- length . lines <$> readProcess "git" ["rev-list", "HEAD"] ""
            gGitSha <- trim <$> readProcess "git" ["rev-parse", "HEAD"] ""
            gHomeDir <- getHomeDirectory

            let gAllowDirty = False
                gGithubReleaseTag = Nothing
                Platform arch _ = buildPlatform
                gArch = arch
                gBinarySuffix = ""
                gUploadLabel = Nothing
                gTestHaddocks = True
                gProjectRoot = "" -- Set to real value velow.
                gBuildArgs = []
                gCertificateName = Nothing
                gUploadOnly = False
                global0 = foldl (flip id) Global{..} flags

            -- Need to get paths after options since the '--arch' argument can effect them.
            projectRoot' <- getStackPath global0 "project-root"
            let global = global0
                    { gProjectRoot = projectRoot' }
            return $ Just $ rules global args
  where
    getStackPath global path = do
      out <- readProcess stackProgName (stackArgs global ++ ["path", "--" ++ path]) ""
      return $ trim $ fromMaybe out $ stripPrefix (path ++ ":") out

-- | Additional command-line options.
options :: [OptDescr (Either String (Global -> Global))]
options =
    [ Option "" [gpgKeyOptName]
        (ReqArg (\v -> Right $ \g -> g{gGpgKey = Just v}) "USER-ID")
        ("GPG user ID to sign distribution package with (defaults to " ++
         gpgKeyEnvVar ++
         " environment variable).")
    , Option "" [allowDirtyOptName] (NoArg $ Right $ \g -> g{gAllowDirty = True})
        "Allow a dirty working tree for release."
    , Option "" [githubAuthTokenOptName]
        (ReqArg (\v -> Right $ \g -> g{gGithubAuthToken = Just v}) "TOKEN")
        ("Github personal access token (defaults to " ++
         githubAuthTokenEnvVar ++
         " environment variable).")
    , Option "" [githubReleaseTagOptName]
        (ReqArg (\v -> Right $ \g -> g{gGithubReleaseTag = Just v}) "TAG")
        "Github release tag to upload to."
    , Option "" [archOptName]
        (ReqArg
            (\v -> case simpleParse v of
                Nothing -> Left $ "Unknown architecture in --arch option: " ++ v
                Just arch -> Right $ \g -> g{gArch = arch})
            "ARCHITECTURE")
        "Architecture to build (e.g. 'i386' or 'x86_64')."
    , Option "" [binaryVariantOptName]
        (ReqArg (\v -> Right $ \g -> g{gBinarySuffix = v}) "SUFFIX")
        "Extra suffix to add to binary executable archive filename."
    , Option "" [uploadLabelOptName]
        (ReqArg (\v -> Right $ \g -> g{gUploadLabel = Just v}) "LABEL")
        "Label to give the uploaded release asset"
    , Option "" [noTestHaddocksOptName] (NoArg $ Right $ \g -> g{gTestHaddocks = False})
        "Disable testing building haddocks."
    , Option "" [staticOptName] (NoArg $ Right $ \g -> g{gBuildArgs = gBuildArgs g ++ ["--flag=stack:static"]})
        "Build a static binary."
    , Option "" [buildArgsOptName]
        (ReqArg
            (\v -> Right $ \g -> g{gBuildArgs = gBuildArgs g ++ words v})
            "\"ARG1 ARG2 ...\"")
        "Additional arguments to pass to 'stack build'."
    , Option "" [certificateNameOptName]
        (ReqArg (\v -> Right $ \g -> g{gCertificateName = Just v}) "NAME")
        "Certificate name for code signing on Windows"
    , Option "" [uploadOnlyOptName] (NoArg $ Right $ \g -> g{gUploadOnly = True})
        "Just upload an existing file, but don't try to build it."
    ]

-- | Shake rules.
rules :: Global -> [String] -> Rules ()
rules global@Global{..} args = do
    case args of
        [] -> error "No wanted target(s) specified."
        _ -> want args

    phony releasePhony $ do
        need [checkPhony]
        need [uploadPhony]

    phony cleanPhony $
        removeFilesAfter releaseDir ["//*"]

    phony checkPhony $
        need [releaseCheckDir </> binaryExeFileName]

    phony uploadPhony $
        mapM_ (\f -> need [releaseDir </> f <.> uploadExt]) binaryPkgAndSigFileNames

    phony buildPhony $
        mapM_ (\f -> need [releaseDir </> f]) binaryPkgAndSigFileNames

    releaseDir </> "*" <.> uploadExt %> \out -> do
        let srcFile = dropExtension out
            -- mUploadLabel =
            --     case takeExtension srcFile of
            --         e | e == ascExt -> fmap (++ " (GPG signature)") gUploadLabel
            --           | e == sha256Ext -> fmap (++ " (SHA256 checksum)") gUploadLabel
            --           | otherwise -> gUploadLabel
        need [srcFile]
        uploadToGithubRelease global srcFile Nothing
        copyFileChanged srcFile out

    releaseCheckDir </> binaryExeFileName %> \out -> do
        need [releaseBinDir </> binaryName </> stackExeFileName]
        Stdout dirty <- cmd "git status --porcelain"
        when (not gAllowDirty && not (null (trim dirty))) $
            error ("Working tree is dirty.  Use --" ++ allowDirtyOptName ++ " option to continue anyway.")
        withTempDir $ \tmpDir -> do
            let cmd0 c = cmd [gProjectRoot </> releaseBinDir </> binaryName </> stackExeFileName]
                    (stackArgs global)
                    ["--local-bin-path=" ++ tmpDir]
                    c
            -- 'stack.cabal' is autogenerated by 'stack', delete to ensure not stale
            _ <- liftIO $ tryJust (guard . isDoesNotExistError) (removeFile "stack.cabal")
            () <- cmd0 "install" gBuildArgs integrationTestFlagArgs $ concat $ concat
                [["--pedantic --no-haddock-deps "]
                ,[" --haddock" | gTestHaddocks]
                ,[" stack"]]
            let cmd' c = cmd (AddPath [tmpDir] []) stackProgName (stackArgs global) c
            () <- cmd' "test" gBuildArgs integrationTestFlagArgs "--pedantic --exec stack-integration-test stack"
            return ()
        copyFileChanged (releaseBinDir </> binaryName </> stackExeFileName) out

    unless gUploadOnly $ releaseDir </> binaryPkgZipFileName %> \out -> do
        stageFiles <- getBinaryPkgStageFiles
        putNormal $ "zip " ++ out
        liftIO $ do
            entries <- forM stageFiles $ \stageFile -> do
                Zip.readEntry
                    [Zip.OptLocation
#if MIN_VERSION_zip_archive(0,3,0)
                        (dropFileName (dropDirectoryPrefix (releaseStageDir </> binaryName) stageFile))
#else
                        (dropDirectoryPrefix (releaseStageDir </> binaryName) stageFile)
#endif
                        False]
                    stageFile
            let archive = foldr Zip.addEntryToArchive Zip.emptyArchive entries
            L8.writeFile out (Zip.fromArchive archive)

    unless gUploadOnly $ releaseDir </> binaryPkgTarGzFileName %> \out -> do
        stageFiles <- getBinaryPkgStageFiles
        writeTarGz out releaseStageDir stageFiles

    releaseStageDir </> binaryName </> stackExeFileName %> \out -> do
        copyFileChanged (releaseDir </> binaryExeFileName) out

    releaseStageDir </> (binaryName ++ "//*") %> \out -> do
        copyFileChanged
            (dropDirectoryPrefix (releaseStageDir </> binaryName) out)
            out

    unless gUploadOnly $ releaseDir </> binaryExeFileName %> \out -> do
        need [releaseBinDir </> binaryName </> stackExeFileName]
        (Stdout versionOut) <- cmd (releaseBinDir </> binaryName </> stackExeFileName) "--version"
        when (not gAllowDirty && "dirty" `isInfixOf` lower versionOut) $
            error ("Refusing continue because 'stack --version' reports dirty.  Use --" ++
                   allowDirtyOptName ++ " option to continue anyway.")
        case platformOS of
            Windows -> do
                -- Windows doesn't have or need a 'strip' command, so skip it.
                -- Instead, we sign the executable
                liftIO $ copyFile (releaseBinDir </> binaryName </> stackExeFileName) out
                case gCertificateName of
                    Nothing -> return ()
                    Just certName ->
                        actionOnException
                            (command_ [] "c:\\Program Files\\Microsoft SDKs\\Windows\\v7.1\\Bin\\signtool.exe"
                                ["sign"
                                ,"/v"
                                ,"/d", synopsis gStackPackageDescription
                                ,"/du", homepage gStackPackageDescription
                                ,"/n", certName
                                ,"/t", "http://timestamp.verisign.com/scripts/timestamp.dll"
                                ,out])
                            (removeFile out)
            Linux ->
                cmd "strip -p --strip-unneeded --remove-section=.comment -o"
                    [out, releaseBinDir </> binaryName </> stackExeFileName]
            _ ->
                cmd "strip -o"
                    [out, releaseBinDir </> binaryName </> stackExeFileName]

    releaseDir </> binaryInstallerFileName %> \out -> do
        need [releaseDir </> binaryExeFileName]
        need [releaseDir </> binaryInstallerNSIFileName]

        actionOnException
            (command_ [Cwd releaseDir] "c:\\Program Files (x86)\\NSIS\\Unicode\\makensis.exe"
                [ "-V3"
                , binaryInstallerNSIFileName])
            (removeFile out)

    releaseDir </> binaryInstallerNSIFileName %> \out -> do
        need ["etc" </> "scripts" </> "build-stack-installer" <.> "hs"]
        cmd "stack etc/scripts/build-stack-installer.hs" 
            [ binaryExeFileName
            , binaryInstallerFileName
            , out
            ] :: Action ()

    releaseDir </> "*" <.> ascExt %> \out -> do
        need [out -<.> ""]
        _ <- liftIO $ tryJust (guard . isDoesNotExistError) (removeFile out)
        case gGpgKey of
            Nothing -> error "No GPG key specified"
            Just gpgKey ->
                cmd ("gpg " ++ gpgOptions ++ " --detach-sig --armor")
                    [ "-u", gpgKey
                    , dropExtension out ]

    releaseDir </> "*" <.> sha256Ext %> \out -> do
        need [out -<.> ""]
        bs <- liftIO $ do
            _ <- tryJust (guard . isDoesNotExistError) (removeFile out)
            S8.readFile (dropExtension out)
        writeFileChanged
          out
          ( S8.unpack (digestToHexByteString (hash bs :: Digest SHA256)) ++
            "  " ++
            takeFileName (dropExtension out) ++
            "\n" )

    releaseBinDir </> binaryName </> stackExeFileName %> \out -> do
        alwaysRerun
        -- 'stack.cabal' is autogenerated by 'stack', delete to ensure not stale
        _ <- liftIO $ tryJust (guard . isDoesNotExistError) (removeFile "stack.cabal")
        actionOnException
            (cmd stackProgName
                (stackArgs global)
                ["--local-bin-path=" ++ takeDirectory out]
                "install"
                gBuildArgs
                integrationTestFlagArgs
                "--pedantic"
                "stack")
            (tryJust (guard . isDoesNotExistError) (removeFile out))

  where

    integrationTestFlagArgs =
        -- Explicitly enabling 'hide-dependency-versions' and 'supported-build' to work around
        -- https://github.com/commercialhaskell/stack/issues/4960
        [ "--flag=stack:integration-tests"
        , "--flag=stack:hide-dependency-versions"
        , "--flag=stack:supported-build"
        ]

    getBinaryPkgStageFiles = do
        docFiles <- getDocFiles
        let stageFiles = concat
                [[releaseStageDir </> binaryName </> stackExeFileName]
                ,map ((releaseStageDir </> binaryName) </>) docFiles]
        need stageFiles
        return stageFiles

    getDocFiles = getDirectoryFiles "." ["LICENSE", "*.md", "doc//*.md"]

    releasePhony = "release"
    checkPhony = "check"
    uploadPhony = "upload"
    cleanPhony = "clean"
    buildPhony = "build"

    releaseCheckDir = releaseDir </> "check"
    releaseStageDir = releaseDir </> "stage"
    releaseBinDir = releaseDir </> "bin"

    binaryPkgAndSigFileNames =
        concatMap sigHashFileNames binaryPkgFileNames
    sigHashFileNames x =
        case gGpgKey of
            Nothing -> [x, x <.> sha256Ext]
            Just _ -> [x, x <.> sha256Ext, x <.> ascExt]
    binaryPkgFileNames =
        case platformOS of
            Windows -> [binaryExeFileName, binaryPkgZipFileName, binaryPkgTarGzFileName, binaryInstallerFileName]
            _ -> [binaryExeFileName, binaryPkgTarGzFileName]
    binaryPkgZipFileName = binaryName <.> zipExt
    binaryPkgTarGzFileName = binaryName <.> tarGzExt
    -- Adding '-bin' to name to work around https://github.com/commercialhaskell/stack/issues/4961
    binaryExeFileName = binaryName ++ "-bin" <.> exe
    binaryInstallerNSIFileName = binaryName ++ "-installer" <.> nsiExt
    binaryInstallerFileName = binaryName ++ "-installer" <.> exe
    binaryName =
        concat
            [ stackProgName
            , "-"
            , stackVersionStr global
            , "-"
            , display platformOS
            , "-"
            , display gArch
            , if null gBinarySuffix then "" else "-" ++ gBinarySuffix ]
    stackExeFileName = stackProgName <.> exe

    zipExt = ".zip"
    tarGzExt = tarExt <.> gzExt
    gzExt = ".gz"
    tarExt = ".tar"
    ascExt = ".asc"
    sha256Ext = ".sha256"
    uploadExt = ".upload"
    nsiExt = ".nsi"

-- | Upload file to Github release.
uploadToGithubRelease :: Global -> FilePath -> Maybe String -> Action ()
uploadToGithubRelease global@Global{..} file mUploadLabel = do
    -- TODO: consider using https://github.com/tfausak/github-release
    putNormal $ "Uploading to Github: " ++ file
    GithubRelease{..} <- getGithubRelease
    resp <- liftIO $ callGithubApi global
        [(CI.mk $ S8.pack "Content-Type", defaultMimeLookup (T.pack file))]
        (Just file)
        (replace
            "{?name,label}"
            ("?name=" ++ urlEncodeStr (takeFileName file) ++
             (case mUploadLabel of
                 Nothing -> ""
                 Just uploadLabel -> "&label=" ++ urlEncodeStr uploadLabel))
            relUploadUrl)
    case eitherDecode resp of
        Left e -> error ("Could not parse Github asset upload response (" ++ e ++ "):\n" ++ L8.unpack resp ++ "\n")
        Right (GithubReleaseAsset{..}) ->
            when (assetState /= "uploaded") $
                error ("Invalid asset state after Github asset upload: " ++ assetState)
  where
    urlEncodeStr = S8.unpack . urlEncode True . S8.pack
    getGithubRelease = do
        releases <- getGithubReleases
        let tag = fromMaybe ("v" ++ stackVersionStr global) gGithubReleaseTag
        return $ fromMaybe
            (error ("Could not find Github release with tag '" ++ tag ++ "'.\n" ++
                    "Use --" ++ githubReleaseTagOptName ++ " option to specify a different tag."))
            (find (\r -> relTagName r == tag) releases)
    getGithubReleases :: Action [GithubRelease]
    getGithubReleases = do
        resp <- liftIO $ callGithubApi global
            [] Nothing "https://api.github.com/repos/commercialhaskell/stack/releases"
        case eitherDecode resp of
            Left e -> error ("Could not parse Github releases (" ++ e ++ "):\n" ++ L8.unpack resp ++ "\n")
            Right r -> return r

-- | Make a request to the Github API and return the response.
callGithubApi :: Global -> RequestHeaders -> Maybe FilePath -> String -> IO L8.ByteString
callGithubApi Global{..} headers mpostFile url = do
    req0 <- parseUrlThrow url
    let authToken =
            fromMaybe
                (error $
                     "Github auth token required.\n" ++
                     "Use " ++ githubAuthTokenEnvVar ++ " environment variable\n" ++
                     "or --" ++ githubAuthTokenOptName ++ " option to specify.")
                gGithubAuthToken
        req1 =
            req0
                { requestHeaders =
                    [ (CI.mk $ S8.pack "Authorization", S8.pack $ "token " ++ authToken)
                    , (CI.mk $ S8.pack "User-Agent", S8.pack "commercialhaskell/stack") ] ++
                    headers }
    req <- case mpostFile of
        Nothing -> return req1
        Just postFile -> do
            lbs <- L8.readFile postFile
            return $ req1
                { method = S8.pack "POST"
                , requestBody = RequestBodyLBS lbs }
    manager <- newManager tlsManagerSettings
    runResourceT $ do
        res <- http req manager
        runConduit $ responseBody res .| CC.sinkLazy

-- | Create a .tar.gz files from files.  The paths should be absolute, and will
-- be made relative to the base directory in the tarball.
writeTarGz :: FilePath -> FilePath -> [FilePath] -> Action ()
writeTarGz out baseDir inputFiles = liftIO $ do
    content <- Tar.pack baseDir $ map (dropDirectoryPrefix baseDir) inputFiles
    L8.writeFile out $ GZip.compress $ Tar.write content

-- | Drops a directory prefix from a path.  The prefix automatically has a path
-- separator character appended.  Fails if the path does not begin with the prefix.
dropDirectoryPrefix :: FilePath -> FilePath -> FilePath
dropDirectoryPrefix prefix path =
    case stripPrefix (toStandard prefix ++ "/") (toStandard path) of
        Nothing -> error ("dropDirectoryPrefix: cannot drop " ++ show prefix ++ " from " ++ show path)
        Just stripped -> stripped

-- | String representation of stack package version.
stackVersionStr :: Global -> String
stackVersionStr =
    display . pkgVersion . package . gStackPackageDescription

-- | Current operating system.
platformOS :: OS
platformOS =
    let Platform _ os = buildPlatform
    in os

-- | Directory in which to store build and intermediate files.
releaseDir :: FilePath
releaseDir = "_release"

-- | @GITHUB_AUTH_TOKEN@ environment variable name.
githubAuthTokenEnvVar :: String
githubAuthTokenEnvVar = "GITHUB_AUTH_TOKEN"

-- | @--github-auth-token@ command-line option name.
githubAuthTokenOptName :: String
githubAuthTokenOptName = "github-auth-token"

-- | @--github-release-tag@ command-line option name.
githubReleaseTagOptName :: String
githubReleaseTagOptName = "github-release-tag"

-- | Default GPG key ID for signing bindists
defaultGpgKey :: Maybe String
defaultGpgKey = Nothing

-- | @STACK_RELEASE_GPG_KEY@ environment variable name.
gpgKeyEnvVar :: String
gpgKeyEnvVar =  "STACK_RELEASE_GPG_KEY"

-- | @--gpg-key@ command-line option name.
gpgKeyOptName :: String
gpgKeyOptName = "gpg-key"

-- | @--allow-dirty@ command-line option name.
allowDirtyOptName :: String
allowDirtyOptName = "allow-dirty"

-- | @--arch@ command-line option name.
archOptName :: String
archOptName = "arch"

-- | @--binary-variant@ command-line option name.
binaryVariantOptName :: String
binaryVariantOptName = "binary-variant"

-- | @--upload-label@ command-line option name.
uploadLabelOptName :: String
uploadLabelOptName = "upload-label"

-- | @--no-test-haddocks@ command-line option name.
noTestHaddocksOptName :: String
noTestHaddocksOptName = "no-test-haddocks"

-- | @--build-args@ command-line option name.
buildArgsOptName :: String
buildArgsOptName = "build-args"

-- | @--static@ command-line option name.
staticOptName :: String
staticOptName = "static"

-- | @--certificate-name@ command-line option name.
certificateNameOptName :: String
certificateNameOptName = "certificate-name"

-- | @--upload-only@ command-line option name.
uploadOnlyOptName :: String
uploadOnlyOptName = "upload-only"

-- | Arguments to pass to all 'stack' invocations.
stackArgs :: Global -> [String]
stackArgs Global{..} = ["--install-ghc", "--arch=" ++ display gArch, "--interleaved-output"]

-- | Name of the 'stack' program.
stackProgName :: FilePath
stackProgName = "stack"

-- | Options to pass to invocations of gpg
gpgOptions :: String
gpgOptions = "--digest-algo=sha512"

-- | Linux distribution/version combination.
data DistroVersion = DistroVersion
    { dvDistro :: !String
    , dvVersion :: !String
    , dvCodeName :: !String }

-- | A Github release, as returned by the Github API.
data GithubRelease = GithubRelease
    { relUploadUrl :: !String
    , relTagName :: !String }
    deriving (Show)
instance FromJSON GithubRelease where
    parseJSON = withObject "GithubRelease" $ \o ->
        GithubRelease
        <$> o .: T.pack "upload_url"
        <*> o .: T.pack "tag_name"

-- | A Github release asset, as returned by the Github API.
data GithubReleaseAsset = GithubReleaseAsset
    { assetState :: !String }
    deriving (Show)
instance FromJSON GithubReleaseAsset where
    parseJSON = withObject "GithubReleaseAsset" $ \o ->
        GithubReleaseAsset
        <$> o .: T.pack "state"

-- | Global values and options.
data Global = Global
    { gStackPackageDescription :: !PackageDescription
    , gGpgKey :: !(Maybe String)
    , gAllowDirty :: !Bool
    , gGithubAuthToken :: !(Maybe String)
    , gGithubReleaseTag :: !(Maybe String)
    , gGitRevCount :: !Int
    , gGitSha :: !String
    , gProjectRoot :: !FilePath
    , gHomeDir :: !FilePath
    , gArch :: !Arch
    , gBinarySuffix :: !String
    , gUploadLabel :: !(Maybe String)
    , gTestHaddocks :: !Bool
    , gBuildArgs :: [String]
    , gCertificateName :: !(Maybe String)
    , gUploadOnly :: !Bool
    }
    deriving (Show)
