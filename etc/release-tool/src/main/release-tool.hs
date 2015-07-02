{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List
import Data.Maybe
import Data.String.Here
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

import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.GZip as GZip
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

-- | Entrypoint.
main :: IO ()
main =
    shakeArgsWith
        shakeOptions { shakeFiles = releaseDir
                     , shakeVerbosity = Chatty
                     , shakeChange = ChangeModtimeAndDigestInput }
        options $
        \flags args -> do
            gStackPackageDescription <-
                packageDescription <$> readPackageDescription silent "stack.cabal"
            gGithubAuthToken <- lookupEnv githubAuthTokenEnvVar
            gLocalInstallRoot <- getStackPath "local-install-root"
            gProjectRoot <- getStackPath "project-root"
            gGitRevCount <- read . trim <$> readProcess "git" ["rev-list", "HEAD", "--count"] ""
            gGitSha <- trim <$> readProcess "git" ["rev-parse", "HEAD"] ""
            let gGpgKey = Nothing
                gAllowDirty = False
                gGithubReleaseTag = Nothing
            return $ Just $ rules (foldl (flip id) Global{..} flags) args
  where
    getStackPath path = do
      out <- readProcess "stack" ["path", "--" ++ path] ""
      return $ trim $ fromMaybe out $ stripPrefix (path ++ ":") out

-- | Additional command-line options.
options :: [OptDescr (Either String (Global -> Global))]
options =
    [ Option "" [gpgKeyOptName]
        (ReqArg (\v -> Right $ \g -> g{gGpgKey = Just v}) "USER-ID")
        "GPG user ID to sign distribution package with"
    , Option "" [allowDirtyOptName] (NoArg $ Right $ \g -> g{gAllowDirty = True})
        "Allow a dirty working tree for release."
    , Option "" [githubAuthTokenOptName]
        (ReqArg (\v -> Right $ \g -> g{gGithubAuthToken = Just v}) "TOKEN")
        ("Github personal access token (defaults to " ++
         githubAuthTokenEnvVar ++
         " environment variable).")
    , Option "" [githubReleaseTagOptName]
        (ReqArg (\v -> Right $ \g -> g{gGithubReleaseTag = Just v}) "TAG")
        "Github release tag to upload to." ]

-- | Shake rules.
rules :: Global -> [String] -> Rules ()
rules global@Global{..} args = do
    case args of
        [] -> error "No wanted target(s) specified."
        _ -> want args

    phony ubuntuUploadPhony $
        mapM_
            (\v -> need [ubuntuVersionDebDir (fst v) </> ubuntuVersionDebFileName <.> uploadExt])
            ubuntuVersions

    phony ubuntuPackagesPhony $
        mapM_
            (\v -> need [ubuntuVersionDebDir (fst v) </> ubuntuVersionDebFileName])
            ubuntuVersions

    phony releasePhony $ do
        need [checkPhony]
        need [uploadPhony]

    phony cleanPhony $
        removeFilesAfter releaseDir ["//*"]

    phony checkPhony $
        need [releaseCheckDir </> stackExeFileName]

    phony uploadPhony $
        mapM_ (\f -> need [releaseDir </> f <.> uploadExt]) releaseFileNames

    phony buildPhony $
        mapM_ (\f -> need [releaseDir </> f]) releaseFileNames

    releaseDir </> "*" <.> uploadExt %> \out -> do
        need [dropExtension out]
        uploadToGithubRelease global (dropExtension out)
        copyFile' (dropExtension out) out

    releaseCheckDir </> stackExeFileName %> \out -> do
        need [installBinDir </> stackExeFileName]
        Stdout dirty <- cmd "git status --porcelain"
        when (not gAllowDirty && not (null (trim dirty))) $
            error ("Working tree is dirty.  Use --" ++ allowDirtyOptName ++ " option to continue anyway.")
        let instExeFile = installBinDir </> stackExeFileName
            tmpExeFile = installBinDir </> stackExeFileName <.> "tmp"
        --FIXME: once 'stack install --path' implemented, use it instead of this temp file.
        liftIO $ renameFile instExeFile tmpExeFile
        actionFinally
            (do opt <- addPath [installBinDir] []
                () <- cmd opt "stack build"
                () <- cmd opt "stack clean"
                () <- cmd opt "stack build --pedantic"
                () <- cmd opt "stack test --flag stack:integration-tests"
                return ())
            (renameFile tmpExeFile instExeFile)
        copyFileChanged (installBinDir </> stackExeFileName) out

    releaseDir </> releaseExeZipFileName %> \out -> do
        need [releaseDir </> stackExeFileName]
        putNormal $ "zip " ++ (releaseDir </> stackExeFileName)
        liftIO $ do
            entry <- Zip.readEntry [] (releaseDir </> stackExeFileName)
            let entry' = entry{Zip.eRelativePath = stackExeFileName}
                archive = Zip.addEntryToArchive entry' Zip.emptyArchive
            L8.writeFile out (Zip.fromArchive archive)

    releaseDir </> releaseExeGzFileName %> \out -> do
        need [releaseDir </> stackExeFileName]
        putNormal $ "gzip " ++ (releaseDir </> stackExeFileName)
        liftIO $ do
            fc <- L8.readFile (releaseDir </> stackExeFileName)
            L8.writeFile out $ GZip.compress fc

    releaseDir </> stackExeFileName %> \out -> do
        need [installBinDir </> stackExeFileName]
        case platformOS of
            Windows ->
                -- Windows doesn't have or need a 'strip' command, so skip it.
                liftIO $ copyFile (installBinDir </> stackExeFileName) out
            Linux ->
                cmd "strip -p --strip-unneeded --remove-section=.comment -o"
                    [out, installBinDir </> stackExeFileName]
            _ ->
                cmd "strip -o"
                    [out, installBinDir </> stackExeFileName]

    releaseDir </> releaseExeCompressedAscFileName %> \out -> do
        need [out -<.> ""]
        _ <- liftIO $ tryJust (guard . isDoesNotExistError) (removeFile out)
        cmd "gpg --detach-sig --armor"
            (maybe [] (\k -> ["-u", k]) gGpgKey)
            [out -<.> ""]

    installBinDir </> stackExeFileName %> \_ -> do
        alwaysRerun
        cmd "stack build"

    ubuntuVersionDebDir "*" </> ubuntuVersionDebFileName <.> uploadExt %> \out -> do
        let ubuntuVersion = ubuntuVersionFromPath out
        need [ubuntuVersionDebDir ubuntuVersion </> ubuntuVersionDebFileName]
        () <- cmd "deb-s3 upload -b download.fpcomplete.com --sign=9BEFB442"
            [ "--prefix=ubuntu/" ++ ubuntuCodeName ubuntuVersion
            , dropExtension out ]
        copyFileChanged (dropExtension out) out

    ubuntuVersionDebDir "*" </> ubuntuVersionDebFileName %> \out -> do
        alwaysRerun
        let ubuntuVersion = ubuntuVersionFromPath out
        need [ubuntuVersionDir ubuntuVersion </> imageIDFileName]
        liftIO $ createDirectoryIfMissing True (takeDirectory out)
        cmd "docker run --rm"
            [ "--volume=" ++ gProjectRoot </> ubuntuVersionDir ubuntuVersion </> "stack-root" ++
              ":/mnt/stack-root"
            , "--env=STACK_ROOT=/mnt/stack-root"
            , "--volume=" ++ gProjectRoot </> ubuntuVersionDir ubuntuVersion </> "stack-work" ++
              ":/mnt/src/.stack-work"
            , "--volume=" ++ gProjectRoot ++ ":/mnt/src"
            , "--workdir=/mnt/src"
            , "--volume=" ++ gProjectRoot </> ubuntuVersionDebDir ubuntuVersion ++ ":/mnt/deb"
            , "--env=OUTPUT_DEB=/mnt/deb/" ++ ubuntuVersionDebFileName
            , "--env=DEB_VERSION=" ++ ubuntuVersionDebVersionStr
            , "--env=PKG_MAINTAINER=" ++ maintainer gStackPackageDescription
            , "--env=PKG_DESCRIPTION=" ++ synopsis gStackPackageDescription
            , "--env=PKG_LICENSE=" ++ display (license gStackPackageDescription)
            , "--env=PKG_URL=" ++ homepage gStackPackageDescription
            , ubuntuDockerImageTag ubuntuVersion]

    ubuntuVersionDir "*" </> imageIDFileName %> \out -> do
        alwaysRerun
        let ubuntuVersion = ubuntuVersionFromPath out
            imageTag = ubuntuDockerImageTag ubuntuVersion
        need
            [ ubuntuVersionDockerDir ubuntuVersion </> "Dockerfile"
            , ubuntuVersionDockerDir ubuntuVersion </> "run.sh" ]
        _ <- buildDockerImage (ubuntuVersionDockerDir ubuntuVersion) imageTag out
        return ()

    ubuntuVersionDockerDir "*" </> "Dockerfile" %> \out -> do
        alwaysRerun
        let ubuntuVersion = ubuntuVersionFromPath out
        writeFileChanged out [template|templates/ubuntu-packages/docker/Dockerfile|]

    ubuntuVersionDockerDir "*" </> "run.sh" %> \out -> do
        alwaysRerun
        writeFileChanged out [hereFile|templates/ubuntu-packages/docker/run.sh|]

  where
    ubuntuVersionFromPath path =
        case stripPrefix (ubuntuVersionDir "" ++ "/") path of
            Nothing -> error ("Cannot determine Ubuntu version from path: " ++ path)
            Just path' -> takeDirectory1 path'

    releasePhony = "release"
    checkPhony = "check"
    uploadPhony = "upload"
    cleanPhony = "clean"
    buildPhony = "build"
    ubuntuPackagesPhony = "ubuntu-packages"
    ubuntuUploadPhony = "ubuntu-upload"

    releaseCheckDir = releaseDir </> "check"
    installBinDir = gLocalInstallRoot </> "bin"
    ubuntuVersionDir ver = releaseDir </> "ubuntu" </> ver
    ubuntuVersionDockerDir ver = ubuntuVersionDir ver </> "docker"
    ubuntuVersionDebDir ver = ubuntuVersionDir ver </> "deb"

    stackExeFileName = stackProgName <.> exe
    releaseFileNames = [releaseExeCompressedFileName, releaseExeCompressedAscFileName]
    releaseExeCompressedAscFileName = releaseExeCompressedFileName <.> ascExt
    releaseExeCompressedFileName =
        case platformOS of
            Windows -> releaseExeZipFileName
            _ -> releaseExeGzFileName
    releaseExeZipFileName = releaseExeFileNameNoExt <.> zipExt
    releaseExeGzFileName = releaseExeFileName <.> gzExt
    releaseExeFileName = releaseExeFileNameNoExt <.> exe
    releaseExeFileNameNoExt = releaseName global
    ubuntuVersionDebFileName =
        concat [stackProgName, "_", ubuntuVersionDebVersionStr, "_amd64"] <.> "deb"
    imageIDFileName = "image-id"

    zipExt = "zip"
    gzExt = "gz"
    ascExt = "asc"
    uploadExt = "upload"

    ubuntuDockerImageTag ver = "stack_release_tool/ubuntu:" ++ ver
    ubuntuVersionDebVersionStr =
        concat [stackVersionStr global, "-", show gGitRevCount, "-", gGitSha]

    ubuntuVersions =
        [ ("12.04", "precise")
        , ("14.04", "trusty")
        , ("14.10", "utopic")
        , ("15.04", "vivid") ]
    ubuntuCodeName v =
        fromMaybe
            ("Unknown Ubuntu version: " ++ v)
            (lookup v ubuntuVersions)

-- | Upload file to Github release.
uploadToGithubRelease :: Global -> FilePath -> Action ()
uploadToGithubRelease global@Global{..} file = do
    putNormal $ "Uploading to Github: " ++ file
    GithubRelease{..} <- getGithubRelease
    resp <- liftIO $ callGithubApi global
        [(CI.mk $ S8.pack "Content-Type", defaultMimeLookup (T.pack file))]
        (Just file)
        (replace
            "{?name}"
            ("?name=" ++ S8.unpack (urlEncode True (S8.pack (takeFileName file))))
            relUploadUrl)
    case eitherDecode resp of
        Left e -> error ("Could not parse Github asset upload response (" ++ e ++ "):\n" ++ L8.unpack resp ++ "\n")
        Right (GithubReleaseAsset{..}) ->
            when (assetState /= "uploaded") $
                error ("Invalid asset state after Github asset upload: " ++ assetState)
  where
    getGithubRelease = do
        releases <- getGithubReleases
        let tag = fromMaybe ("v" ++ stackVersionStr global) gGithubReleaseTag
        return $ fromMaybe
            (error ("Could not find Github release with tag '" ++ tag ++ "'.\n" ++
                    "Use --" ++ githubReleaseTagOptName ++ " option to specify a different tag."))
            (find (\r -> relTagName r == tag) releases)
    getGithubReleases = do
        resp <- liftIO $ callGithubApi global
            [] Nothing "https://api.github.com/repos/commercialhaskell/stack/releases"
        case eitherDecode resp of
            Left e -> error ("Could not parse Github releases (" ++ e ++ "):\n" ++ L8.unpack resp ++ "\n")
            Right r -> return r

-- | Make a request to the Github API and return the response.
callGithubApi :: Global -> RequestHeaders -> Maybe FilePath -> String -> IO L8.ByteString
callGithubApi Global{..} headers mpostFile url = do
    req0 <- parseUrl url
    let authToken =
            fromMaybe
                (error $
                     "Github auth token required.\n" ++
                     "Use " ++ githubAuthTokenEnvVar ++ " environment variable\n" ++
                     "or --" ++ githubAuthTokenOptName ++ " option to specify.")
                gGithubAuthToken
        req1 =
            req0
                { checkStatus = \_ _ _ -> Nothing
                , requestHeaders =
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
    withManager $ \manager -> do
        res <- http req manager
        responseBody res $$+- CC.sinkLazy

-- | Build a Docker image and write its ID to a file if changed.
buildDockerImage :: FilePath -> String -> FilePath -> Action String
buildDockerImage buildDir imageTag out = do
    alwaysRerun
    () <- cmd "docker build" ["--tag=" ++ imageTag, buildDir]
    (Stdout imageIdOut) <- cmd "docker inspect --format={{.Id}}" [imageTag]
    writeFileChanged out imageIdOut
    return (trim imageIdOut)

-- | Name of the release binary (e.g. @stack-x.y.x-arch-os@)
releaseName :: Global -> String
releaseName global = concat [stackProgName, "-", stackVersionStr global, "-", platformName]

-- | String representation of stack package version.
stackVersionStr :: Global -> String
stackVersionStr = display . pkgVersion . package . gStackPackageDescription

-- | Name of current platform.
platformName :: String
platformName = display buildPlatform

-- | Current operating system.
platformOS :: OS
platformOS =
    let Platform _ os = buildPlatform
    in os

-- | Directory in which to store build and intermediate files.
releaseDir :: FilePath
releaseDir = "_release"

-- | @GITHUB_AUTH_TOKEN@ environment variale name.
githubAuthTokenEnvVar :: String
githubAuthTokenEnvVar = "GITHUB_AUTH_TOKEN"

-- | @--github-auth-token@ command-line option name.
githubAuthTokenOptName :: String
githubAuthTokenOptName = "github-auth-token"

-- | @--github-release-tag@ command-line option name.
githubReleaseTagOptName :: String
githubReleaseTagOptName = "github-release-tag"

-- | @--gpg-key@ command-line option name.
gpgKeyOptName :: String
gpgKeyOptName = "gpg-key"

-- | @--allow-dirty@ command-line option name.
allowDirtyOptName :: String
allowDirtyOptName = "allow-dirty"

-- | Name of the 'stack' program.
stackProgName :: FilePath
stackProgName = "stack"

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
    , gLocalInstallRoot :: !FilePath
    , gGpgKey :: !(Maybe String)
    , gAllowDirty :: !Bool
    , gGithubAuthToken :: !(Maybe String)
    , gGithubReleaseTag :: !(Maybe String)
    , gGitRevCount :: !Integer
    , gGitSha :: !String
    , gProjectRoot :: !FilePath
    }
