#!/usr/bin/env stack
-- stack --install-ghc runghc --package=shake --package=extra --package=zip-archive --package=mime-types --package=http-types --package=http-conduit --package=text --package=conduit-combinators --package=conduit --package=case-insensitive --package=aeson --package=zlib --package tar
{-# OPTIONS_GHC -Wall -Werror #-}
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
            gStackPackageDescription <-
                packageDescription <$> readPackageDescription silent "stack.cabal"
            gGithubAuthToken <- lookupEnv githubAuthTokenEnvVar
            gGitRevCount <- length . lines <$> readProcess "git" ["rev-list", "HEAD"] ""
            gGitSha <- trim <$> readProcess "git" ["rev-parse", "HEAD"] ""
            gHomeDir <- getHomeDirectory
            let -- @gScriptPath@ was retrived using the @executable-path@ package, but it
                -- has trouble with GHC 7.10.2 on OS X
                gScriptPath = "scripts/release/release.hs"
                gGpgKey = "9BEFB442"
                gAllowDirty = False
                gGithubReleaseTag = Nothing
                Platform arch _ = buildPlatform
                gArch = arch
                gBinarySuffix = ""
                gLocalInstallRoot = "" -- Set to real value below.
                gProjectRoot = "" -- Set to real value velow.
                global0 = foldl (flip id) Global{..} flags
            -- Need to get paths after options since the '--arch' argument can effect them.
            localInstallRoot' <- getStackPath global0 "local-install-root"
            projectRoot' <- getStackPath global0 "project-root"
            let global = global0
                    { gLocalInstallRoot = localInstallRoot'
                    , gProjectRoot = projectRoot' }
            return $ Just $ rules global args
  where
    getStackPath global path = do
      out <- readProcess stackProgName (stackArgs global ++ ["path", "--" ++ path]) ""
      return $ trim $ fromMaybe out $ stripPrefix (path ++ ":") out

-- | Additional command-line options.
options :: [OptDescr (Either String (Global -> Global))]
options =
    [ Option "" [gpgKeyOptName]
        (ReqArg (\v -> Right $ \g -> g{gGpgKey = v}) "USER-ID")
        "GPG user ID to sign distribution package with."
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
        "Extra suffix to add to binary executable archive filename." ]

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
        mapM_ (\f -> need [releaseDir </> f <.> uploadExt]) binaryFileNames

    phony buildPhony $
        mapM_ (\f -> need [releaseDir </> f]) binaryFileNames

    forM_ distros $ \distro -> do

        phony (distroUploadPhony distro) $
            mapM_
                (\v -> need [distroVersionDir (DistroVersion distro (fst v)) </> distroPackageFileName distro <.> uploadExt])
                (distroVersions distro)

        phony (distroPackagesPhony distro) $
            mapM_
                (\v -> need [distroVersionDir (DistroVersion distro (fst v)) </> distroPackageFileName distro])
                (distroVersions distro)

    releaseDir </> "*" <.> uploadExt %> \out -> do
        need [dropExtension out]
        uploadToGithubRelease global (dropExtension out)
        copyFile' (dropExtension out) out

    releaseCheckDir </> binaryExeFileName %> \out -> do
        need [installBinDir </> stackOrigExeFileName]
        Stdout dirty <- cmd "git status --porcelain"
        when (not gAllowDirty && not (null (trim dirty))) $
            error ("Working tree is dirty.  Use --" ++ allowDirtyOptName ++ " option to continue anyway.")
        let instExeFile = installBinDir </> stackOrigExeFileName
            tmpExeFile = installBinDir </> stackOrigExeFileName <.> "tmp"
        --EKB FIXME: once 'stack install --path' implemented, use it instead of this temp file.
        liftIO $ renameFile instExeFile tmpExeFile
        actionFinally
            (do opt <- addPath [installBinDir] []
                () <- cmd opt stackProgName (stackArgs global) "build --pedantic --haddock --no-haddock-deps"
                () <- cmd opt stackProgName (stackArgs global) "clean"
                () <- cmd opt stackProgName (stackArgs global) "build --pedantic"
                () <- cmd opt stackProgName (stackArgs global) "test --pedantic --flag stack:integration-tests"
                return ())
            (renameFile tmpExeFile instExeFile)
        copyFileChanged (installBinDir </> stackOrigExeFileName) out

    releaseDir </> binaryExeZipFileName %> \out -> do
        stageFiles <- getStageFiles
        putNormal $ "zip " ++ out
        liftIO $ do
            entries <- forM stageFiles $ \stageFile -> do
                Zip.readEntry
                    [Zip.OptLocation
                        (dropDirectoryPrefix (releaseDir </> binaryExeStageDirName) stageFile)
                        False]
                    stageFile
            let archive = foldr Zip.addEntryToArchive Zip.emptyArchive entries
            L8.writeFile out (Zip.fromArchive archive)

    releaseDir </> binaryExeTarGzFileName %> \out -> do
        stageFiles <- getStageFiles
        putNormal $ "tar gzip " ++ out
        liftIO $ do
            content <- Tar.pack releaseDir $
                map (dropDirectoryPrefix releaseDir) stageFiles
            L8.writeFile out $ GZip.compress $ Tar.write content

    releaseDir </> binaryExeStageDirName </> binaryExeFileName %> \out -> do
        copyFile' (releaseDir </> binaryExeFileName) out

    releaseDir </> (binaryExeStageDirName ++ "//*") %> \out -> do
        copyFile'
            (dropDirectoryPrefix (releaseDir </> binaryExeStageDirName) out)
            out

    releaseDir </> binaryExeFileName %> \out -> do
        need [installBinDir </> stackOrigExeFileName]
        case platformOS of
            Windows ->
                -- Windows doesn't have or need a 'strip' command, so skip it.
                liftIO $ copyFile (installBinDir </> stackOrigExeFileName) out
            Linux ->
                cmd "strip -p --strip-unneeded --remove-section=.comment -o"
                    [out, installBinDir </> stackOrigExeFileName]
            _ ->
                cmd "strip -o"
                    [out, installBinDir </> stackOrigExeFileName]

    releaseDir </> binaryExeCompressedAscFileName %> \out -> do
        need [out -<.> ""]
        _ <- liftIO $ tryJust (guard . isDoesNotExistError) (removeFile out)
        cmd "gpg --detach-sig --armor"
            [ "-u", gGpgKey
            , dropExtension out ]

    installBinDir </> stackOrigExeFileName %> \_ -> do
        alwaysRerun
        cmd stackProgName (stackArgs global) "build --pedantic"

    forM_ distros $ \distro0 -> do

        distroVersionDir (anyDistroVersion' distro0) </> distroPackageFileName distro0 <.> uploadExt %> \out -> do
            let dv@DistroVersion{..} = distroVersionFromPath out
            need [dropExtension out]
            uploadPackage dvDistro dv (dropExtension out)
            copyFileChanged (dropExtension out) out

        distroVersionDir (anyDistroVersion' distro0) </> distroPackageFileName distro0 %> \out -> do
            alwaysRerun
            let dv@DistroVersion{..} = distroVersionFromPath out
            need [distroVersionDir dv </> imageIDFileName]
            liftIO $ createDirectoryIfMissing True (takeDirectory out)
            cmd "docker run --rm"
                [ "--volume=" ++ gProjectRoot </> distroVersionDir dv </> "stack-root" ++
                ":/mnt/stack-root"
                , "--env=STACK_ROOT=/mnt/stack-root"
                , "--volume=" ++ gProjectRoot </> distroVersionDir dv </> "stack-work" ++
                ":/mnt/src/.stack-work"
                , "--volume=" ++ gProjectRoot ++ ":/mnt/src"
                , "--workdir=/mnt/src"
                , "--volume=" ++ gProjectRoot </> distroVersionDir dv ++ ":/mnt/out"
                , "--env=OUTPUT_PKG=/mnt/out/" ++ distroPackageFileName dvDistro
                , "--env=PKG_VERSION=" ++ distroPackageVersionStr dvDistro
                , "--env=PKG_MAINTAINER=" ++ maintainer gStackPackageDescription
                , "--env=PKG_DESCRIPTION=" ++ synopsis gStackPackageDescription
                , "--env=PKG_LICENSE=" ++ display (license gStackPackageDescription)
                , "--env=PKG_URL=" ++ homepage gStackPackageDescription
                , distroVersionDockerImageTag dv ]

    distroVersionDir anyDistroVersion </> imageIDFileName %> \out -> do
        alwaysRerun
        let dv@DistroVersion{..} = distroVersionFromPath out
            imageTag = distroVersionDockerImageTag dv
        need
            [ distroVersionDockerDir dv </> "Dockerfile"
            , distroVersionDockerDir dv </> "run.sh" ]
        _ <- buildDockerImage (distroVersionDockerDir dv) imageTag out
        return ()

    distroVersionDockerDir anyDistroVersion </> "Dockerfile" %> \out -> do
        let DistroVersion{..} = distroVersionFromPath out
        template <- readTemplate (distroTemplateDir dvDistro </> "docker/Dockerfile")
        writeFileChanged out $
            replace "<<DISTRO-VERSION>>" dvVersion $
            replace "<<DISTRO>>" dvDistro template

    distroVersionDockerDir anyDistroVersion </> "run.sh" %> \out -> do
        let DistroVersion{..} = distroVersionFromPath out
        writeFileChanged out =<< readTemplate (distroTemplateDir dvDistro </> "docker/run.sh")

  where

    getStageFiles = do
        docs <- getDirectoryFiles rootDir
            ["LICENSE", "*.md", "doc//*"]
        let stageFiles = concat
                [[releaseDir </> binaryExeStageDirName </> binaryExeFileName]
                ,map ((releaseDir </> binaryExeStageDirName) </>) docs]
        need stageFiles
        return stageFiles

    distroVersionFromPath path =
        case stripPrefix (releaseDir ++ "/") path of
            Nothing -> error ("Cannot determine Ubuntu version from path: " ++ path)
            Just path' -> DistroVersion (takeDirectory1 path') (takeDirectory1 (dropDirectory1 path'))
    readTemplate path =
        readFile' (takeDirectory gScriptPath </> "templates" </> path)

    releasePhony = "release"
    checkPhony = "check"
    uploadPhony = "upload"
    cleanPhony = "clean"
    buildPhony = "build"
    distroPackagesPhony distro = distro ++ "-packages"
    distroUploadPhony distro = distro ++ "-upload"

    releaseCheckDir = releaseDir </> "check"
    installBinDir = gLocalInstallRoot </> "bin"
    distroVersionDockerDir dv = distroVersionDir dv </> "docker"
    distroVersionDir DistroVersion{..} = releaseDir </> dvDistro </> dvVersion

    binaryFileNames = [binaryExeCompressedFileName, binaryExeCompressedAscFileName]
    binaryExeCompressedAscFileName = binaryExeCompressedFileName <.> ascExt
    binaryExeCompressedFileName =
        case platformOS of
            Windows -> binaryExeZipFileName
            _ -> binaryExeTarGzFileName
    binaryExeZipFileName = binaryName global <.> zipExt
    binaryExeTarGzFileName = binaryName global <.> tarGzExt
    binaryExeStageDirName = binaryName global
    binaryExeFileName = stackOrigExeFileName
    stackOrigExeFileName = stackProgName <.> exe
    distroPackageFileName distro
        | distroPackageExt distro == debExt =
            concat [stackProgName, "_", distroPackageVersionStr distro, "_amd64"] <.> debExt
        | distroPackageExt distro == rpmExt =
            concat [stackProgName, "-", distroPackageVersionStr distro] <.> "x86_64" <.> rpmExt
        | distro == archDistro =
            concat [stackProgName, "_", distroPackageVersionStr distro, "-", "x86_64"] <.> tarGzExt
        | otherwise = error ("distroPackageFileName: unknown distro: " ++ distro)
    imageIDFileName = "image-id"

    zipExt = "zip"
    tarGzExt = tarExt <.> gzExt
    gzExt = "gz"
    tarExt = "tar"
    ascExt = "asc"
    uploadExt = "upload"
    debExt = "deb"
    rpmExt = "rpm"

    distroVersionDockerImageTag DistroVersion{..} =
        "stack_release_tool/" ++ dvDistro ++ ":" ++ dvVersion
    distroPackageVersionStr distro
        | distroPackageExt distro == debExt =
            concat [stackVersionStr global, "-", show gGitRevCount, "-", gGitSha]
        | distroPackageExt distro == rpmExt =
            concat [stackVersionStr global, "_", show gGitRevCount, "_", gGitSha]
        | distro == archDistro =
            stackVersionStr global
        | otherwise = error ("distroPackageVersionStr: unknown distro: " ++ distro)

    distroTemplateDir distro
        | distroPackageExt distro `elem` [debExt, rpmExt] = distroPackageExt distro
        | distro == archDistro = "arch"
        | otherwise = error ("distroTemplateDir: unknown distro: " ++ distro)

    distroPackageExt distro
        | distro `elem` [ubuntuDistro, debianDistro] = debExt
        | distro `elem` [centosDistro, fedoraDistro] = rpmExt
        | distro == archDistro = tarGzExt
        | otherwise = error ("distroPackageExt: unknown distro: " ++ distro)

    distroVersions distro
        | distro == ubuntuDistro =
            [ ("12.04", "precise")
            , ("14.04", "trusty")
            , ("14.10", "utopic")
            , ("15.04", "vivid") ]
        | distro == debianDistro =
            [ ("7", "wheezy")
            , ("8", "jessie") ]
        | distro == centosDistro =
            [ ("7", "7")
            , ("6", "6") ]
        | distro == fedoraDistro =
            [ ("21", "21")
            , ("22", "22") ]
        | distro == archDistro =
            [ ("current", "current") ]
        | otherwise = error ("distroVersions: unknown distro: " ++ distro)

    distroVersionCodeName DistroVersion{..} =
        fromMaybe
            ("distroVersionCodeName: unknown " ++ dvDistro ++ " version: " ++ dvVersion)
            (lookup dvVersion (distroVersions dvDistro))

    distros =
        [ ubuntuDistro
        , debianDistro
        , centosDistro
        , fedoraDistro
        , archDistro ]
    ubuntuDistro = "ubuntu"
    debianDistro = "debian"
    centosDistro = "centos"
    fedoraDistro = "fedora"
    archDistro = "arch"

    anyDistroVersion = DistroVersion "*" "*"
    anyDistroVersion' distro = DistroVersion distro "*"

    uploadPackage :: String -> DistroVersion -> FilePath -> Action ()
    uploadPackage distro dv@DistroVersion{..} pkgFile
        | distroPackageExt distro == debExt =
            cmd "deb-s3 upload -b download.fpcomplete.com --preserve-versions"
                [ "--sign=" ++ gGpgKey
                , "--prefix=" ++ dvDistro ++ "/" ++ distroVersionCodeName dv
                , pkgFile ]
        | distroPackageExt distro == rpmExt = do
            let rpmmacrosFile = gHomeDir </> ".rpmmacros"
            rpmmacrosExists <- liftIO $ System.Directory.doesFileExist rpmmacrosFile
            when rpmmacrosExists $
                error ("'" ++ rpmmacrosFile ++ "' already exists.  Move it out of the way first.")
            actionFinally
                (do writeFileLines rpmmacrosFile
                        [ "%_signature gpg"
                        , "%_gpg_name " ++ gGpgKey ]
                    cmd "rpm-s3 --verbose --sign --bucket=download.fpcomplete.com"
                        [ "--repopath=" ++ dvDistro ++ "/" ++ dvVersion
                        , pkgFile ])
                (liftIO $ removeFile rpmmacrosFile)
        | distro == archDistro = do
            () <- cmd "aws s3 cp"
                [ pkgFile
                , "s3://download.fpcomplete.com/archlinux/" ++ takeFileName pkgFile ]
            putNormal "WARNING: Arch package uploaded, but applying the AUR patch is a manual step."
        | otherwise = error ("uploadPackage: unknown distro: " ++ distro)


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
    manager <- newManager tlsManagerSettings
    runResourceT $ do
        res <- http req manager
        responseBody res $$+- CC.sinkLazy

-- | Drops a directory prefix from a path.  The prefix automatically has a path
-- separator character appended.  Fails if the path does not begin with the prefix.
dropDirectoryPrefix :: FilePath -> FilePath -> FilePath
dropDirectoryPrefix prefix path =
    case stripPrefix (prefix ++ "/") path of
        Nothing -> error ("dropDirectoryPrefix: cannot drop " ++ show prefix ++ " from " ++ show path)
        Just stripped -> stripped

-- | Build a Docker image and write its ID to a file if changed.
buildDockerImage :: FilePath -> String -> FilePath -> Action String
buildDockerImage buildDir imageTag out = do
    alwaysRerun
    () <- cmd "docker build" ["--tag=" ++ imageTag, buildDir]
    (Stdout imageIdOut) <- cmd "docker inspect --format={{.Id}}" [imageTag]
    writeFileChanged out imageIdOut
    return (trim imageIdOut)

-- | Name of the release binary (e.g. @stack-x.y.x-arch-os@)
binaryName :: Global -> String
binaryName global@Global{..} =
    concat
        [ stackProgName
        , "-"
        , stackVersionStr global
        , "-"
        , platformName global
        , if null gBinarySuffix then "" else "-" ++ gBinarySuffix ]

-- | String representation of stack package version.
stackVersionStr :: Global -> String
stackVersionStr =
    display . pkgVersion . package . gStackPackageDescription

-- | Name of current platform.
platformName :: Global -> String
platformName Global{..} =
    display (Platform gArch platformOS)

-- | Current operating system.
platformOS :: OS
platformOS =
    let Platform _ os = buildPlatform
    in os

-- | Directory in which to store build and intermediate files.
releaseDir :: FilePath
releaseDir = "_release"

-- | Root directory of the project
rootDir :: FilePath
rootDir = "."

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

-- | @--arch@ command-line option name.
archOptName :: String
archOptName = "arch"

-- | @--binary-variant@ command-line option name.
binaryVariantOptName :: String
binaryVariantOptName = "binary-variant"

-- | Arguments to pass to all 'stack' invocations.
stackArgs :: Global -> [String]
stackArgs Global{..} = ["--arch=" ++ display gArch]

-- | Name of the 'stack' program.
stackProgName :: FilePath
stackProgName = "stack"

-- | Linux distribution/version combination.
data DistroVersion = DistroVersion
    { dvDistro :: !String
    , dvVersion :: !String }

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
    , gGpgKey :: !String
    , gAllowDirty :: !Bool
    , gGithubAuthToken :: !(Maybe String)
    , gGithubReleaseTag :: !(Maybe String)
    , gGitRevCount :: !Int
    , gGitSha :: !String
    , gProjectRoot :: !FilePath
    , gHomeDir :: !FilePath
    , gScriptPath :: !FilePath
    , gArch :: !Arch
    , gBinarySuffix :: !String }
    deriving (Show)
