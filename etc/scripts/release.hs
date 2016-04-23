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
            let gGpgKey = "0x575159689BEFB442"
                gAllowDirty = False
                gGithubReleaseTag = Nothing
                Platform arch _ = buildPlatform
                gArch = arch
                gBinarySuffix = ""
                gUploadLabel = Nothing
                gTestHaddocks = True
                gProjectRoot = "" -- Set to real value velow.
                gBuildArgs = []
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
        "Extra suffix to add to binary executable archive filename."
    , Option "" [uploadLabelOptName]
        (ReqArg (\v -> Right $ \g -> g{gUploadLabel = Just v}) "LABEL")
        "Label to give the uploaded release asset"
    , Option "" [noTestHaddocksOptName] (NoArg $ Right $ \g -> g{gTestHaddocks = False})
        "Disable testing building haddocks."
    , Option "" [buildArgsOptName]
        (ReqArg
            (\v -> Right $ \g -> g{gBuildArgs = words v})
            "\"ARG1 ARG2 ...\"")
        "Additional arguments to pass to 'stack build'."
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
        mapM_ (\f -> need [releaseDir </> f <.> uploadExt]) binaryPkgFileNames

    phony buildPhony $
        mapM_ (\f -> need [releaseDir </> f]) binaryPkgFileNames

    distroPhonies ubuntuDistro ubuntuVersions debPackageFileName
    distroPhonies debianDistro debianVersions debPackageFileName
    distroPhonies centosDistro centosVersions rpmPackageFileName
    distroPhonies fedoraDistro fedoraVersions rpmPackageFileName

    releaseDir </> "*" <.> uploadExt %> \out -> do
        let srcFile = dropExtension out
            mUploadLabel =
                if takeExtension srcFile == ascExt
                    then fmap (++ " (GPG signature)") gUploadLabel
                    else gUploadLabel
        uploadToGithubRelease global srcFile mUploadLabel
        copyFileChanged srcFile out

    releaseCheckDir </> binaryExeFileName %> \out -> do
        need [releaseBinDir </> binaryName </> stackExeFileName]
        Stdout dirty <- cmd "git status --porcelain"
        when (not gAllowDirty && not (null (trim dirty))) $
            error ("Working tree is dirty.  Use --" ++ allowDirtyOptName ++ " option to continue anyway.")
        withTempDir $ \tmpDir -> do
            let cmd0 = cmd (releaseBinDir </> binaryName </> stackExeFileName)
                    (stackArgs global)
                    gBuildArgs
                    ["--local-bin-path=" ++ tmpDir]
            () <- cmd0 $ concat $ concat
                [["install --pedantic --no-haddock-deps"], [" --haddock" | gTestHaddocks]]
            () <- cmd0 "install --resolver=lts-4.0 cabal-install"
            let cmd' = cmd (AddPath [tmpDir] []) stackProgName (stackArgs global) gBuildArgs
            () <- cmd' "test --pedantic --flag stack:integration-tests"
            return ()
        copyFileChanged (releaseBinDir </> binaryName </> stackExeFileName) out

    releaseDir </> binaryPkgZipFileName %> \out -> do
        stageFiles <- getBinaryPkgStageFiles
        putNormal $ "zip " ++ out
        liftIO $ do
            entries <- forM stageFiles $ \stageFile -> do
                Zip.readEntry
                    [Zip.OptLocation
                        (dropDirectoryPrefix (releaseStageDir </> binaryName) stageFile)
                        False]
                    stageFile
            let archive = foldr Zip.addEntryToArchive Zip.emptyArchive entries
            L8.writeFile out (Zip.fromArchive archive)

    releaseDir </> binaryPkgTarGzFileName %> \out -> do
        stageFiles <- getBinaryPkgStageFiles
        writeTarGz out releaseStageDir stageFiles

    releaseStageDir </> binaryName </> stackExeFileName %> \out -> do
        copyFileChanged (releaseDir </> binaryExeFileName) out

    releaseStageDir </> (binaryName ++ "//*") %> \out -> do
        copyFileChanged
            (dropDirectoryPrefix (releaseStageDir </> binaryName) out)
            out

    releaseDir </> binaryExeFileName %> \out -> do
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
                actionOnException
                    (command_ [] "c:\\Program Files\\Microsoft SDKs\\Windows\\v7.1\\Bin\\signtool.exe"
                        ["sign"
                        ,"/v"
                        ,"/d", synopsis gStackPackageDescription
                        ,"/du", homepage gStackPackageDescription
                        ,"/n", "FP Complete, Corporation"
                        ,"/t", "http://timestamp.verisign.com/scripts/timestamp.dll"
                        ,out])
                    (removeFile out)
            Linux ->
                cmd "strip -p --strip-unneeded --remove-section=.comment -o"
                    [out, releaseBinDir </> binaryName </> stackExeFileName]
            _ ->
                cmd "strip -o"
                    [out, releaseBinDir </> binaryName </> stackExeFileName]

    releaseDir </> binaryPkgSignatureFileName %> \out -> do
        need [out -<.> ""]
        _ <- liftIO $ tryJust (guard . isDoesNotExistError) (removeFile out)
        cmd ("gpg " ++ gpgOptions ++ " --detach-sig --armor")
            [ "-u", gGpgKey
            , dropExtension out ]

    releaseBinDir </> binaryName </> stackExeFileName %> \out -> do
        alwaysRerun
        actionOnException
            (cmd stackProgName
                (stackArgs global)
                gBuildArgs
                ["--local-bin-path=" ++ takeDirectory out]
                 "install --pedantic")
            (removeFile out)

    debDistroRules ubuntuDistro ubuntuVersions
    debDistroRules debianDistro debianVersions
    rpmDistroRules centosDistro centosVersions
    rpmDistroRules fedoraDistro fedoraVersions

  where

    debDistroRules debDistro0 debVersions = do
        let anyVersion0 = anyDistroVersion debDistro0
        distroVersionDir anyVersion0 </> debPackageFileName anyVersion0 <.> uploadExt %> \out -> do
           let DistroVersion{..} = distroVersionFromPath out debVersions
               pkgFile = dropExtension out
           need [pkgFile]
           () <- cmd "deb-s3 upload --preserve-versions --bucket download.fpcomplete.com"
               [ "--sign=" ++ gGpgKey
               , "--gpg-options=" ++ replace "-" "\\-" gpgOptions
               , "--prefix=" ++ dvDistro
               , "--codename=" ++ dvCodeName
               , pkgFile ]
           -- Also upload to the old, incorrect location for people who still have their systems
           -- configured with it.
           () <- cmd "deb-s3 upload --preserve-versions --bucket download.fpcomplete.com"
               [ "--sign=" ++ gGpgKey
               , "--gpg-options=" ++ replace "-" "\\-" gpgOptions
               , "--prefix=" ++ dvDistro ++ "/" ++ dvCodeName
               , pkgFile ]
           copyFileChanged pkgFile out
        distroVersionDir anyVersion0 </> debPackageFileName anyVersion0 %> \out -> do
            docFiles <- getDocFiles
            let dv@DistroVersion{..} = distroVersionFromPath out debVersions
                inputFiles = concat
                    [[debStagedExeFile dv
                     ,debStagedBashCompletionFile dv]
                    ,map (debStagedDocDir dv </>) docFiles]
            need inputFiles
            cmd "fpm -f -s dir -t deb"
                "--deb-recommends git --deb-recommends gnupg"
                "-d g++ -d gcc -d libc6-dev -d libffi-dev -d libgmp-dev -d make -d xz-utils -d zlib1g-dev"
                ["-n", stackProgName
                ,"-C", debStagingDir dv
                ,"-v", debPackageVersionStr dv
                ,"-p", out
                ,"-m", maintainer gStackPackageDescription
                ,"--description", synopsis gStackPackageDescription
                ,"--license", display (license gStackPackageDescription)
                ,"--url", homepage gStackPackageDescription]
                (map (dropDirectoryPrefix (debStagingDir dv)) inputFiles)
        debStagedExeFile anyVersion0 %> \out -> do
            copyFileChanged (releaseDir </> binaryExeFileName) out
        debStagedBashCompletionFile anyVersion0 %> \out -> do
            let dv = distroVersionFromPath out debVersions
            writeBashCompletion (debStagedExeFile dv) out
        debStagedDocDir anyVersion0 ++ "//*" %> \out -> do
            let dv@DistroVersion{..} = distroVersionFromPath out debVersions
                origFile = dropDirectoryPrefix (debStagedDocDir dv) out
            copyFileChanged origFile out

    rpmDistroRules rpmDistro0 rpmVersions = do
        let anyVersion0 = anyDistroVersion rpmDistro0
        distroVersionDir anyVersion0 </> rpmPackageFileName anyVersion0 <.> uploadExt %> \out -> do
           let DistroVersion{..} = distroVersionFromPath out rpmVersions
               pkgFile = dropExtension out
           need [pkgFile]
           let rpmmacrosFile = gHomeDir </> ".rpmmacros"
           rpmmacrosExists <- liftIO $ System.Directory.doesFileExist rpmmacrosFile
           when rpmmacrosExists $
               error ("'" ++ rpmmacrosFile ++ "' already exists.  Move it out of the way first.")
           actionFinally
               (do writeFileLines rpmmacrosFile
                       [ "%_signature gpg"
                       , "%_gpg_name " ++ gGpgKey ]
                   () <- cmd "rpm-s3 --verbose --sign --bucket=download.fpcomplete.com"
                       [ "--repopath=" ++ dvDistro ++ "/" ++ dvVersion
                       , pkgFile ]
                   return ())
               (liftIO $ removeFile rpmmacrosFile)
           copyFileChanged pkgFile out
        distroVersionDir anyVersion0 </> rpmPackageFileName anyVersion0 %> \out -> do
            docFiles <- getDocFiles
            let dv@DistroVersion{..} = distroVersionFromPath out rpmVersions
                inputFiles = concat
                    [[rpmStagedExeFile dv
                     ,rpmStagedBashCompletionFile dv]
                    ,map (rpmStagedDocDir dv </>) docFiles]
            need inputFiles
            cmd "fpm -s dir -t rpm"
                "-d perl -d make -d automake -d gcc -d gmp-devel -d libffi -d zlib -d xz -d tar"
                ["-n", stackProgName
                ,"-C", rpmStagingDir dv
                ,"-v", rpmPackageVersionStr dv
                ,"--iteration", rpmPackageIterationStr dv
                ,"-p", out
                ,"-m", maintainer gStackPackageDescription
                ,"--description", synopsis gStackPackageDescription
                ,"--license", display (license gStackPackageDescription)
                ,"--url", homepage gStackPackageDescription]
                (map (dropDirectoryPrefix (rpmStagingDir dv)) inputFiles)
        rpmStagedExeFile anyVersion0 %> \out -> do
            copyFileChanged (releaseDir </> binaryExeFileName) out
        rpmStagedBashCompletionFile anyVersion0 %> \out -> do
            let dv = distroVersionFromPath out rpmVersions
            writeBashCompletion (rpmStagedExeFile dv) out
        rpmStagedDocDir anyVersion0 ++ "//*" %> \out -> do
            let dv@DistroVersion{..} = distroVersionFromPath out rpmVersions
                origFile = dropDirectoryPrefix (rpmStagedDocDir dv) out
            copyFileChanged origFile out

    writeBashCompletion stagedStackExeFile out = do
        need [stagedStackExeFile]
        (Stdout bashCompletionScript) <- cmd [stagedStackExeFile] "--bash-completion-script" [stackProgName]
        writeFileChanged out bashCompletionScript

    getBinaryPkgStageFiles = do
        docFiles <- getDocFiles
        let stageFiles = concat
                [[releaseStageDir </> binaryName </> stackExeFileName]
                ,map ((releaseStageDir </> binaryName) </>) docFiles]
        need stageFiles
        return stageFiles

    getDocFiles = getDirectoryFiles "." ["LICENSE", "*.md", "doc//*.md"]

    distroVersionFromPath path versions =
        let path' = dropDirectoryPrefix releaseDir path
            version = takeDirectory1 (dropDirectory1 path')
        in DistroVersion (takeDirectory1 path') version (lookupVersionCodeName version versions)

    distroPhonies distro0 versions0 makePackageFileName =
        forM_ versions0 $ \(version0,_) -> do
            let dv@DistroVersion{..} = DistroVersion distro0 version0 (lookupVersionCodeName version0 versions0)
            phony (distroUploadPhony dv) $ need [distroVersionDir dv </> makePackageFileName dv <.> uploadExt]
            phony (distroBuildPhony dv) $ need [distroVersionDir dv </> makePackageFileName dv]

    lookupVersionCodeName version versions =
        fromMaybe (error $ "lookupVersionCodeName: could not find " ++ show version ++ " in " ++ show versions) $
            lookup version versions


    releasePhony = "release"
    checkPhony = "check"
    uploadPhony = "upload"
    cleanPhony = "clean"
    buildPhony = "build"
    distroUploadPhony DistroVersion{..} = "upload-" ++ dvDistro ++ "-" ++ dvVersion
    distroBuildPhony DistroVersion{..} = "build-" ++ dvDistro ++ "-" ++ dvVersion

    releaseCheckDir = releaseDir </> "check"
    releaseStageDir = releaseDir </> "stage"
    releaseBinDir = releaseDir </> "bin"
    distroVersionDir DistroVersion{..} = releaseDir </> dvDistro </> dvVersion

    binaryPkgFileNames = [binaryPkgFileName, binaryPkgSignatureFileName]
    binaryPkgSignatureFileName = binaryPkgFileName <.> ascExt
    binaryPkgFileName =
        case platformOS of
            Windows -> binaryPkgZipFileName
            _ -> binaryPkgTarGzFileName
    binaryPkgZipFileName = binaryName <.> zipExt
    binaryPkgTarGzFileName = binaryName <.> tarGzExt
    binaryExeFileName = binaryName <.> exe
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

    debStagedDocDir dv = debStagingDir dv </> "usr/share/doc" </> stackProgName
    debStagedBashCompletionFile dv = debStagingDir dv </> "etc/bash_completion.d/stack"
    debStagedExeFile dv = debStagingDir dv </> "usr/bin/stack"
    debStagingDir dv = distroVersionDir dv </> debPackageName dv
    debPackageFileName dv = debPackageName dv <.> debExt
    debPackageName dv = stackProgName ++ "_" ++ debPackageVersionStr dv ++ "_amd64"
    debPackageVersionStr DistroVersion{..} = stackVersionStr global ++ "-0~" ++ dvCodeName

    rpmStagedDocDir dv = rpmStagingDir dv </> "usr/share/doc" </> (stackProgName ++ "-" ++ rpmPackageVersionStr dv)
    rpmStagedBashCompletionFile dv = rpmStagingDir dv </> "etc/bash_completion.d/stack"
    rpmStagedExeFile dv = rpmStagingDir dv </> "usr/bin/stack"
    rpmStagingDir dv = distroVersionDir dv </> rpmPackageName dv
    rpmPackageFileName dv = rpmPackageName dv <.> rpmExt
    rpmPackageName dv = stackProgName ++ "-" ++ rpmPackageVersionStr dv ++ "-" ++ rpmPackageIterationStr dv ++ ".x86_64"
    rpmPackageIterationStr DistroVersion{..} = "0." ++ dvCodeName
    rpmPackageVersionStr _ = stackVersionStr global

    ubuntuVersions =
        [ ("12.04", "precise")
        , ("14.04", "trusty")
        , ("14.10", "utopic")
        , ("15.04", "vivid")
        , ("15.10", "wily")
        , ("16.04", "xenial") ]
    debianVersions =
        [ ("7", "wheezy")
        , ("8", "jessie") ]
    centosVersions =
        [ ("7", "el7")
        , ("6", "el6") ]
    fedoraVersions =
        [ ("21", "fc21")
        , ("22", "fc22")
        , ("23", "fc23") ]

    ubuntuDistro = "ubuntu"
    debianDistro = "debian"
    centosDistro = "centos"
    fedoraDistro = "fedora"

    anyDistroVersion distro = DistroVersion distro "*" "*"

    zipExt = ".zip"
    tarGzExt = tarExt <.> gzExt
    gzExt = ".gz"
    tarExt = ".tar"
    ascExt = ".asc"
    uploadExt = ".upload"
    debExt = ".deb"
    rpmExt = ".rpm"


-- | Upload file to Github release.
uploadToGithubRelease :: Global -> FilePath -> Maybe String -> Action ()
uploadToGithubRelease global@Global{..} file mUploadLabel = do
    need [file]
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

-- | @--upload-label@ command-line option name.
uploadLabelOptName :: String
uploadLabelOptName = "upload-label"

-- | @--no-test-haddocks@ command-line option name.
noTestHaddocksOptName :: String
noTestHaddocksOptName = "no-test-haddocks"

buildArgsOptName :: String
buildArgsOptName = "build-args"

-- | Arguments to pass to all 'stack' invocations.
stackArgs :: Global -> [String]
stackArgs Global{..} = ["--install-ghc", "--arch=" ++ display gArch]

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
    , gGpgKey :: !String
    , gAllowDirty :: !Bool
    , gGithubAuthToken :: !(Maybe String)
    , gGithubReleaseTag :: !(Maybe String)
    , gGitRevCount :: !Int
    , gGitSha :: !String
    , gProjectRoot :: !FilePath
    , gHomeDir :: !FilePath
    , gArch :: !Arch
    , gBinarySuffix :: !String
    , gUploadLabel :: (Maybe String)
    , gTestHaddocks :: !Bool
    , gBuildArgs :: [String] }
    deriving (Show)
