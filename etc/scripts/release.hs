{- stack script
    --resolver lts-19.7
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
    --extra-dep Cabal-3.4.1.0
    --extra-dep process-1.6.14.0
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List
import Data.Maybe
import Distribution.PackageDescription.Parsec
import Distribution.Text
import Distribution.System
import Distribution.Package
import Distribution.PackageDescription hiding (options)
#if MIN_VERSION_Cabal(3, 0, 0)
import Distribution.Utils.ShortText (fromShortText)
#endif
import Distribution.Verbosity
import System.Console.GetOpt
import System.Directory
import System.IO.Error
import System.Process

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TarEntry
import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.GZip as GZip
import Data.List.Extra
import Development.Shake
import Development.Shake.FilePath
import Prelude -- Silence AMP warning

#if !MIN_VERSION_Cabal(3, 0, 0)
fromShortText :: String -> String
fromShortText = id
#endif

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
            gGitRevCount <- length . lines <$> readProcess "git" ["rev-list", "HEAD"] ""
            gGitSha <- trim <$> readProcess "git" ["rev-parse", "HEAD"] ""
            gHomeDir <- getHomeDirectory

            let gAllowDirty = False
                Platform arch _ = buildPlatform
                gArch = arch
                gBinarySuffix = ""
                gTestHaddocks = True
                gProjectRoot = "" -- Set to real value velow.
                gBuildArgs = ["--flag", "stack:-developer-mode"]
                gStaticLinux = False
                gCertificateName = Nothing
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
    [ Option "" [allowDirtyOptName] (NoArg $ Right $ \g -> g{gAllowDirty = True})
        "Allow a dirty working tree for release."
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
    , Option "" [noTestHaddocksOptName] (NoArg $ Right $ \g -> g{gTestHaddocks = False})
        "Disable testing building haddocks."
    , Option "" [alpineOptName]
        (NoArg $ Right $ \g ->
          g{gBuildArgs =
              gBuildArgs g ++
              ["--flag=stack:static", "--docker", "--system-ghc", "--no-install-ghc"],
            gStaticLinux = True})
        "Build a static binary using Alpine Docker image."
    , Option "" [buildArgsOptName]
        (ReqArg
            (\v -> Right $ \g -> g{gBuildArgs = gBuildArgs g ++ words v})
            "\"ARG1 ARG2 ...\"")
        "Additional arguments to pass to 'stack build'."
    , Option "" [certificateNameOptName]
        (ReqArg (\v -> Right $ \g -> g{gCertificateName = Just v}) "NAME")
        "Certificate name for code signing on Windows"
    ]

-- | Shake rules.
rules :: Global -> [String] -> Rules ()
rules global@Global{..} args = do
    case args of
        [] -> error "No wanted target(s) specified."
        _ -> want args

    phony releasePhony $ do
        need [checkPhony]
        need [buildPhony]

    phony cleanPhony $
        removeFilesAfter releaseDir ["//*"]

    phony checkPhony $
        need [releaseCheckDir </> binaryExeFileName]

    phony buildPhony $
        mapM_ (\f -> need [releaseDir </> f]) binaryPkgFileNames

    releaseCheckDir </> binaryExeFileName %> \out -> do
        need [releaseBinDir </> binaryName </> stackExeFileName]
        Stdout dirty <- cmd "git status --porcelain"
        when (not gAllowDirty && not (null (trim dirty))) $
            error $ concat
              [ "Working tree is dirty.  Use --"
              , allowDirtyOptName
              , " option to continue anyway. Output:\n"
              , show dirty
              ]
        () <- cmd
            [gProjectRoot </> releaseBinDir </> binaryName </> stackExeFileName]
            (stackArgs global)
            ["build"]
            gBuildArgs
            integrationTestFlagArgs
            ["--pedantic", "--no-haddock-deps", "--test"]
            ["--haddock" | gTestHaddocks]
            ["stack"]
        () <- cmd
            [gProjectRoot </> releaseBinDir </> binaryName </> stackExeFileName]
            ["exec"]
            [gProjectRoot </> releaseBinDir </> binaryName </> "stack-integration-test"]
        copyFileChanged (releaseBinDir </> binaryName </> stackExeFileName) out

    releaseDir </> binaryPkgZipFileName %> \out -> do
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

    releaseDir </> binaryPkgTarGzFileName %> \out -> do
        stageFiles <- getBinaryPkgStageFiles
        writeTarGz id out releaseStageDir stageFiles

    releaseDir </> binaryPkgStaticTarGzFileName %> \out -> do
        stageFiles <- getBinaryPkgStageFiles
        let fixPath path =
                let (x, y) = break (== '/') path
                 in concat [x, "-static", y]
        writeTarGz fixPath out releaseStageDir stageFiles

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
                case gCertificateName of
                    Nothing -> return ()
                    Just certName ->
                        actionOnException
                            (command_ [] "c:\\Program Files\\Microsoft SDKs\\Windows\\v7.1\\Bin\\signtool.exe"
                                ["sign"
                                ,"/v"
                                ,"/d", fromShortText $ synopsis gStackPackageDescription
                                ,"/du", fromShortText $ homepage gStackPackageDescription
                                ,"/n", certName
                                ,"/t", "http://timestamp.verisign.com/scripts/timestamp.dll"
                                ,out])
                            (removeFile out)
            Linux ->
                -- Using Ubuntu's strip to strip an Alpine exe doesn't work, so just copy
                liftIO $ copyFile (releaseBinDir </> binaryName </> stackExeFileName) out
            _ ->
                cmd "strip -o"
                    [out, releaseBinDir </> binaryName </> stackExeFileName]

    releaseDir </> binaryInstallerFileName %> \_ -> do
        need [releaseDir </> binaryExeFileName]
        need [releaseDir </> binaryInstallerNSIFileName]

        command_ [Cwd releaseDir] "c:\\Program Files (x86)\\NSIS\\Unicode\\makensis.exe"
            [ "-V3"
            , binaryInstallerNSIFileName]

    releaseDir </> binaryInstallerNSIFileName %> \out -> do
        need ["etc" </> "scripts" </> "build-stack-installer" <.> "hs"]
        cmd "stack etc/scripts/build-stack-installer.hs"
            [ binaryExeFileName
            , binaryInstallerFileName
            , out
            , stackVersionStr global
            ] :: Action ()

    releaseBinDir </> binaryName </> stackExeFileName %> \out -> do
        alwaysRerun
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
    cleanPhony = "clean"
    buildPhony = "build"

    releaseCheckDir = releaseDir </> "check"
    releaseStageDir = releaseDir </> "stage"
    releaseBinDir = releaseDir </> "bin"

    binaryPkgFileNames =
        case platformOS of
            Windows -> [binaryExeFileName, binaryPkgZipFileName, binaryPkgTarGzFileName, binaryInstallerFileName]
            Linux -> concat
              [ [binaryExeFileName, binaryPkgTarGzFileName]
              , [binaryPkgStaticTarGzFileName | gStaticLinux]
              ]
            _ -> [binaryExeFileName, binaryPkgTarGzFileName]
    binaryPkgZipFileName = binaryName <.> zipExt
    binaryPkgTarGzFileName = binaryName <.> tarGzExt
    binaryPkgStaticTarGzFileName = binaryStaticName <.> tarGzExt
    -- Adding '-bin' to name to work around https://github.com/commercialhaskell/stack/issues/4961
    binaryExeFileName = binaryName ++ "-bin" <.> exe
    -- Prefix with 'installer-' so it doesn't get included in release artifacts
    -- (due to NSIS limitation, needs to be in same directory as executable)
    binaryInstallerNSIFileName = "installer-" ++ binaryName <.> nsiExt
    binaryInstallerFileName = binaryName ++ "-installer" <.> exe
    mkBinaryName isStatic =
        concat
            [ stackProgName
            , "-"
            , stackVersionStr global
            , "-"
            , display platformOS
            , "-"
            , display gArch
            , if isStatic then "-static" else ""
            , if null gBinarySuffix then "" else "-" ++ gBinarySuffix ]
    binaryName = mkBinaryName False
    binaryStaticName = mkBinaryName True
    stackExeFileName = stackProgName <.> exe

    zipExt = ".zip"
    tarGzExt = tarExt <.> gzExt
    gzExt = ".gz"
    tarExt = ".tar"
    nsiExt = ".nsi"

-- | Create a .tar.gz files from files.  The paths should be absolute, and will
-- be made relative to the base directory in the tarball.
writeTarGz :: (FilePath -> FilePath) -> FilePath -> FilePath -> [FilePath] -> Action ()
writeTarGz fixPath out baseDir inputFiles = liftIO $ do
    content <- Tar.pack baseDir $ map (dropDirectoryPrefix baseDir) inputFiles
    L8.writeFile out $ GZip.compress $ Tar.write $ map fixPath' content
  where
    fixPath' :: Tar.Entry -> Tar.Entry
    fixPath' entry =
        case TarEntry.toTarPath isDir $ fixPath $ TarEntry.entryPath entry of
            Left e -> error $ show (Tar.entryPath entry, e)
            Right tarPath -> entry { TarEntry.entryTarPath = tarPath }
      where
        isDir =
            case TarEntry.entryContent entry of
                TarEntry.Directory -> True
                _ -> False

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

-- | @--allow-dirty@ command-line option name.
allowDirtyOptName :: String
allowDirtyOptName = "allow-dirty"

-- | @--arch@ command-line option name.
archOptName :: String
archOptName = "arch"

-- | @--binary-variant@ command-line option name.
binaryVariantOptName :: String
binaryVariantOptName = "binary-variant"

-- | @--no-test-haddocks@ command-line option name.
noTestHaddocksOptName :: String
noTestHaddocksOptName = "no-test-haddocks"

-- | @--build-args@ command-line option name.
buildArgsOptName :: String
buildArgsOptName = "build-args"

-- | @--alpine@ command-line option name.
alpineOptName :: String
alpineOptName = "alpine"

-- | @--certificate-name@ command-line option name.
certificateNameOptName :: String
certificateNameOptName = "certificate-name"

-- | Arguments to pass to all 'stack' invocations.
stackArgs :: Global -> [String]
stackArgs Global{..} = ["--arch=" ++ display gArch, "--interleaved-output"]

-- | Name of the 'stack' program.
stackProgName :: FilePath
stackProgName = "stack"

-- | Linux distribution/version combination.
data DistroVersion = DistroVersion
    { dvDistro :: !String
    , dvVersion :: !String
    , dvCodeName :: !String }

-- | Global values and options.
data Global = Global
    { gStackPackageDescription :: !PackageDescription
    , gAllowDirty :: !Bool
    , gGitRevCount :: !Int
    , gGitSha :: !String
    , gProjectRoot :: !FilePath
    , gHomeDir :: !FilePath
    , gArch :: !Arch
    , gBinarySuffix :: !String
    , gTestHaddocks :: !Bool
    , gBuildArgs :: [String]
    , gStaticLinux :: !Bool
    , gCertificateName :: !(Maybe String)
    }
    deriving (Show)
