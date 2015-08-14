{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}

module Stack.Setup
  ( setupEnv
  , ensureGHC
  , SetupOpts (..)
  ) where

import           Control.Applicative
import           Control.Exception.Enclosed (catchIO)
import           Control.Monad (liftM, when, join, void, unless)
import           Control.Monad.Catch
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import           Control.Monad.State (get, put, modify)
import           Control.Monad.Trans.Control
import           Crypto.Hash (SHA1(SHA1))
import           Data.Aeson.Extended
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Conduit (Conduit, ($$), (=$), await, yield, awaitForever)
import           Data.Conduit.Lift (evalStateC)
import qualified Data.Conduit.List as CL
import           Data.Either
import           Data.Foldable
import           Data.IORef
import           Data.List hiding (concat, elem, maximumBy)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Ord (comparing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import           Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import           Data.Typeable (Typeable)
import qualified Data.Yaml as Yaml
import           Distribution.System (OS (..), Arch (..), Platform (..))
import           Distribution.Text (simpleParse)
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Download.Verified
import           Path
import           Path.IO
import           Prelude hiding (concat, elem) -- Fix AMP warning
import           Safe (headMay, readMay)
import           Stack.Types.Build
import           Stack.Config (resolvePackageEntry)
import           Stack.Constants (distRelativeDir)
import           Stack.Fetch
import           Stack.GhcPkg (createDatabase, getCabalPkgVer, getGlobalDB)
import           Stack.Solver (getGhcVersion)
import           Stack.Types
import           Stack.Types.StackT
import           System.Environment (getExecutablePath)
import           System.Exit (ExitCode (ExitSuccess))
import           System.FilePath (searchPathSeparator)
import qualified System.FilePath as FP
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (rawSystem)
import           System.Process.Read
import           System.Process.Run (runIn)
import           Text.Printf (printf)

data SetupOpts = SetupOpts
    { soptsInstallIfMissing :: !Bool
    , soptsUseSystem :: !Bool
    , soptsWantedCompiler :: !CompilerVersion
    , soptsCompilerCheck :: !VersionCheck
    , soptsStackYaml :: !(Maybe (Path Abs File))
    -- ^ If we got the desired GHC version from that file
    , soptsForceReinstall :: !Bool
    , soptsSanityCheck :: !Bool
    -- ^ Run a sanity check on the selected GHC
    , soptsSkipGhcCheck :: !Bool
    -- ^ Don't check for a compatible GHC version/architecture
    , soptsSkipMsys :: !Bool
    -- ^ Do not use a custom msys installation on Windows
    , soptsUpgradeCabal :: !Bool
    -- ^ Upgrade the global Cabal library in the database to the newest
    -- version. Only works reliably with a stack-managed installation.
    , soptsResolveMissingGHC :: !(Maybe Text)
    -- ^ Message shown to user for how to resolve the missing GHC
    }
    deriving Show
data SetupException = UnsupportedSetupCombo OS Arch
                    | MissingDependencies [String]
                    | UnknownGHCVersion Text CompilerVersion (Set Version)
                    | UnknownOSKey Text
                    | GHCSanityCheckCompileFailed ReadProcessException (Path Abs File)
    deriving Typeable
instance Exception SetupException
instance Show SetupException where
    show (UnsupportedSetupCombo os arch) = concat
        [ "I don't know how to install GHC for "
        , show (os, arch)
        , ", please install manually"
        ]
    show (MissingDependencies tools) =
        "The following executables are missing and must be installed: " ++
        intercalate ", " tools
    show (UnknownGHCVersion oskey wanted known) = concat
        [ "No information found for GHC version "
        , T.unpack (compilerVersionName wanted)
        , ".\nSupported GHC major versions for OS key '" ++ T.unpack oskey ++ "': "
        , intercalate ", " (map show $ Set.toList known)
        ]
    show (UnknownOSKey oskey) =
        "Unable to find installation URLs for OS key: " ++
        T.unpack oskey
    show (GHCSanityCheckCompileFailed e ghc) = concat
        [ "The GHC located at "
        , toFilePath ghc
        , " failed to compile a sanity check. Please see:\n\n"
        , "    https://github.com/commercialhaskell/stack/wiki/Downloads\n\n"
        , "for more information. Exception was:\n"
        , show e
        ]

-- | Modify the environment variables (like PATH) appropriately, possibly doing installation too
setupEnv :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasBuildConfig env, HasHttpManager env, MonadBaseControl IO m)
         => Maybe Text -- ^ Message to give user when necessary GHC is not available
         -> m EnvConfig
setupEnv mResolveMissingGHC = do
    bconfig <- asks getBuildConfig
    let platform = getPlatform bconfig
        sopts = SetupOpts
            { soptsInstallIfMissing = configInstallGHC $ bcConfig bconfig
            , soptsUseSystem = configSystemGHC $ bcConfig bconfig
            , soptsWantedCompiler = bcWantedCompiler bconfig
            , soptsCompilerCheck = configCompilerCheck $ bcConfig bconfig
            , soptsStackYaml = Just $ bcStackYaml bconfig
            , soptsForceReinstall = False
            , soptsSanityCheck = False
            , soptsSkipGhcCheck = configSkipGHCCheck $ bcConfig bconfig
            , soptsSkipMsys = configSkipMsys $ bcConfig bconfig
            , soptsUpgradeCabal = False
            , soptsResolveMissingGHC = mResolveMissingGHC
            }

    mghcBin <- ensureGHC sopts

    -- Modify the initial environment to include the GHC path, if a local GHC
    -- is being used
    menv0 <- getMinimalEnvOverride
    let env = removeHaskellEnvVars
            $ augmentPathMap (fromMaybe [] mghcBin)
            $ unEnvOverride menv0

    menv <- mkEnvOverride platform env
    ghcVer <- getGhcVersion menv
    cabalVer <- getCabalPkgVer menv
    packages <- mapM
        (resolvePackageEntry menv (bcRoot bconfig))
        (bcPackageEntries bconfig)
    let envConfig0 = EnvConfig
            { envConfigBuildConfig = bconfig
            , envConfigCabalVersion = cabalVer
            , envConfigCompilerVersion = GhcVersion ghcVer
            , envConfigPackages = Map.fromList $ concat packages
            }

    -- extra installation bin directories
    mkDirs <- runReaderT extraBinDirs envConfig0
    let mpath = Map.lookup "PATH" env
        mkDirs' = map toFilePath . mkDirs
        depsPath = augmentPath (mkDirs' False) mpath
        localsPath = augmentPath (mkDirs' True) mpath

    deps <- runReaderT packageDatabaseDeps envConfig0
    createDatabase menv deps
    localdb <- runReaderT packageDatabaseLocal envConfig0
    createDatabase menv localdb
    globalDB <- getGlobalDB menv
    let mkGPP locals = T.pack $ intercalate [searchPathSeparator] $ concat
            [ [toFilePathNoTrailingSlash localdb | locals]
            , [toFilePathNoTrailingSlash deps]
            , [toFilePathNoTrailingSlash globalDB]
            ]

    distDir <- runReaderT distRelativeDir envConfig0

    executablePath <- liftIO getExecutablePath

    envRef <- liftIO $ newIORef Map.empty
    let getEnvOverride' es = do
            m <- readIORef envRef
            case Map.lookup es m of
                Just eo -> return eo
                Nothing -> do
                    eo <- mkEnvOverride platform
                        $ Map.insert "PATH" (if esIncludeLocals es then localsPath else depsPath)
                        $ (if esIncludeGhcPackagePath es
                                then Map.insert "GHC_PACKAGE_PATH" (mkGPP (esIncludeLocals es))
                                else id)

                        $ (if esStackExe es
                                then Map.insert "STACK_EXE" (T.pack executablePath)
                                else id)

                        -- For reasoning and duplication, see: https://github.com/fpco/stack/issues/70
                        $ Map.insert "HASKELL_PACKAGE_SANDBOX" (T.pack $ toFilePathNoTrailingSlash deps)
                        $ Map.insert "HASKELL_PACKAGE_SANDBOXES"
                            (T.pack $ if esIncludeLocals es
                                then intercalate [searchPathSeparator]
                                        [ toFilePathNoTrailingSlash localdb
                                        , toFilePathNoTrailingSlash deps
                                        , ""
                                        ]
                                else intercalate [searchPathSeparator]
                                        [ toFilePathNoTrailingSlash deps
                                        , ""
                                        ])
                        $ Map.insert "HASKELL_DIST_DIR" (T.pack $ toFilePathNoTrailingSlash distDir)
                        $ env
                    !() <- atomicModifyIORef envRef $ \m' ->
                        (Map.insert es eo m', ())
                    return eo

    return EnvConfig
        { envConfigBuildConfig = bconfig
            { bcConfig = (bcConfig bconfig) { configEnvOverride = getEnvOverride' }
            }
        , envConfigCabalVersion = cabalVer
        , envConfigCompilerVersion = GhcVersion ghcVer
        , envConfigPackages = envConfigPackages envConfig0
        }

-- | Ensure GHC is installed and provide the PATHs to add if necessary
ensureGHC :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m)
          => SetupOpts
          -> m (Maybe [FilePath])
ensureGHC sopts = do
    case soptsWantedCompiler sopts of
        GhcVersion v | v < $(mkVersion "7.8") -> do
            $logWarn "stack will almost certainly fail with GHC below version 7.8"
            $logWarn "Valiantly attempting to run anyway, but I know this is doomed"
            $logWarn "For more information, see: https://github.com/commercialhaskell/stack/issues/648"
            $logWarn ""
        _ -> return ()

    -- Check the available GHCs
    menv0 <- getMinimalEnvOverride

    msystem <-
        if soptsUseSystem sopts
            then getSystemGHC menv0
            else return Nothing

    Platform expectedArch _ <- asks getPlatform

    let needLocal = case msystem of
            Nothing -> True
            Just _ | soptsSkipGhcCheck sopts -> False
            Just (system, arch) ->
                not (isWanted system) ||
                arch /= expectedArch
        isWanted = isWantedCompiler (soptsCompilerCheck sopts) (soptsWantedCompiler sopts) . GhcVersion

    -- If we need to install a GHC, try to do so
    mpaths <- if needLocal
        then do
            -- Avoid having to load it twice
            siRef <- liftIO $ newIORef Nothing
            manager <- asks getHttpManager
            let getSetupInfo' = liftIO $ do
                    msi <- readIORef siRef
                    case msi of
                        Just si -> return si
                        Nothing -> do
                            si <- getSetupInfo manager
                            writeIORef siRef $ Just si
                            return si

            config <- asks getConfig
            installed <- runReaderT listInstalled config

            -- Install GHC
            ghcIdent <- case getInstalledTool installed $(mkPackageName "ghc") isWanted of
                Just ident -> return ident
                Nothing
                    | soptsInstallIfMissing sopts -> do
                        si <- getSetupInfo'
                        downloadAndInstallGHC menv0 si (soptsWantedCompiler sopts) (soptsCompilerCheck sopts)
                    | otherwise -> do
                        Platform arch _ <- asks getPlatform
                        throwM $ GHCVersionMismatch
                            msystem
                            (soptsWantedCompiler sopts, arch)
                            (soptsCompilerCheck sopts)
                            (soptsStackYaml sopts)
                            (fromMaybe
                                "Try running stack setup to locally install the correct GHC"
                                $ soptsResolveMissingGHC sopts)

            -- Install git on windows, if necessary
            mgitIdent <- case configPlatform config of
                Platform _ os | isWindows os && not (soptsSkipMsys sopts) ->
                    case getInstalledTool installed $(mkPackageName "git") (const True) of
                        Just ident -> return (Just ident)
                        Nothing
                            | soptsInstallIfMissing sopts -> do
                                si <- getSetupInfo'
                                let VersionedDownloadInfo version info = siPortableGit si
                                Just <$> downloadAndInstallTool si info $(mkPackageName "git") version installGitWindows
                            | otherwise -> do
                                $logWarn "Continuing despite missing tool: git"
                                return Nothing
                _ -> return Nothing

            let idents = catMaybes [Just ghcIdent, mgitIdent]
            paths <- runReaderT (mapM binDirs idents) config
            return $ Just $ map toFilePathNoTrailingSlash $ concat paths
        else return Nothing

    menv <-
        case mpaths of
            Nothing -> return menv0
            Just paths -> do
                config <- asks getConfig
                let m0 = unEnvOverride menv0
                    path0 = Map.lookup "PATH" m0
                    path = augmentPath paths path0
                    m = Map.insert "PATH" path m0
                mkEnvOverride (configPlatform config) (removeHaskellEnvVars m)

    when (soptsUpgradeCabal sopts) $ do
        unless needLocal $ do
            $logWarn "Trying to upgrade Cabal library on a GHC not installed by stack."
            $logWarn "This may fail, caveat emptor!"

        upgradeCabal menv

    when (soptsSanityCheck sopts) $ sanityCheck menv

    return mpaths

-- | Install the newest version of Cabal globally
upgradeCabal :: (MonadIO m, MonadLogger m, MonadReader env m, HasHttpManager env, HasConfig env, MonadBaseControl IO m, MonadMask m)
             => EnvOverride
             -> m ()
upgradeCabal menv = do
    let name = $(mkPackageName "Cabal")
    rmap <- resolvePackages menv Set.empty (Set.singleton name)
    newest <-
        case Map.keys rmap of
            [] -> error "No Cabal library found in index, cannot upgrade"
            [PackageIdentifier name' version]
                | name == name' -> return version
            x -> error $ "Unexpected results for resolvePackages: " ++ show x
    installed <- getCabalPkgVer menv
    if installed >= newest
        then $logInfo $ T.concat
            [ "Currently installed Cabal is "
            , T.pack $ versionString installed
            , ", newest is "
            , T.pack $ versionString newest
            , ". I'm not upgrading Cabal."
            ]
        else withSystemTempDirectory "stack-cabal-upgrade" $ \tmpdir -> do
            $logInfo $ T.concat
                [ "Installing Cabal-"
                , T.pack $ versionString newest
                , " to replace "
                , T.pack $ versionString installed
                ]
            tmpdir' <- parseAbsDir tmpdir
            let ident = PackageIdentifier name newest
            m <- unpackPackageIdents menv tmpdir' Nothing (Set.singleton ident)

            ghcPath <- join $ findExecutable menv "ghc"
            newestDir <- parseRelDir $ versionString newest
            let installRoot = toFilePath $ parent (parent ghcPath)
                                       </> $(mkRelDir "new-cabal")
                                       </> newestDir

            dir <-
                case Map.lookup ident m of
                    Nothing -> error $ "upgradeCabal: Invariant violated, dir missing"
                    Just dir -> return dir

            runIn dir "ghc" menv ["Setup.hs"] Nothing
            let setupExe = toFilePath $ dir </> $(mkRelFile "Setup")
                dirArgument name' = concat
                    [ "--"
                    , name'
                    , "dir="
                    , installRoot FP.</> name'
                    ]
            runIn dir setupExe menv
                ( "configure"
                : map dirArgument (words "lib bin data doc")
                )
                Nothing
            runIn dir setupExe menv ["build"] Nothing
            runIn dir setupExe menv ["install"] Nothing
            $logInfo "New Cabal library installed"

-- | Get the major version of the system GHC, if available
getSystemGHC :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m) => EnvOverride -> m (Maybe (Version, Arch))
getSystemGHC menv = do
    exists <- doesExecutableExist menv "ghc"
    if exists
        then do
            eres <- tryProcessStdout Nothing menv "ghc" ["--info"]
            return $ do
                Right bs <- Just eres
                pairs <- readMay $ S8.unpack bs :: Maybe [(String, String)]
                version <- lookup "Project version" pairs >>= parseVersionFromString
                arch <- lookup "Target platform" pairs >>= simpleParse . takeWhile (/= '-')
                Just (version, arch)
        else return Nothing

data DownloadInfo = DownloadInfo
    { downloadInfoUrl :: Text
    , downloadInfoContentLength :: Int
    , downloadInfoSha1 :: Maybe ByteString
    }
    deriving Show

data VersionedDownloadInfo = VersionedDownloadInfo
    { vdiVersion :: Version
    , vdiDownloadInfo :: DownloadInfo
    }
    deriving Show

parseDownloadInfoFromObject :: Yaml.Object -> Yaml.Parser DownloadInfo
parseDownloadInfoFromObject o = do
    url           <- o .: "url"
    contentLength <- o .: "content-length"
    sha1TextMay   <- o .:? "sha1"
    return DownloadInfo
        { downloadInfoUrl = url
        , downloadInfoContentLength = contentLength
        , downloadInfoSha1 = fmap T.encodeUtf8 sha1TextMay
        }

instance FromJSON DownloadInfo where
    parseJSON = withObject "DownloadInfo" parseDownloadInfoFromObject
instance FromJSON VersionedDownloadInfo where
    parseJSON = withObject "VersionedDownloadInfo" $ \o -> do
        version <- o .: "version"
        downloadInfo <- parseDownloadInfoFromObject o
        return VersionedDownloadInfo
            { vdiVersion = version
            , vdiDownloadInfo = downloadInfo
            }

data SetupInfo = SetupInfo
    { siSevenzExe :: DownloadInfo
    , siSevenzDll :: DownloadInfo
    , siPortableGit :: VersionedDownloadInfo
    , siGHCs :: Map Text (Map Version DownloadInfo)
    }
    deriving Show
instance FromJSON SetupInfo where
    parseJSON = withObject "SetupInfo" $ \o -> SetupInfo
        <$> o .: "sevenzexe-info"
        <*> o .: "sevenzdll-info"
        <*> o .: "portable-git"
        <*> o .: "ghc"

-- | Download the most recent SetupInfo
getSetupInfo :: (MonadIO m, MonadThrow m) => Manager -> m SetupInfo
getSetupInfo manager = do
    bss <- liftIO $ flip runReaderT manager
         $ withResponse req $ \res -> responseBody res $$ CL.consume
    let bs = S8.concat bss
    either throwM return $ Yaml.decodeEither' bs
  where
    req = "https://raw.githubusercontent.com/fpco/stackage-content/master/stack/stack-setup-2.yaml"

markInstalled :: (MonadIO m, MonadReader env m, HasConfig env, MonadThrow m)
              => PackageIdentifier -- ^ e.g., ghc-7.8.4, git-2.4.0.1
              -> m ()
markInstalled ident = do
    dir <- asks $ configLocalPrograms . getConfig
    fpRel <- parseRelFile $ packageIdentifierString ident ++ ".installed"
    liftIO $ writeFile (toFilePath $ dir </> fpRel) "installed"

unmarkInstalled :: (MonadIO m, MonadReader env m, HasConfig env, MonadThrow m)
                => PackageIdentifier
                -> m ()
unmarkInstalled ident = do
    dir <- asks $ configLocalPrograms . getConfig
    fpRel <- parseRelFile $ packageIdentifierString ident ++ ".installed"
    removeFileIfExists $ dir </> fpRel

listInstalled :: (MonadIO m, MonadReader env m, HasConfig env, MonadThrow m)
              => m [PackageIdentifier]
listInstalled = do
    dir <- asks $ configLocalPrograms . getConfig
    createTree dir
    (_, files) <- listDirectory dir
    return $ mapMaybe toIdent files
  where
    toIdent fp = do
        x <- T.stripSuffix ".installed" $ T.pack $ toFilePath $ filename fp
        parsePackageIdentifierFromString $ T.unpack x

installDir :: (MonadReader env m, HasConfig env, MonadThrow m, MonadLogger m)
           => PackageIdentifier
           -> m (Path Abs Dir)
installDir ident = do
    config <- asks getConfig
    reldir <- parseRelDir $ packageIdentifierString ident
    return $ configLocalPrograms config </> reldir

-- | Binary directories for the given installed package
binDirs :: (MonadReader env m, HasConfig env, MonadThrow m, MonadLogger m)
        => PackageIdentifier
        -> m [Path Abs Dir]
binDirs ident = do
    config <- asks getConfig
    dir <- installDir ident
    case (configPlatform config, packageNameString $ packageIdentifierName ident) of
        (Platform _ (isWindows -> True), "ghc") -> return
            [ dir </> $(mkRelDir "bin")
            , dir </> $(mkRelDir "mingw") </> $(mkRelDir "bin")
            ]
        (Platform _ (isWindows -> True), "git") -> return
            [ dir </> $(mkRelDir "cmd")
            , dir </> $(mkRelDir "usr") </> $(mkRelDir "bin")
            ]
        (_, "ghc") -> return
            [ dir </> $(mkRelDir "bin")
            ]
        (Platform _ x, tool) -> do
            $logWarn $ "binDirs: unexpected OS/tool combo: " <> T.pack (show (x, tool))
            return []

getInstalledTool :: [PackageIdentifier] -- ^ already installed
                 -> PackageName         -- ^ package to find
                 -> (Version -> Bool)   -- ^ which versions are acceptable
                 -> Maybe PackageIdentifier
getInstalledTool installed name goodVersion =
    if null available
        then Nothing
        else Just $ maximumBy (comparing packageIdentifierVersion) available
  where
    available = filter goodPackage installed
    goodPackage pi' =
        packageIdentifierName pi' == name &&
        goodVersion (packageIdentifierVersion pi')

downloadAndInstallTool :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m)
                       => SetupInfo
                       -> DownloadInfo
                       -> PackageName
                       -> Version
                       -> (SetupInfo -> Path Abs File -> ArchiveType -> Path Abs Dir -> PackageIdentifier -> m ())
                       -> m PackageIdentifier
downloadAndInstallTool si downloadInfo name version installer = do
    let ident = PackageIdentifier name version
    (file, at) <- downloadFromInfo downloadInfo ident
    dir <- installDir ident
    unmarkInstalled ident
    installer si file at dir ident
    markInstalled ident
    return ident

downloadAndInstallGHC :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m)
           => EnvOverride
           -> SetupInfo
           -> CompilerVersion
           -> VersionCheck
           -> m PackageIdentifier
downloadAndInstallGHC menv si wanted versionCheck = do
    osKey <- getOSKey menv
    pairs <-
        case Map.lookup osKey $ siGHCs si of
            Nothing -> throwM $ UnknownOSKey osKey
            Just pairs -> return pairs
    let mpair =
            listToMaybe $
            sortBy (flip (comparing fst)) $
            filter (\(v, _) -> isWantedCompiler versionCheck wanted (GhcVersion v)) (Map.toList pairs)
    (selectedVersion, downloadInfo) <-
        case mpair of
            Just pair -> return pair
            Nothing -> throwM $ UnknownGHCVersion osKey wanted (Map.keysSet pairs)
    platform <- asks $ configPlatform . getConfig
    let installer =
            case platform of
                Platform _ os | isWindows os -> installGHCWindows
                _ -> installGHCPosix
    downloadAndInstallTool si downloadInfo $(mkPackageName "ghc") selectedVersion installer

getOSKey :: (MonadReader env m, MonadThrow m, HasConfig env, MonadLogger m, MonadIO m, MonadCatch m, MonadBaseControl IO m)
         => EnvOverride -> m Text
getOSKey menv = do
    platform <- asks $ configPlatform . getConfig
    case platform of
        Platform I386 Linux -> ("linux32" <>) <$> getLinuxSuffix
        Platform X86_64 Linux -> ("linux64" <>) <$> getLinuxSuffix
        Platform I386 OSX -> return "macosx"
        Platform X86_64 OSX -> return "macosx"
        Platform I386 FreeBSD -> return "freebsd32"
        Platform X86_64 FreeBSD -> return "freebsd64"
        Platform I386 OpenBSD -> return "openbsd32"
        Platform X86_64 OpenBSD -> return "openbsd64"
        Platform I386 Windows -> return "windows32"
        Platform X86_64 Windows -> return "windows64"

        Platform I386 (OtherOS "windowsintegersimple") -> return "windowsintegersimple32"
        Platform X86_64 (OtherOS "windowsintegersimple") -> return "windowsintegersimple64"

        Platform arch os -> throwM $ UnsupportedSetupCombo os arch
  where
    getLinuxSuffix = do
        executablePath <- liftIO getExecutablePath
        elddOut <- tryProcessStdout Nothing menv "ldd" [executablePath]
        return $ case elddOut of
            Left _ -> ""
            Right lddOut -> if hasLineWithFirstWord "libgmp.so.3" lddOut then "-gmp4" else ""
    hasLineWithFirstWord w =
      elem (Just w) . map (headMay . T.words) . T.lines . T.decodeUtf8With T.lenientDecode

downloadFromInfo :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m)
             => DownloadInfo
             -> PackageIdentifier
             -> m (Path Abs File, ArchiveType)
downloadFromInfo downloadInfo ident = do
    config <- asks getConfig
    at <-
        case extension of
            ".tar.xz" -> return TarXz
            ".tar.bz2" -> return TarBz2
            ".7z.exe" -> return SevenZ
            _ -> error $ "Unknown extension: " ++ extension
    relfile <- parseRelFile $ packageIdentifierString ident ++ extension
    let path = configLocalPrograms config </> relfile
    chattyDownload (packageIdentifierText ident) downloadInfo path
    return (path, at)
  where
    url = downloadInfoUrl downloadInfo
    extension =
        loop $ T.unpack url
      where
        loop fp
            | ext `elem` [".tar", ".bz2", ".xz", ".exe", ".7z"] = loop fp' ++ ext
            | otherwise = ""
          where
            (fp', ext) = FP.splitExtension fp

data ArchiveType
    = TarBz2
    | TarXz
    | SevenZ

installGHCPosix :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m)
                => SetupInfo
                -> Path Abs File
                -> ArchiveType
                -> Path Abs Dir
                -> PackageIdentifier
                -> m ()
installGHCPosix _ archiveFile archiveType destDir ident = do
    platform <- asks getPlatform
    menv0 <- getMinimalEnvOverride
    menv <- mkEnvOverride platform (removeHaskellEnvVars (unEnvOverride menv0))
    $logDebug $ "menv = " <> T.pack (show (unEnvOverride menv))
    zipTool' <-
        case archiveType of
            TarXz -> return "xz"
            TarBz2 -> return "bzip2"
            SevenZ -> error "Don't know how to deal with .7z files on non-Windows"
    (zipTool, makeTool, tarTool) <- checkDependencies $ (,,)
        <$> checkDependency zipTool'
        <*> (checkDependency "gmake" <|> checkDependency "make")
        <*> checkDependency "tar"

    $logDebug $ "ziptool: " <> T.pack zipTool
    $logDebug $ "make: " <> T.pack makeTool
    $logDebug $ "tar: " <> T.pack tarTool

    withSystemTempDirectory "stack-setup" $ \root' -> do
        root <- parseAbsDir root'
        dir <- liftM (root Path.</>) $ parseRelDir $ packageIdentifierString ident

        $logSticky $ "Unpacking GHC ..."
        $logDebug $ "Unpacking " <> T.pack (toFilePath archiveFile)
        readInNull root tarTool menv ["xf", toFilePath archiveFile] Nothing

        $logSticky "Configuring GHC ..."
        readInNull dir (toFilePath $ dir Path.</> $(mkRelFile "configure"))
                   menv ["--prefix=" ++ toFilePath destDir] Nothing

        $logSticky "Installing GHC ..."
        readInNull dir makeTool menv ["install"] Nothing

        $logStickyDone $ "Installed GHC."
        $logDebug $ "GHC installed to " <> T.pack (toFilePath destDir)
  where
    -- | Check if given processes appear to be present, throwing an exception if
    -- missing.
    checkDependencies :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env)
                      => CheckDependency a -> m a
    checkDependencies (CheckDependency f) = do
        menv <- getMinimalEnvOverride
        liftIO (f menv) >>= either (throwM . MissingDependencies) return

checkDependency :: String -> CheckDependency String
checkDependency tool = CheckDependency $ \menv -> do
    exists <- doesExecutableExist menv tool
    return $ if exists then Right tool else Left [tool]

newtype CheckDependency a = CheckDependency (EnvOverride -> IO (Either [String] a))
    deriving Functor
instance Applicative CheckDependency where
    pure x = CheckDependency $ \_ -> return (Right x)
    CheckDependency f <*> CheckDependency x = CheckDependency $ \menv -> do
        f' <- f menv
        x' <- x menv
        return $
            case (f', x') of
                (Left e1, Left e2) -> Left $ e1 ++ e2
                (Left e, Right _) -> Left e
                (Right _, Left e) -> Left e
                (Right f'', Right x'') -> Right $ f'' x''
instance Alternative CheckDependency where
    empty = CheckDependency $ \_ -> return $ Left []
    CheckDependency x <|> CheckDependency y = CheckDependency $ \menv -> do
        res1 <- x menv
        case res1 of
            Left _ -> y menv
            Right x' -> return $ Right x'

installGHCWindows :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m)
                  => SetupInfo
                  -> Path Abs File
                  -> ArchiveType
                  -> Path Abs Dir
                  -> PackageIdentifier
                  -> m ()
installGHCWindows si archiveFile archiveType destDir _ = do
    suffix <-
        case archiveType of
            TarXz -> return ".xz"
            TarBz2 -> return ".bz2"
            _ -> error $ "GHC on Windows must be a tarball file"
    tarFile <-
        case T.stripSuffix suffix $ T.pack $ toFilePath archiveFile of
            Nothing -> error $ "Invalid GHC filename: " ++ show archiveFile
            Just x -> parseAbsFile $ T.unpack x

    config <- asks getConfig
    run7z <- setup7z si config

    run7z (parent archiveFile) archiveFile
    run7z (parent archiveFile) tarFile
    removeFile tarFile `catchIO` \e ->
        $logWarn (T.concat
            [ "Exception when removing "
            , T.pack $ toFilePath tarFile
            , ": "
            , T.pack $ show e
            ])

    $logInfo $ "GHC installed to " <> T.pack (toFilePath destDir)

installGitWindows :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m)
                  => SetupInfo
                  -> Path Abs File
                  -> ArchiveType
                  -> Path Abs Dir
                  -> PackageIdentifier
                  -> m ()
installGitWindows si archiveFile archiveType destDir _ = do
    case archiveType of
        SevenZ -> return ()
        _ -> error $ "Git on Windows must be a 7z archive"

    config <- asks getConfig
    run7z <- setup7z si config
    run7z destDir archiveFile

-- | Download 7z as necessary, and get a function for unpacking things.
--
-- Returned function takes an unpack directory and archive.
setup7z :: (MonadReader env m, HasHttpManager env, MonadThrow m, MonadIO m, MonadIO n, MonadLogger m, MonadBaseControl IO m)
        => SetupInfo
        -> Config
        -> m (Path Abs Dir -> Path Abs File -> n ())
setup7z si config = do
    chattyDownload "7z.dll" (siSevenzDll si) dll
    chattyDownload "7z.exe" (siSevenzExe si) exe
    return $ \outdir archive -> liftIO $ do
        ec <- rawSystem (toFilePath exe)
            [ "x"
            , "-o" ++ toFilePath outdir
            , "-y"
            , toFilePath archive
            ]
        when (ec /= ExitSuccess)
            $ error $ "Problem while decompressing " ++ toFilePath archive
  where
    dir = configLocalPrograms config </> $(mkRelDir "7z")
    exe = dir </> $(mkRelFile "7z.exe")
    dll = dir </> $(mkRelFile "7z.dll")

chattyDownload :: (MonadReader env m, HasHttpManager env, MonadIO m, MonadLogger m, MonadThrow m, MonadBaseControl IO m)
               => Text          -- ^ label
               -> DownloadInfo  -- ^ URL, content-length, and sha1
               -> Path Abs File -- ^ destination
               -> m ()
chattyDownload label downloadInfo path = do
    let url = downloadInfoUrl downloadInfo
    req <- parseUrl $ T.unpack url
    $logSticky $ T.concat
      [ "Preparing to download "
      , label
      , " ..."
      ]
    $logDebug $ T.concat
      [ "Downloading from "
      , url
      , " to "
      , T.pack $ toFilePath path
      , " ..."
      ]
    hashChecks <- case downloadInfoSha1 downloadInfo of
        Just sha1ByteString -> do
            let sha1 = CheckHexDigestByteString sha1ByteString
            $logDebug $ T.concat
                [ "Will check against sha1 hash: "
                , T.decodeUtf8With T.lenientDecode sha1ByteString
                ]
            return [HashCheck SHA1 sha1]
        Nothing -> do
            $logWarn $ T.concat
                [ "No sha1 found in metadata,"
                , " download hash won't be checked."
                ]
            return []
    let dReq = DownloadRequest
            { drRequest = req
            , drHashChecks = hashChecks
            , drLengthCheck = Just totalSize
            , drRetryPolicy = drRetryPolicyDefault
            }
    runInBase <- liftBaseWith $ \run -> return (void . run)
    x <- verifiedDownload dReq path (chattyDownloadProgress runInBase)
    if x
        then $logStickyDone ("Downloaded " <> label <> ".")
        else $logStickyDone "Already downloaded."
  where
    totalSize = downloadInfoContentLength downloadInfo
    chattyDownloadProgress runInBase _ = do
        _ <- liftIO $ runInBase $ $logSticky $
          label <> ": download has begun"
        CL.map (Sum . S.length)
          =$ chunksOverTime 1
          =$ go
      where
        go = evalStateC 0 $ awaitForever $ \(Sum size) -> do
            modify (+ size)
            totalSoFar <- get
            liftIO $ runInBase $ $logSticky $ T.pack $
                chattyProgressWithTotal totalSoFar totalSize

        -- Note(DanBurton): Total size is now always known in this file.
        -- However, printing in the case where it isn't known may still be
        -- useful in other parts of the codebase.
        -- So I'm just commenting out the code rather than deleting it.

        --      case mcontentLength of
        --        Nothing -> chattyProgressNoTotal totalSoFar
        --        Just 0 -> chattyProgressNoTotal totalSoFar
        --        Just total -> chattyProgressWithTotal totalSoFar total
        ---- Example: ghc: 42.13 KiB downloaded...
        --chattyProgressNoTotal totalSoFar =
        --    printf ("%s: " <> bytesfmt "%7.2f" totalSoFar <> " downloaded...")
        --           (T.unpack label)

        -- Example: ghc: 50.00 MiB / 100.00 MiB (50.00%) downloaded...
        chattyProgressWithTotal totalSoFar total =
          printf ("%s: " <>
                  bytesfmt "%7.2f" totalSoFar <> " / " <>
                  bytesfmt "%.2f" total <>
                  " (%6.2f%%) downloaded...")
                 (T.unpack label)
                 percentage
          where percentage :: Double
                percentage = (fromIntegral totalSoFar / fromIntegral total * 100)

-- | Given a printf format string for the decimal part and a number of
-- bytes, formats the bytes using an appropiate unit and returns the
-- formatted string.
--
-- >>> bytesfmt "%.2" 512368
-- "500.359375 KiB"
bytesfmt :: Integral a => String -> a -> String
bytesfmt formatter bs = printf (formatter <> " %s")
                               (fromIntegral (signum bs) * dec :: Double)
                               (bytesSuffixes !! i)
  where
    (dec,i) = getSuffix (abs bs)
    getSuffix n = until p (\(x,y) -> (x / 1024, y+1)) (fromIntegral n,0)
      where p (n',numDivs) = n' < 1024 || numDivs == (length bytesSuffixes - 1)
    bytesSuffixes :: [String]
    bytesSuffixes = ["B","KiB","MiB","GiB","TiB","PiB","EiB","ZiB","YiB"]

-- Await eagerly (collect with monoidal append),
-- but space out yields by at least the given amount of time.
-- The final yield may come sooner, and may be a superfluous mempty.
-- Note that Integer and Float literals can be turned into NominalDiffTime
-- (these literals are interpreted as "seconds")
chunksOverTime :: (Monoid a, MonadIO m) => NominalDiffTime -> Conduit a m a
chunksOverTime diff = do
    currentTime <- liftIO getCurrentTime
    evalStateC (currentTime, mempty) go
  where
    -- State is a tuple of:
    -- * the last time a yield happened (or the beginning of the sink)
    -- * the accumulated awaits since the last yield
    go = await >>= \case
      Nothing -> do
        (_, acc) <- get
        yield acc
      Just a -> do
        (lastTime, acc) <- get
        let acc' = acc <> a
        currentTime <- liftIO getCurrentTime
        if diff < diffUTCTime currentTime lastTime
          then put (currentTime, mempty) >> yield acc'
          else put (lastTime,    acc')
        go


-- | Perform a basic sanity check of GHC
sanityCheck :: (MonadIO m, MonadMask m, MonadLogger m, MonadBaseControl IO m)
            => EnvOverride
            -> m ()
sanityCheck menv = withSystemTempDirectory "stack-sanity-check" $ \dir -> do
    dir' <- parseAbsDir dir
    let fp = toFilePath $ dir' </> $(mkRelFile "Main.hs")
    liftIO $ writeFile fp $ unlines
        [ "import Distribution.Simple" -- ensure Cabal library is present
        , "main = putStrLn \"Hello World\""
        ]
    ghc <- join $ findExecutable menv "ghc"
    $logDebug $ "Performing a sanity check on: " <> T.pack (toFilePath ghc)
    eres <- tryProcessStdout (Just dir') menv "ghc"
        [ fp
        , "-no-user-package-db"
        ]
    case eres of
        Left e -> throwM $ GHCSanityCheckCompileFailed e ghc
        Right _ -> return () -- TODO check that the output of running the command is correct

toFilePathNoTrailingSlash :: Path loc Dir -> FilePath
toFilePathNoTrailingSlash = FP.dropTrailingPathSeparator . toFilePath

-- Remove potentially confusing environment variables
removeHaskellEnvVars :: Map Text Text -> Map Text Text
removeHaskellEnvVars =
    Map.delete "GHC_PACKAGE_PATH" .
    Map.delete "HASKELL_PACKAGE_SANDBOX" .
    Map.delete "HASKELL_PACKAGE_SANDBOXES" .
    Map.delete "HASKELL_DIST_DIR"
