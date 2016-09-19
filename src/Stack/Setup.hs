{-# LANGUAGE DeriveDataTypeable #-} -- ghc < 7.10
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}

module Stack.Setup
  ( setupEnv
  , ensureCompiler
  , ensureDockerStackExe
  , getSystemCompiler
  , SetupOpts (..)
  , defaultStackSetupYaml
  , removeHaskellEnvVars
  ) where

import              Control.Applicative
import              Control.Concurrent.Async.Lifted (Concurrently(..))
import              Control.Exception.Enclosed (catchIO, tryAny)
import              Control.Monad (liftM, when, join, void, unless)
import              Control.Monad.Catch
import              Control.Monad.IO.Class (MonadIO, liftIO)
import              Control.Monad.Logger
import              Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import              Control.Monad.State (get, put, modify)
import              Control.Monad.Trans.Control
import "cryptohash" Crypto.Hash (SHA1(SHA1))
import              Data.Aeson.Extended
import qualified    Data.ByteString as S
import qualified    Data.ByteString.Char8 as S8
import qualified    Data.ByteString.Lazy as LBS
import              Data.Char (isSpace)
import              Data.Conduit (Conduit, ($$), (=$), await, yield, awaitForever)
import              Data.Conduit.Lift (evalStateC)
import qualified    Data.Conduit.List as CL
import              Data.Either
import              Data.Foldable hiding (concatMap, or, maximum)
import              Data.IORef
import              Data.IORef.RunOnce (runOnce)
import              Data.List hiding (concat, elem, maximumBy, any)
import              Data.Map (Map)
import qualified    Data.Map as Map
import              Data.Maybe
import              Data.Monoid
import              Data.Ord (comparing)
import              Data.Set (Set)
import qualified    Data.Set as Set
import              Data.String
import              Data.Text (Text)
import qualified    Data.Text as T
import qualified    Data.Text.Encoding as T
import qualified    Data.Text.Encoding.Error as T
import              Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import              Data.Typeable (Typeable)
import qualified    Data.Yaml as Yaml
import              Distribution.System (OS (Linux), Arch (..), Platform (..))
import qualified    Distribution.System as Cabal
import              Distribution.Text (simpleParse)
import              Lens.Micro (set)
import              Network.HTTP.Client.Conduit (HasHttpManager, Manager, getHttpManager, parseUrlThrow,
                                                 responseBody, withResponse)
import              Network.HTTP.Download.Verified
import              Path
import              Path.Extra (toFilePathNoTrailingSep)
import              Path.IO hiding (findExecutable)
import qualified    Paths_stack as Meta
import              Prelude hiding (concat, elem, any) -- Fix AMP warning
import              Safe (headMay, readMay)
import              Stack.Build (build)
import              Stack.Config (resolvePackageEntry, loadConfig)
import              Stack.Constants (distRelativeDir, stackProgName)
import              Stack.Exec (defaultEnvSettings)
import              Stack.Fetch
import              Stack.GhcPkg (createDatabase, getCabalPkgVer, getGlobalDB, mkGhcPackagePath)
import              Stack.PrettyPrint
import              Stack.Setup.Installed
import              Stack.Types.Build
import              Stack.Types.Compiler
import              Stack.Types.Config
import              Stack.Types.Docker
import              Stack.Types.Internal (HasTerminal, HasReExec, HasLogLevel, envConfigBuildOpts, buildOptsInstallExes)
import              Stack.Types.PackageIdentifier
import              Stack.Types.PackageName
import              Stack.Types.StackT
import              Stack.Types.Version
import qualified    System.Directory as D
import              System.Environment (getExecutablePath)
import              System.Exit (ExitCode (..), exitFailure)
import              System.FilePath (searchPathSeparator)
import qualified    System.FilePath as FP
import              System.Process (rawSystem)
import              System.Process.Log (withProcessTimeLog)
import              System.Process.Read
import              System.Process.Run (runCmd, Cmd(..))
import              Text.Printf (printf)

-- | Default location of the stack-setup.yaml file
defaultStackSetupYaml :: String
defaultStackSetupYaml =
    "https://raw.githubusercontent.com/fpco/stackage-content/master/stack/stack-setup-2.yaml"

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
    , soptsStackSetupYaml :: !FilePath
    -- ^ Location of the main stack-setup.yaml file
    , soptsGHCBindistURL :: !(Maybe String)
    -- ^ Alternate GHC binary distribution (requires custom GHCVariant)
    }
    deriving Show
data SetupException = UnsupportedSetupCombo OS Arch
                    | MissingDependencies [String]
                    | UnknownCompilerVersion Text CompilerVersion [CompilerVersion]
                    | UnknownOSKey Text
                    | GHCSanityCheckCompileFailed ReadProcessException (Path Abs File)
                    | WantedMustBeGHC
                    | RequireCustomGHCVariant
                    | ProblemWhileDecompressing (Path Abs File)
                    | SetupInfoMissingSevenz
                    | GHCJSRequiresStandardVariant
                    | GHCJSNotBooted
                    | DockerStackExeNotFound Version Text
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
    show (UnknownCompilerVersion oskey wanted known) = concat
        [ "No information found for "
        , compilerVersionString wanted
        , ".\nSupported versions for OS key '" ++ T.unpack oskey ++ "': "
        , intercalate ", " (map show known)
        ]
    show (UnknownOSKey oskey) =
        "Unable to find installation URLs for OS key: " ++
        T.unpack oskey
    show (GHCSanityCheckCompileFailed e ghc) = concat
        [ "The GHC located at "
        , toFilePath ghc
        , " failed to compile a sanity check. Please see:\n\n"
        , "    http://docs.haskellstack.org/en/stable/install_and_upgrade/\n\n"
        , "for more information. Exception was:\n"
        , show e
        ]
    show WantedMustBeGHC =
        "The wanted compiler must be GHC"
    show RequireCustomGHCVariant =
        "A custom --ghc-variant must be specified to use --ghc-bindist"
    show (ProblemWhileDecompressing archive) =
        "Problem while decompressing " ++ toFilePath archive
    show SetupInfoMissingSevenz =
        "SetupInfo missing Sevenz EXE/DLL"
    show GHCJSRequiresStandardVariant =
        "stack does not yet support using --ghc-variant with GHCJS"
    show GHCJSNotBooted =
        "GHCJS does not yet have its boot packages installed.  Use \"stack setup\" to attempt to run ghcjs-boot."
    show (DockerStackExeNotFound stackVersion osKey) = concat
        [ stackProgName
        , "-"
        , versionString stackVersion
        , " executable not found for "
        , T.unpack osKey
        , "\nUse the '"
        , T.unpack dockerStackExeArgName
        , "' option to specify a location"]

-- | Modify the environment variables (like PATH) appropriately, possibly doing installation too
setupEnv :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasBuildConfig env, HasHttpManager env, HasTerminal env, HasReExec env, HasLogLevel env, HasGHCVariant env, MonadBaseControl IO m)
         => Maybe Text -- ^ Message to give user when necessary GHC is not available
         -> m EnvConfig
setupEnv mResolveMissingGHC = do
    bconfig <- asks getBuildConfig
    let platform = getPlatform bconfig
        wc = whichCompiler (bcWantedCompiler bconfig)
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
            , soptsStackSetupYaml = defaultStackSetupYaml
            , soptsGHCBindistURL = Nothing
            }

    (mghcBin, compilerBuild) <- ensureCompiler sopts

    -- Modify the initial environment to include the GHC path, if a local GHC
    -- is being used
    menv0 <- getMinimalEnvOverride
    env <- removeHaskellEnvVars
             <$> augmentPathMap (maybe [] edBins mghcBin) (unEnvOverride menv0)
    menv <- mkEnvOverride platform env

    (compilerVer, cabalVer, globaldb) <- runConcurrently $ (,,)
        <$> Concurrently (getCompilerVersion menv wc)
        <*> Concurrently (getCabalPkgVer menv wc)
        <*> Concurrently (getGlobalDB menv wc)

    $logDebug "Resolving package entries"
    packages <- mapM
        (resolvePackageEntry menv (bcRoot bconfig))
        (bcPackageEntries bconfig)
    let envConfig0 = EnvConfig
            { envConfigBuildConfig = bconfig
            , envConfigCabalVersion = cabalVer
            , envConfigCompilerVersion = compilerVer
            , envConfigCompilerBuild = compilerBuild
            , envConfigPackages = Map.fromList $ concat packages
            }

    -- extra installation bin directories
    mkDirs <- runReaderT extraBinDirs envConfig0
    let mpath = Map.lookup "PATH" env
    depsPath <- augmentPath (mkDirs False) mpath
    localsPath <- augmentPath (mkDirs True) mpath

    deps <- runReaderT packageDatabaseDeps envConfig0
    createDatabase menv wc deps
    localdb <- runReaderT packageDatabaseLocal envConfig0
    createDatabase menv wc localdb
    extras <- runReaderT packageDatabaseExtra envConfig0
    let mkGPP locals = mkGhcPackagePath locals localdb deps extras globaldb

    distDir <- runReaderT distRelativeDir envConfig0

    executablePath <- liftIO getExecutablePath

    utf8EnvVars <- getUtf8EnvVars menv compilerVer

    envRef <- liftIO $ newIORef Map.empty
    let getEnvOverride' es = do
            m <- readIORef envRef
            case Map.lookup es m of
                Just eo -> return eo
                Nothing -> do
                    eo <- mkEnvOverride platform
                        $ Map.insert "PATH" (if esIncludeLocals es then localsPath else depsPath)
                        $ (if esIncludeGhcPackagePath es
                                then Map.insert
                                       (case wc of { Ghc -> "GHC_PACKAGE_PATH"; Ghcjs -> "GHCJS_PACKAGE_PATH" })
                                       (mkGPP (esIncludeLocals es))
                                else id)

                        $ (if esStackExe es
                                then Map.insert "STACK_EXE" (T.pack executablePath)
                                else id)

                        $ (if esLocaleUtf8 es
                                then Map.union utf8EnvVars
                                else id)

                        $ case (soptsSkipMsys sopts, platform) of
                            (False, Platform Cabal.I386   Cabal.Windows)
                                -> Map.insert "MSYSTEM" "MINGW32"
                            (False, Platform Cabal.X86_64 Cabal.Windows)
                                -> Map.insert "MSYSTEM" "MINGW64"
                            _   -> id

                        -- For reasoning and duplication, see: https://github.com/fpco/stack/issues/70
                        $ Map.insert "HASKELL_PACKAGE_SANDBOX" (T.pack $ toFilePathNoTrailingSep deps)
                        $ Map.insert "HASKELL_PACKAGE_SANDBOXES"
                            (T.pack $ if esIncludeLocals es
                                then intercalate [searchPathSeparator]
                                        [ toFilePathNoTrailingSep localdb
                                        , toFilePathNoTrailingSep deps
                                        , ""
                                        ]
                                else intercalate [searchPathSeparator]
                                        [ toFilePathNoTrailingSep deps
                                        , ""
                                        ])
                        $ Map.insert "HASKELL_DIST_DIR" (T.pack $ toFilePathNoTrailingSep distDir) env

                    () <- atomicModifyIORef envRef $ \m' ->
                        (Map.insert es eo m', ())
                    return eo

    return EnvConfig
        { envConfigBuildConfig = bconfig
            { bcConfig = maybe id addIncludeLib mghcBin
                          (bcConfig bconfig)
                { configEnvOverride = getEnvOverride' }
            }
        , envConfigCabalVersion = cabalVer
        , envConfigCompilerVersion = compilerVer
        , envConfigCompilerBuild = compilerBuild
        , envConfigPackages = envConfigPackages envConfig0
        }

-- | Add the include and lib paths to the given Config
addIncludeLib :: ExtraDirs -> Config -> Config
addIncludeLib (ExtraDirs _bins includes libs) config = config
    { configExtraIncludeDirs = Set.union
        (configExtraIncludeDirs config)
        (Set.fromList includes)
    , configExtraLibDirs = Set.union
        (configExtraLibDirs config)
        (Set.fromList libs)
    }

-- | Ensure compiler (ghc or ghcjs) is installed and provide the PATHs to add if necessary
ensureCompiler :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, HasTerminal env, HasReExec env, HasLogLevel env, HasGHCVariant env, MonadBaseControl IO m)
               => SetupOpts
               -> m (Maybe ExtraDirs, CompilerBuild)
ensureCompiler sopts = do
    let wc = whichCompiler (soptsWantedCompiler sopts)
    when (getGhcVersion (soptsWantedCompiler sopts) < $(mkVersion "7.8")) $ do
        $logWarn "stack will almost certainly fail with GHC below version 7.8"
        $logWarn "Valiantly attempting to run anyway, but I know this is doomed"
        $logWarn "For more information, see: https://github.com/commercialhaskell/stack/issues/648"
        $logWarn ""

    -- Check the available GHCs
    menv0 <- getMinimalEnvOverride

    msystem <-
        if soptsUseSystem sopts
            then do
                $logDebug "Getting system compiler version"
                getSystemCompiler menv0 wc
            else return Nothing

    Platform expectedArch _ <- asks getPlatform

    let needLocal = case msystem of
            Nothing -> True
            Just _ | soptsSkipGhcCheck sopts -> False
            Just (system, arch) ->
                not (isWanted system) ||
                arch /= expectedArch
        isWanted = isWantedCompiler (soptsCompilerCheck sopts) (soptsWantedCompiler sopts)

    getSetupInfo' <- runOnce (getSetupInfo (soptsStackSetupYaml sopts) =<< asks getHttpManager)

    let getMmsys2Tool = do
            platform <- asks getPlatform
            localPrograms <- asks $ configLocalPrograms . getConfig
            installed <- listInstalled localPrograms

            case platform of
                Platform _ Cabal.Windows | not (soptsSkipMsys sopts) ->
                    case getInstalledTool installed $(mkPackageName "msys2") (const True) of
                        Just tool -> return (Just tool)
                        Nothing
                            | soptsInstallIfMissing sopts -> do
                                si <- getSetupInfo'
                                osKey <- getOSKey platform
                                config <- asks getConfig
                                VersionedDownloadInfo version info <-
                                    case Map.lookup osKey $ siMsys2 si of
                                        Just x -> return x
                                        Nothing -> error $ "MSYS2 not found for " ++ T.unpack osKey
                                let tool = Tool (PackageIdentifier $(mkPackageName "msys2") version)
                                Just <$> downloadAndInstallTool (configLocalPrograms config) si info tool (installMsys2Windows osKey)
                            | otherwise -> do
                                $logWarn "Continuing despite missing tool: msys2"
                                return Nothing
                _ -> return Nothing


        -- If we need to install a GHC or MSYS, try to do so
        -- Return the additional directory paths of GHC & MSYS.
    (mtools, compilerBuild) <- if needLocal
        then do

            localPrograms <- asks $ configLocalPrograms . getConfig
            installed <- listInstalled localPrograms

            -- Install GHC
            ghcVariant <- asks getGHCVariant
            config <- asks getConfig
            (installedCompiler, compilerBuild) <-
                    case wc of
                        Ghc -> do
                            ghcBuild <- getGhcBuild menv0
                            ghcPkgName <- parsePackageNameFromString ("ghc" ++ ghcVariantSuffix ghcVariant ++ compilerBuildSuffix ghcBuild)
                            return (getInstalledTool installed ghcPkgName (isWanted . GhcVersion), ghcBuild)
                        Ghcjs -> return (getInstalledGhcjs installed isWanted, CompilerBuildStandard)
            compilerTool <- case installedCompiler of
                Just tool -> return tool
                Nothing
                    | soptsInstallIfMissing sopts -> do
                        si <- getSetupInfo'
                        downloadAndInstallCompiler
                            compilerBuild
                            si
                            (soptsWantedCompiler sopts)
                            (soptsCompilerCheck sopts)
                            (soptsGHCBindistURL sopts)
                    | otherwise ->
                        throwM $ CompilerVersionMismatch
                            msystem
                            (soptsWantedCompiler sopts, expectedArch)
                            ghcVariant
                            compilerBuild
                            (soptsCompilerCheck sopts)
                            (soptsStackYaml sopts)
                            (fromMaybe
                                ("Try running \"stack setup\" to install the correct GHC into "
                                <> T.pack (toFilePath (configLocalPrograms config)))
                                $ soptsResolveMissingGHC sopts)

            -- Install msys2 on windows, if necessary
            mmsys2Tool <- getMmsys2Tool
            return (Just (Just compilerTool, mmsys2Tool), compilerBuild)
        -- Have the right ghc, may still need msys
        else do
            mmsys2Tool <- getMmsys2Tool
            return (Just (Nothing, mmsys2Tool), CompilerBuildStandard)

    mpaths <- case mtools of
        Nothing -> return Nothing
        Just (compilerTool, mmsys2Tool) -> do
            -- Add GHC's and MSYS's paths to the config.
            let idents = catMaybes [compilerTool, mmsys2Tool]
            paths <- mapM extraDirs idents
            return $ Just $ mconcat paths

    menv <-
        case mpaths of
            Nothing -> return menv0
            Just ed -> do
                config <- asks getConfig
                m <- augmentPathMap (edBins ed) (unEnvOverride menv0)
                mkEnvOverride (configPlatform config) (removeHaskellEnvVars m)

    when (soptsUpgradeCabal sopts) $ do
        unless needLocal $ do
            $logWarn "Trying to upgrade Cabal library on a GHC not installed by stack."
            $logWarn "This may fail, caveat emptor!"
        upgradeCabal menv wc

    case mtools of
        Just (Just (ToolGhcjs cv), _) -> ensureGhcjsBooted menv cv (soptsInstallIfMissing sopts)
        _ -> return ()

    when (soptsSanityCheck sopts) $ sanityCheck menv wc

    return (mpaths, compilerBuild)

-- | Determine which GHC build to use dependong on which shared libraries are available
-- on the system.
getGhcBuild
    :: (MonadIO m, MonadBaseControl IO m, MonadCatch m, MonadLogger m, HasConfig env, MonadReader env m)
    => EnvOverride -> m CompilerBuild
getGhcBuild menv = do

    config <- asks getConfig
    case configGHCBuild config of
        Just ghcBuild -> return ghcBuild
        Nothing -> determineGhcBuild
  where
    determineGhcBuild = do
        -- TODO: a more reliable, flexible, and data driven approach would be to actually download small
        -- "test" executables (from setup-info) that link to the same gmp/tinfo versions
        -- that GHC does (i.e. built in same environment as the GHC bindist). The algorithm would go
        -- something like this:
        --
        -- check for previous 'uname -a'/`ldconfig -p` plus compiler version/variant in cache
        -- if cached, then use that as suffix
        -- otherwise:
        --     download setup-info
        --     go through all with right prefix for os/version/variant
        --     first try "standard" (no extra suffix), then the rest
        --         download "compatibility check" exe if not already downloaded
        --         try running it
        --         if successful, then choose that
        --             cache compiler suffix with the uname -a and ldconfig -p output hash plus compiler version
        --
        -- Of course, could also try to make a static GHC bindist instead of all this rigamarole.

        platform <- asks getPlatform
        case platform of
            Platform _ Linux -> do
                -- Some systems don't have ldconfig in the PATH, so make sure to look in /sbin and /usr/sbin as well
                sbinEnv <- modifyEnvOverride menv $
                    Map.insert "PATH" $
                    "/sbin:/usr/sbin" <>
                    (maybe "" (":" <>) $ Map.lookup "PATH" (eoTextMap menv))
                eldconfigOut <- tryProcessStdout Nothing sbinEnv "ldconfig" ["-p"]
                egccErrOut <- tryProcessStderrStdout Nothing menv "gcc" ["-v"]
                let firstWords = case eldconfigOut of
                        Right ldconfigOut -> mapMaybe (headMay . T.words) $
                            T.lines $ T.decodeUtf8With T.lenientDecode ldconfigOut
                        Left _ -> []
                    checkLib lib
                        | libT `elem` firstWords = do
                            $logDebug ("Found shared library " <> libT <> " in 'ldconfig -p' output")
                            return True
                        | otherwise = do
#ifdef WINDOWS
                            -- (mkAbsDir "/usr/lib") fails to compile on Windows, thus the CPP
                            return False
#else
                            -- This is a workaround for the fact that libtinfo.so.6 doesn't appear in
                            -- the 'ldconfig -p' output on Arch even when it exists.
                            -- There doesn't seem to be an easy way to get the true list of directories
                            -- to scan for shared libs, but this works for our particular case.
                            e <- doesFileExist ($(mkAbsDir "/usr/lib") </> lib)
                            if e
                                then $logDebug ("Found shared library " <> libT <> " in /usr/lib")
                                else $logDebug ("Did not find shared library " <> libT)
                            return e
#endif
                      where
                        libT = T.pack (toFilePath lib)
                    noPie = case egccErrOut of
                        Right (gccErr,gccOut) ->
                            "--enable-default-pie" `elem` S8.words (gccOut <> gccErr)
                        Left _ -> False
                hastinfo5 <- checkLib $(mkRelFile "libtinfo.so.5")
                hastinfo6 <- checkLib $(mkRelFile "libtinfo.so.6")
                hasncurses6 <- checkLib $(mkRelFile "libncursesw.so.6")
                hasgmp5 <- checkLib $(mkRelFile "libgmp.so.10")
                hasgmp4 <- checkLib $(mkRelFile "libgmp.so.3")
                let libComponents =
                        if  | hastinfo5 && hasgmp5 -> []
                            | hastinfo6 && hasgmp5 -> ["tinfo6"]
                            | hasncurses6 && hasgmp5 -> ["ncurses6"]
                            | hasgmp4 && hastinfo5 -> ["gmp4"]
                            | otherwise -> []
                    pieComponents =
                        if noPie
                            then ["nopie"]
                            else []
                case concat [libComponents, pieComponents] of
                    [] -> useBuild CompilerBuildStandard
                    components -> useBuild (CompilerBuildSpecialized (intercalate "-" components))
            _ -> useBuild CompilerBuildStandard
    useBuild CompilerBuildStandard = do
        $logDebug "Using standard GHC build"
        return CompilerBuildStandard
    useBuild (CompilerBuildSpecialized s) = do
        $logDebug ("Using " <> T.pack s <> " GHC build")
        return (CompilerBuildSpecialized s)

-- | Ensure Docker container-compatible 'stack' executable is downloaded
ensureDockerStackExe
    :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m)
    => Platform -> m (Path Abs File)
ensureDockerStackExe containerPlatform = do
    config <- asks getConfig
    containerPlatformDir <- runReaderT platformOnlyRelDir (containerPlatform,PlatformVariantNone)
    let programsPath = configLocalProgramsBase config </> containerPlatformDir
        stackVersion = fromCabalVersion Meta.version
        tool = Tool (PackageIdentifier $(mkPackageName "stack") stackVersion)
    stackExePath <- (</> $(mkRelFile "stack")) <$> installDir programsPath tool
    stackExeExists <- doesFileExist stackExePath
    unless stackExeExists $
        do
           $logInfo $ mconcat ["Downloading Docker-compatible ", T.pack stackProgName, " executable"]
           si <- getSetupInfo defaultStackSetupYaml =<< asks getHttpManager
           osKey <- getOSKey containerPlatform
           info <-
               case Map.lookup osKey (siStack si) of
                   Just versions ->
                       case Map.lookup stackVersion versions of
                           Just x -> return x
                           Nothing ->
                               case mapMaybe (matchMinor stackVersion) (Map.keys versions) of
                                   (v:_) ->
                                       case Map.lookup v versions of
                                           Just x -> return x
                                           Nothing ->  throwM (DockerStackExeNotFound v osKey)
                                   [] -> throwM (DockerStackExeNotFound stackVersion osKey)
                   Nothing -> throwM (DockerStackExeNotFound stackVersion osKey)
           _ <-
               downloadAndInstallTool
                   programsPath
                   si
                   info
                   tool
                   installDockerStackExe
           return ()
    return stackExePath
  where
    matchMinor stackVersion v =
        if checkVersion MatchMinor stackVersion v
            then Just v
            else Nothing

-- | Install the newest version of Cabal globally
upgradeCabal :: (MonadIO m, MonadLogger m, MonadReader env m, HasHttpManager env, HasConfig env, MonadBaseControl IO m, MonadMask m)
             => EnvOverride
             -> WhichCompiler
             -> m ()
upgradeCabal menv wc = do
    let name = $(mkPackageName "Cabal")
    rmap <- resolvePackages menv Map.empty (Set.singleton name)
    newest <-
        case Map.keys rmap of
            [] -> error "No Cabal library found in index, cannot upgrade"
            [PackageIdentifier name' version]
                | name == name' -> return version
            x -> error $ "Unexpected results for resolvePackages: " ++ show x
    installed <- getCabalPkgVer menv wc
    if installed >= newest
        then $logInfo $ T.concat
            [ "Currently installed Cabal is "
            , T.pack $ versionString installed
            , ", newest is "
            , T.pack $ versionString newest
            , ". I'm not upgrading Cabal."
            ]
        else withSystemTempDir "stack-cabal-upgrade" $ \tmpdir -> do
            $logInfo $ T.concat
                [ "Installing Cabal-"
                , T.pack $ versionString newest
                , " to replace "
                , T.pack $ versionString installed
                ]
            let ident = PackageIdentifier name newest
            -- Nothing below: use the newest .cabal file revision
            m <- unpackPackageIdents menv tmpdir Nothing (Map.singleton ident Nothing)

            compilerPath <- join $ findExecutable menv (compilerExeName wc)
            newestDir <- parseRelDir $ versionString newest
            let installRoot = toFilePath $ parent (parent compilerPath)
                                       </> $(mkRelDir "new-cabal")
                                       </> newestDir

            dir <-
                case Map.lookup ident m of
                    Nothing -> error "upgradeCabal: Invariant violated, dir missing"
                    Just dir -> return dir

            runCmd (Cmd (Just dir) (compilerExeName wc) menv ["Setup.hs"]) Nothing
            platform <- asks getPlatform
            let setupExe = toFilePath $ dir </>
                  (case platform of
                     Platform _ Cabal.Windows -> $(mkRelFile "Setup.exe")
                     _ -> $(mkRelFile "Setup"))
                dirArgument name' = concat
                    [ "--"
                    , name'
                    , "dir="
                    , installRoot FP.</> name'
                    ]
                args = "configure" : map dirArgument (words "lib bin data doc")
            runCmd (Cmd (Just dir) setupExe menv args) Nothing
            runCmd (Cmd (Just dir) setupExe menv ["build"]) Nothing
            runCmd (Cmd (Just dir) setupExe menv ["install"]) Nothing
            $logInfo "New Cabal library installed"

-- | Get the version of the system compiler, if available
getSystemCompiler :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m) => EnvOverride -> WhichCompiler -> m (Maybe (CompilerVersion, Arch))
getSystemCompiler menv wc = do
    let exeName = case wc of
            Ghc -> "ghc"
            Ghcjs -> "ghcjs"
    exists <- doesExecutableExist menv exeName
    if exists
        then do
            eres <- tryProcessStdout Nothing menv exeName ["--info"]
            let minfo = do
                    Right bs <- Just eres
                    pairs_ <- readMay $ S8.unpack bs :: Maybe [(String, String)]
                    version <- lookup "Project version" pairs_ >>= parseVersionFromString
                    arch <- lookup "Target platform" pairs_ >>= simpleParse . takeWhile (/= '-')
                    return (version, arch)
            case (wc, minfo) of
                (Ghc, Just (version, arch)) -> return (Just (GhcVersion version, arch))
                (Ghcjs, Just (_, arch)) -> do
                    eversion <- tryAny $ getCompilerVersion menv Ghcjs
                    case eversion of
                        Left _ -> return Nothing
                        Right version -> return (Just (version, arch))
                (_, Nothing) -> return Nothing
        else return Nothing

-- | Download the most recent SetupInfo
getSetupInfo
    :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader env m, HasConfig env)
    => String -> Manager -> m SetupInfo
getSetupInfo stackSetupYaml manager = do
    config <- asks getConfig
    setupInfos <-
        mapM
            loadSetupInfo
            (SetupInfoFileOrURL stackSetupYaml :
             configSetupInfoLocations config)
    return (mconcat setupInfos)
  where
    loadSetupInfo (SetupInfoInline si) = return si
    loadSetupInfo (SetupInfoFileOrURL urlOrFile) = do
        bs <-
            case parseUrlThrow urlOrFile of
                Just req -> do
                    bss <-
                        liftIO $
                        flip runReaderT manager $
                        withResponse req $
                        \res ->
                             responseBody res $$ CL.consume
                    return $ S8.concat bss
                Nothing -> liftIO $ S.readFile urlOrFile
        WithJSONWarnings si warnings <- either throwM return (Yaml.decodeEither' bs)
        when (urlOrFile /= defaultStackSetupYaml) $
            logJSONWarnings urlOrFile warnings
        return si

getInstalledTool :: [Tool]            -- ^ already installed
                 -> PackageName       -- ^ package to find
                 -> (Version -> Bool) -- ^ which versions are acceptable
                 -> Maybe Tool
getInstalledTool installed name goodVersion =
    if null available
        then Nothing
        else Just $ Tool $ maximumBy (comparing packageIdentifierVersion) available
  where
    available = mapMaybe goodPackage installed
    goodPackage (Tool pi') =
        if packageIdentifierName pi' == name &&
           goodVersion (packageIdentifierVersion pi')
            then Just pi'
            else Nothing
    goodPackage _ = Nothing

getInstalledGhcjs :: [Tool]
                  -> (CompilerVersion -> Bool)
                  -> Maybe Tool
getInstalledGhcjs installed goodVersion =
    if null available
        then Nothing
        else Just $ ToolGhcjs $ maximum available
  where
    available = mapMaybe goodPackage installed
    goodPackage (ToolGhcjs cv) = if goodVersion cv then Just cv else Nothing
    goodPackage _ = Nothing

downloadAndInstallTool :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasHttpManager env, MonadBaseControl IO m)
                       => Path Abs Dir
                       -> SetupInfo
                       -> DownloadInfo
                       -> Tool
                       -> (SetupInfo -> Path Abs File -> ArchiveType -> Path Abs Dir -> Path Abs Dir -> m ())
                       -> m Tool
downloadAndInstallTool programsDir si downloadInfo tool installer = do
    ensureDir programsDir
    (file, at) <- downloadFromInfo programsDir downloadInfo tool
    dir <- installDir programsDir tool
    tempDir <- tempInstallDir programsDir tool
    ignoringAbsence (removeDirRecur tempDir)
    ensureDir tempDir
    unmarkInstalled programsDir tool
    installer si file at tempDir dir
    markInstalled programsDir tool
    ignoringAbsence (removeDirRecur tempDir)
    return tool

downloadAndInstallCompiler :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasGHCVariant env, HasHttpManager env, HasTerminal env, HasReExec env, HasLogLevel env, MonadBaseControl IO m)
                           => CompilerBuild
                           -> SetupInfo
                           -> CompilerVersion
                           -> VersionCheck
                           -> Maybe String
                           -> m Tool
downloadAndInstallCompiler ghcBuild si wanted@GhcVersion{} versionCheck mbindistURL = do
    ghcVariant <- asks getGHCVariant
    (selectedVersion, downloadInfo) <- case mbindistURL of
        Just bindistURL -> do
            case ghcVariant of
                GHCCustom _ -> return ()
                _ -> throwM RequireCustomGHCVariant
            case wanted of
                GhcVersion version ->
                    return (version, GHCDownloadInfo mempty mempty (DownloadInfo (T.pack bindistURL) Nothing Nothing))
                _ ->
                    throwM WantedMustBeGHC
        _ -> do
            ghcKey <- getGhcKey ghcBuild
            case Map.lookup ghcKey $ siGHCs si of
                Nothing -> throwM $ UnknownOSKey ghcKey
                Just pairs_ -> getWantedCompilerInfo ghcKey versionCheck wanted GhcVersion pairs_
    config <- asks getConfig
    let installer =
            case configPlatform config of
                Platform _ Cabal.Windows -> installGHCWindows selectedVersion
                _ -> installGHCPosix selectedVersion downloadInfo
    $logInfo $
        "Preparing to install GHC" <>
        (case ghcVariant of
            GHCStandard -> ""
            v -> " (" <> T.pack (ghcVariantName v) <> ")") <>
        (case ghcBuild of
            CompilerBuildStandard -> ""
            b -> " (" <> T.pack (compilerBuildName b) <> ")") <>
        " to an isolated location."
    $logInfo "This will not interfere with any system-level installation."
    ghcPkgName <- parsePackageNameFromString ("ghc" ++ ghcVariantSuffix ghcVariant ++ compilerBuildSuffix ghcBuild)
    let tool = Tool $ PackageIdentifier ghcPkgName selectedVersion
    downloadAndInstallTool (configLocalPrograms config) si (gdiDownloadInfo downloadInfo) tool installer
downloadAndInstallCompiler compilerBuild si wanted versionCheck _mbindistUrl = do
    config <- asks getConfig
    ghcVariant <- asks getGHCVariant
    case (ghcVariant, compilerBuild) of
        (GHCStandard, CompilerBuildStandard) -> return ()
        _ -> throwM GHCJSRequiresStandardVariant
    (selectedVersion, downloadInfo) <- case Map.lookup "source" $ siGHCJSs si of
        Nothing -> throwM $ UnknownOSKey "source"
        Just pairs_ -> getWantedCompilerInfo "source" versionCheck wanted id pairs_
    $logInfo "Preparing to install GHCJS to an isolated location."
    $logInfo "This will not interfere with any system-level installation."
    let tool = ToolGhcjs selectedVersion
    downloadAndInstallTool (configLocalPrograms config) si downloadInfo tool installGHCJS

getWantedCompilerInfo :: (Ord k, MonadThrow m)
                      => Text
                      -> VersionCheck
                      -> CompilerVersion
                      -> (k -> CompilerVersion)
                      -> Map k a
                      -> m (k, a)
getWantedCompilerInfo key versionCheck wanted toCV pairs_ =
    case mpair of
        Just pair -> return pair
        Nothing -> throwM $ UnknownCompilerVersion key wanted (map toCV (Map.keys pairs_))
  where
    mpair =
        listToMaybe $
        sortBy (flip (comparing fst)) $
        filter (isWantedCompiler versionCheck wanted . toCV . fst) (Map.toList pairs_)

getGhcKey :: (MonadReader env m, HasPlatform env, HasGHCVariant env, MonadCatch m)
          => CompilerBuild -> m Text
getGhcKey ghcBuild = do
    ghcVariant <- asks getGHCVariant
    platform <- asks getPlatform
    osKey <- getOSKey platform
    return $ osKey <> T.pack (ghcVariantSuffix ghcVariant) <> T.pack (compilerBuildSuffix ghcBuild)

getOSKey :: (MonadThrow m)
         => Platform -> m Text
getOSKey platform =
    case platform of
        Platform I386   Cabal.Linux   -> return "linux32"
        Platform X86_64 Cabal.Linux   -> return "linux64"
        Platform I386   Cabal.OSX     -> return "macosx"
        Platform X86_64 Cabal.OSX     -> return "macosx"
        Platform I386   Cabal.FreeBSD -> return "freebsd32"
        Platform X86_64 Cabal.FreeBSD -> return "freebsd64"
        Platform I386   Cabal.OpenBSD -> return "openbsd32"
        Platform X86_64 Cabal.OpenBSD -> return "openbsd64"
        Platform I386   Cabal.Windows -> return "windows32"
        Platform X86_64 Cabal.Windows -> return "windows64"
        Platform Arm    Cabal.Linux   -> return "linux-armv7"
        Platform arch os -> throwM $ UnsupportedSetupCombo os arch

downloadFromInfo
    :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasHttpManager env, MonadBaseControl IO m)
    => Path Abs Dir -> DownloadInfo -> Tool -> m (Path Abs File, ArchiveType)
downloadFromInfo programsDir downloadInfo tool = do
    at <-
        case extension of
            ".tar.xz" -> return TarXz
            ".tar.bz2" -> return TarBz2
            ".tar.gz" -> return TarGz
            ".7z.exe" -> return SevenZ
            _ -> fail $ "Unknown extension for url: " ++ url
    relfile <- parseRelFile $ toolString tool ++ extension
    path <- case url of
        (parseUrlThrow -> Just _) -> do
            let path = programsDir </> relfile
            ensureDir programsDir
            chattyDownload (T.pack (toolString tool)) downloadInfo path
            return path
        (parseAbsFile -> Just path) -> do
            let DownloadInfo{downloadInfoContentLength=contentLength, downloadInfoSha1=sha1} =
                    downloadInfo
            when (isJust contentLength) $
                $logWarn ("`content-length` in not checked \n" <>
                          "and should not be specified when `url` is a file path")
            when (isJust sha1) $
                $logWarn ("`sha1` is not checked and \n" <>
                          "should not be specified when `url` is a file path")
            return path
        _ ->
            fail $ "`url` must be either an HTTP URL or absolute file path: " ++ url
    return (path, at)
  where
    url = T.unpack $ downloadInfoUrl downloadInfo
    extension = loop url
      where
        loop fp
            | ext `elem` [".tar", ".bz2", ".xz", ".exe", ".7z", ".gz"] = loop fp' ++ ext
            | otherwise = ""
          where
            (fp', ext) = FP.splitExtension fp

data ArchiveType
    = TarBz2
    | TarXz
    | TarGz
    | SevenZ

installGHCPosix :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m, HasTerminal env)
                => Version
                -> GHCDownloadInfo
                -> SetupInfo
                -> Path Abs File
                -> ArchiveType
                -> Path Abs Dir
                -> Path Abs Dir
                -> m ()
installGHCPosix version downloadInfo _ archiveFile archiveType tempDir destDir = do
    platform <- asks getPlatform
    menv0 <- getMinimalEnvOverride
    menv <- mkEnvOverride platform (removeHaskellEnvVars (unEnvOverride menv0))
    $logDebug $ "menv = " <> T.pack (show (unEnvOverride menv))
    (zipTool', compOpt) <-
        case archiveType of
            TarXz -> return ("xz", 'J')
            TarBz2 -> return ("bzip2", 'j')
            TarGz -> return ("gzip", 'z')
            SevenZ -> error "Don't know how to deal with .7z files on non-Windows"
    -- Slight hack: OpenBSD's tar doesn't support xz.
    -- https://github.com/commercialhaskell/stack/issues/2283#issuecomment-237980986
    let tarDep =
          case (platform, archiveType) of
            (Platform _ Cabal.OpenBSD, TarXz) -> checkDependency "gtar"
            _ -> checkDependency "tar"
    (zipTool, makeTool, tarTool) <- checkDependencies $ (,,)
        <$> checkDependency zipTool'
        <*> (checkDependency "gmake" <|> checkDependency "make")
        <*> tarDep

    $logDebug $ "ziptool: " <> T.pack zipTool
    $logDebug $ "make: " <> T.pack makeTool
    $logDebug $ "tar: " <> T.pack tarTool

    dir <-
        liftM (tempDir </>) $
        parseRelDir $
        "ghc-" ++ versionString version

    let runStep step wd env cmd args = do
            menv' <- modifyEnvOverride menv (Map.union env)
            result <- try (readProcessNull (Just wd) menv' cmd args)
            case result of
                Right _ -> return ()
                Left ex -> do
                    $logError (T.pack (show (ex :: ReadProcessException)))
                    $prettyError $
                        hang 2
                          ("Error encountered while" <+> step <+> "GHC with" <> line <>
                           shellMagenta (fromString (unwords (cmd : args))) <> line <>
                           -- TODO: Figure out how to insert \ in the appropriate spots
                           -- hang 2 (shellMagenta (fillSep (fromString cmd : map fromString args))) <> line <>
                           "run in " <> display wd) <> line <> line <>
                        "The following directories may now contain files, but won't be used by stack:" <> line <>
                        "  -" <+> display tempDir <> line <>
                        "  -" <+> display destDir <> line
                    liftIO exitFailure

    $logSticky $ T.concat ["Unpacking GHC into ", T.pack . toFilePath $ tempDir, " ..."]
    $logDebug $ "Unpacking " <> T.pack (toFilePath archiveFile)
    runStep "unpacking" tempDir mempty tarTool [compOpt : "xf", toFilePath archiveFile]

    $logSticky "Configuring GHC ..."
    runStep "configuring" dir
        (gdiConfigureEnv downloadInfo)
        (toFilePath $ dir </> $(mkRelFile "configure"))
        (("--prefix=" ++ toFilePath destDir) : map T.unpack (gdiConfigureOpts downloadInfo))

    $logSticky "Installing GHC ..."
    runStep "installing" dir mempty makeTool ["install"]

    $logStickyDone $ "Installed GHC."
    $logDebug $ "GHC installed to " <> T.pack (toFilePath destDir)

installGHCJS :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, HasTerminal env, HasReExec env, HasLogLevel env, MonadBaseControl IO m)
             => SetupInfo
             -> Path Abs File
             -> ArchiveType
             -> Path Abs Dir
             -> Path Abs Dir
             -> m ()
installGHCJS si archiveFile archiveType _tempDir destDir = do
    platform <- asks getPlatform
    menv0 <- getMinimalEnvOverride
    -- This ensures that locking is disabled for the invocations of
    -- stack below.
    let removeLockVar = Map.delete "STACK_LOCK"
    menv <- mkEnvOverride platform (removeLockVar (removeHaskellEnvVars (unEnvOverride menv0)))
    $logDebug $ "menv = " <> T.pack (show (unEnvOverride menv))

    -- NOTE: this is a bit of a hack - instead of using the temp
    -- directory, leave the unpacked source tarball in the destination
    -- directory. This way, the absolute paths in the wrapper scripts
    -- will point to executables that exist in
    -- src/.stack-work/install/... - see
    -- https://github.com/commercialhaskell/stack/issues/1016
    --
    -- This is also used by 'ensureGhcjsBooted', because it can use the
    -- environment of the stack.yaml which came with ghcjs, in order to
    -- install cabal-install. This lets us also fix the version of
    -- cabal-install used.
    let unpackDir = destDir </> $(mkRelDir "src")
    runUnpack <- case platform of
        Platform _ Cabal.Windows -> return $
            withUnpackedTarball7z "GHCJS" si archiveFile archiveType Nothing unpackDir
        _ -> do
            zipTool' <-
                case archiveType of
                    TarXz -> return "xz"
                    TarBz2 -> return "bzip2"
                    TarGz -> return "gzip"
                    SevenZ -> error "Don't know how to deal with .7z files on non-Windows"
            (zipTool, tarTool) <- checkDependencies $ (,)
                <$> checkDependency zipTool'
                <*> checkDependency "tar"
            $logDebug $ "ziptool: " <> T.pack zipTool
            $logDebug $ "tar: " <> T.pack tarTool
            return $ do
                ignoringAbsence (removeDirRecur destDir)
                ignoringAbsence (removeDirRecur unpackDir)
                readProcessNull (Just destDir) menv tarTool ["xf", toFilePath archiveFile]
                innerDir <- expectSingleUnpackedDir archiveFile destDir
                renameDir innerDir unpackDir

    $logSticky $ T.concat ["Unpacking GHCJS into ", T.pack . toFilePath $ unpackDir, " ..."]
    $logDebug $ "Unpacking " <> T.pack (toFilePath archiveFile)
    runUnpack

    $logSticky "Setting up GHCJS build environment"
    let stackYaml = unpackDir </> $(mkRelFile "stack.yaml")
        destBinDir = destDir </> $(mkRelDir "bin")
    ensureDir destBinDir
    envConfig' <- loadGhcjsEnvConfig stackYaml destBinDir

    -- On windows we need to copy options files out of the install dir.  Argh!
    -- This is done before the build, so that if it fails, things fail
    -- earlier.
    mwindowsInstallDir <- case platform of
        Platform _ Cabal.Windows ->
            liftM Just $ runInnerStackT envConfig' installationRootLocal
        _ -> return Nothing

    $logSticky "Installing GHCJS (this will take a long time) ..."
    runInnerStackT (set (envConfigBuildOpts.buildOptsInstallExes) True envConfig') $
        build (\_ -> return ()) Nothing defaultBuildOptsCLI
    -- Copy over *.options files needed on windows.
    forM_ mwindowsInstallDir $ \dir -> do
        (_, files) <- listDir (dir </> $(mkRelDir "bin"))
        forM_ (filter ((".options" `isSuffixOf`). toFilePath) files) $ \optionsFile -> do
            let dest = destDir </> $(mkRelDir "bin") </> filename optionsFile
            ignoringAbsence (removeFile dest)
            copyFile optionsFile dest
    $logStickyDone "Installed GHCJS."

-- Install the downloaded stack binary distribution
installDockerStackExe
    :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, MonadBaseControl IO m)
    => SetupInfo
    -> Path Abs File
    -> ArchiveType
    -> Path Abs Dir
    -> Path Abs Dir
    -> m ()
installDockerStackExe _ archiveFile _ _tempDir destDir = do
    (_,tarTool) <-
        checkDependencies $
        (,) <$> checkDependency "gzip" <*> checkDependency "tar"
    menv <- getMinimalEnvOverride
    ensureDir destDir
    readProcessNull
        (Just destDir)
        menv
        tarTool
        ["xf", toFilePath archiveFile, "--strip-components", "1"]

ensureGhcjsBooted :: (MonadIO m, MonadBaseControl IO m, MonadLogger m, MonadCatch m, HasConfig env, HasHttpManager env, HasTerminal env, HasReExec env, HasLogLevel env, MonadReader env m)
                  => EnvOverride -> CompilerVersion -> Bool -> m ()
ensureGhcjsBooted menv cv shouldBoot  = do
    eres <- try $ sinkProcessStdout Nothing menv "ghcjs" [] (return ())
    case eres of
        Right () -> return ()
        Left (ReadProcessException _ _ _ err) | "no input files" `S.isInfixOf` LBS.toStrict err ->
            return ()
        Left (ReadProcessException _ _ _ err) | "ghcjs_boot.completed" `S.isInfixOf` LBS.toStrict err ->
            if not shouldBoot then throwM GHCJSNotBooted else do
                config <- asks getConfig
                destDir <- installDir (configLocalPrograms config) (ToolGhcjs cv)
                let stackYaml = destDir </> $(mkRelFile "src/stack.yaml")
                -- TODO: Remove 'actualStackYaml' and just use
                -- 'stackYaml' for a version after 0.1.6. It's for
                -- compatibility with the directories setup used for
                -- most of the life of the development branch between
                -- 0.1.5 and 0.1.6. See
                -- https://github.com/commercialhaskell/stack/issues/749#issuecomment-147382783
                -- This only affects the case where GHCJS has been
                -- installed with an older version and not yet booted.
                stackYamlExists <- doesFileExist stackYaml
                ghcjsVersion <- case cv of
                        GhcjsVersion version _ -> return version
                        _ -> fail "ensureGhcjsBooted invoked on non GhcjsVersion"
                actualStackYaml <- if stackYamlExists then return stackYaml
                    else
                        liftM ((destDir </> $(mkRelDir "src")) </>) $
                        parseRelFile $ "ghcjs-" ++ versionString ghcjsVersion ++ "/stack.yaml"
                actualStackYamlExists <- doesFileExist actualStackYaml
                unless actualStackYamlExists $
                    fail "Couldn't find GHCJS stack.yaml in old or new location."
                bootGhcjs ghcjsVersion actualStackYaml destDir
        Left err -> throwM err

bootGhcjs :: (MonadIO m, MonadBaseControl IO m, MonadLogger m, MonadCatch m, HasHttpManager env, HasTerminal env, HasReExec env, HasLogLevel env, MonadReader env m)
          => Version -> Path Abs File -> Path Abs Dir -> m ()
bootGhcjs ghcjsVersion stackYaml destDir = do
    envConfig <- loadGhcjsEnvConfig stackYaml (destDir </> $(mkRelDir "bin"))
    menv <- liftIO $ configEnvOverride (getConfig envConfig) defaultEnvSettings
    -- Install cabal-install if missing, or if the installed one is old.
    mcabal <- getCabalInstallVersion menv
    shouldInstallCabal <- case mcabal of
        Nothing -> do
            $logInfo "No cabal-install binary found for use with GHCJS."
            return True
        Just v
            | v < $(mkVersion "1.22.4") -> do
                $logInfo $
                    "The cabal-install found on PATH is too old to be used for booting GHCJS (version " <>
                    versionText v <>
                    ")."
                return True
            | v >= $(mkVersion "1.23") -> do
                $logWarn $
                    "The cabal-install found on PATH is a version stack doesn't know about, version " <>
                    versionText v <>
                    ". This may or may not work.\n" <>
                    "See this issue: https://github.com/ghcjs/ghcjs/issues/470"
                return False
            | ghcjsVersion >= $(mkVersion "0.2.0.20160413") && v >= $(mkVersion "1.22.8") -> do
                $logWarn $
                    "The cabal-install found on PATH, version " <>
                    versionText v <>
                    ", is >= 1.22.8.\n" <>
                    "That version has a bug preventing ghcjs < 0.2.0.20160413 from booting.\n" <>
                    "See this issue: https://github.com/ghcjs/ghcjs/issues/470"
                return True
            | otherwise -> return False
    let envSettings = defaultEnvSettings { esIncludeGhcPackagePath = False }
    menv' <- liftIO $ configEnvOverride (getConfig envConfig) envSettings
    when shouldInstallCabal $ do
        $logInfo "Building a local copy of cabal-install from source."
        runInnerStackT envConfig $
            build (\_ -> return ())
                  Nothing
                  defaultBuildOptsCLI { boptsCLITargets = ["cabal-install"] }
        mcabal' <- getCabalInstallVersion menv'
        case mcabal' of
            Nothing ->
                $logError $
                    "Failed to get cabal-install version after installing it.\n" <>
                    "This shouldn't happen, because it gets built to the snapshot bin directory, which should be treated as being on the PATH."
            Just v | v >= $(mkVersion "1.22.8") && v < $(mkVersion "1.23") ->
                $logWarn $
                    "Installed version of cabal-install is in a version range which may not work.\n" <>
                    "See this issue: https://github.com/ghcjs/ghcjs/issues/470\n" <>
                    "This version is specified by the stack.yaml file included in the ghcjs tarball.\n"
            _ -> return ()
    $logSticky "Booting GHCJS (this will take a long time) ..."
    logProcessStderrStdout Nothing "ghcjs-boot" menv' ["--clean"]
    $logStickyDone "GHCJS booted."

loadGhcjsEnvConfig :: (MonadIO m, HasHttpManager r, MonadReader r m, HasTerminal r, HasReExec r, HasLogLevel r)
                     => Path Abs File -> Path b t -> m EnvConfig
loadGhcjsEnvConfig stackYaml binPath = runInnerStackLoggingT $ do
    lc <- loadConfig
        (mempty
            { configMonoidInstallGHC = First (Just True)
            , configMonoidLocalBinPath = First (Just (toFilePath binPath))
            })
        Nothing
        (Just stackYaml)
    bconfig <- lcLoadBuildConfig lc Nothing
    runInnerStackT bconfig $ setupEnv Nothing

getCabalInstallVersion :: (MonadIO m, MonadBaseControl IO m, MonadLogger m, MonadCatch m)
                       => EnvOverride -> m (Maybe Version)
getCabalInstallVersion menv = do
    ebs <- tryProcessStdout Nothing menv "cabal" ["--numeric-version"]
    case ebs of
        Left _ -> return Nothing
        Right bs -> Just <$> parseVersion (T.dropWhileEnd isSpace (T.decodeUtf8 bs))

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
                  => Version
                  -> SetupInfo
                  -> Path Abs File
                  -> ArchiveType
                  -> Path Abs Dir
                  -> Path Abs Dir
                  -> m ()
installGHCWindows version si archiveFile archiveType _tempDir destDir = do
    tarComponent <- parseRelDir $ "ghc-" ++ versionString version
    withUnpackedTarball7z "GHC" si archiveFile archiveType (Just tarComponent) destDir
    $logInfo $ "GHC installed to " <> T.pack (toFilePath destDir)

installMsys2Windows :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m)
                  => Text -- ^ OS Key
                  -> SetupInfo
                  -> Path Abs File
                  -> ArchiveType
                  -> Path Abs Dir
                  -> Path Abs Dir
                  -> m ()
installMsys2Windows osKey si archiveFile archiveType _tempDir destDir = do
    exists <- liftIO $ D.doesDirectoryExist $ toFilePath destDir
    when exists $ liftIO (D.removeDirectoryRecursive $ toFilePath destDir) `catchIO` \e -> do
        $logError $ T.pack $
            "Could not delete existing msys directory: " ++
            toFilePath destDir
        throwM e

    msys <- parseRelDir $ "msys" ++ T.unpack (fromMaybe "32" $ T.stripPrefix "windows" osKey)
    withUnpackedTarball7z "MSYS2" si archiveFile archiveType (Just msys) destDir


    -- I couldn't find this officially documented anywhere, but you need to run
    -- the MSYS shell once in order to initialize some pacman stuff. Once that
    -- run happens, you can just run commands as usual.
    platform <- asks getPlatform
    menv0 <- getMinimalEnvOverride
    newEnv0 <- modifyEnvOverride menv0 $ Map.insert "MSYSTEM" "MSYS"
    newEnv <- augmentPathMap [destDir </> $(mkRelDir "usr") </> $(mkRelDir "bin")]
                             (unEnvOverride newEnv0)
    menv <- mkEnvOverride platform newEnv
    runCmd (Cmd (Just destDir) "sh" menv ["--login", "-c", "true"]) Nothing

    -- No longer installing git, it's unreliable
    -- (https://github.com/commercialhaskell/stack/issues/1046) and the
    -- MSYS2-installed version has bad CRLF defaults.
    --
    -- Install git. We could install other useful things in the future too.
    -- runCmd (Cmd (Just destDir) "pacman" menv ["-Sy", "--noconfirm", "git"]) Nothing

-- | Unpack a compressed tarball using 7zip.  Expects a single directory in
-- the unpacked results, which is renamed to the destination directory.
withUnpackedTarball7z :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m)
                      => String -- ^ Name of tool, used in error messages
                      -> SetupInfo
                      -> Path Abs File -- ^ Path to archive file
                      -> ArchiveType
                      -> Maybe (Path Rel Dir) -- ^ Name of directory expected in archive.  If Nothing, expects a single folder.
                      -> Path Abs Dir -- ^ Destination directory.
                      -> m ()
withUnpackedTarball7z name si archiveFile archiveType msrcDir destDir = do
    suffix <-
        case archiveType of
            TarXz -> return ".xz"
            TarBz2 -> return ".bz2"
            TarGz -> return ".gz"
            _ -> error $ name ++ " must be a tarball file"
    tarFile <-
        case T.stripSuffix suffix $ T.pack $ toFilePath archiveFile of
            Nothing -> error $ "Invalid " ++ name ++ " filename: " ++ show archiveFile
            Just x -> parseAbsFile $ T.unpack x
    run7z <- setup7z si
    let tmpName = toFilePathNoTrailingSep (dirname destDir) ++ "-tmp"
    ensureDir (parent destDir)
    withTempDir (parent destDir) tmpName $ \tmpDir -> do
        ignoringAbsence (removeDirRecur destDir)
        run7z (parent archiveFile) archiveFile
        run7z tmpDir tarFile
        absSrcDir <- case msrcDir of
            Just srcDir -> return $ tmpDir </> srcDir
            Nothing -> expectSingleUnpackedDir archiveFile tmpDir
        removeFile tarFile `catchIO` \e ->
            $logWarn (T.concat
                [ "Exception when removing "
                , T.pack $ toFilePath tarFile
                , ": "
                , T.pack $ show e
                ])
        renameDir absSrcDir destDir

expectSingleUnpackedDir :: (MonadIO m, MonadThrow m) => Path Abs File -> Path Abs Dir -> m (Path Abs Dir)
expectSingleUnpackedDir archiveFile destDir = do
    contents <- listDir destDir
    case contents of
        ([dir], []) -> return dir
        _ -> error $ "Expected a single directory within unpacked " ++ toFilePath archiveFile

-- | Download 7z as necessary, and get a function for unpacking things.
--
-- Returned function takes an unpack directory and archive.
setup7z :: (MonadReader env m, HasHttpManager env, HasConfig env, MonadThrow m, MonadIO m, MonadIO n, MonadLogger m, MonadLogger n, MonadBaseControl IO m)
        => SetupInfo
        -> m (Path Abs Dir -> Path Abs File -> n ())
setup7z si = do
    dir <- asks $ configLocalPrograms . getConfig
    ensureDir dir
    let exe = dir </> $(mkRelFile "7z.exe")
        dll = dir </> $(mkRelFile "7z.dll")
    case (siSevenzDll si, siSevenzExe si) of
        (Just sevenzDll, Just sevenzExe) -> do
            chattyDownload "7z.dll" sevenzDll dll
            chattyDownload "7z.exe" sevenzExe exe
            return $ \outdir archive -> do
                let cmd = toFilePath exe
                    args =
                        [ "x"
                        , "-o" ++ toFilePath outdir
                        , "-y"
                        , toFilePath archive
                        ]
                ec <- $withProcessTimeLog cmd args $
                    liftIO $ rawSystem cmd args
                when (ec /= ExitSuccess)
                    $ liftIO $ throwM (ProblemWhileDecompressing archive)
        _ -> throwM SetupInfoMissingSevenz

chattyDownload :: (MonadReader env m, HasHttpManager env, MonadIO m, MonadLogger m, MonadThrow m, MonadBaseControl IO m)
               => Text          -- ^ label
               -> DownloadInfo  -- ^ URL, content-length, and sha1
               -> Path Abs File -- ^ destination
               -> m ()
chattyDownload label downloadInfo path = do
    let url = downloadInfoUrl downloadInfo
    req <- parseUrlThrow $ T.unpack url
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
            , drLengthCheck = mtotalSize
            , drRetryPolicy = drRetryPolicyDefault
            }
    runInBase <- liftBaseWith $ \run -> return (void . run)
    x <- verifiedDownload dReq path (chattyDownloadProgress runInBase)
    if x
        then $logStickyDone ("Downloaded " <> label <> ".")
        else $logStickyDone "Already downloaded."
  where
    mtotalSize = downloadInfoContentLength downloadInfo
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
                case mtotalSize of
                    Nothing -> chattyProgressNoTotal totalSoFar
                    Just 0 -> chattyProgressNoTotal totalSoFar
                    Just totalSize -> chattyProgressWithTotal totalSoFar totalSize

        -- Example: ghc: 42.13 KiB downloaded...
        chattyProgressNoTotal totalSoFar =
            printf ("%s: " <> bytesfmt "%7.2f" totalSoFar <> " downloaded...")
                   (T.unpack label)

        -- Example: ghc: 50.00 MiB / 100.00 MiB (50.00%) downloaded...
        chattyProgressWithTotal totalSoFar total =
          printf ("%s: " <>
                  bytesfmt "%7.2f" totalSoFar <> " / " <>
                  bytesfmt "%.2f" total <>
                  " (%6.2f%%) downloaded...")
                 (T.unpack label)
                 percentage
          where percentage :: Double
                percentage = fromIntegral totalSoFar / fromIntegral total * 100

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
            -> WhichCompiler
            -> m ()
sanityCheck menv wc = withSystemTempDir "stack-sanity-check" $ \dir -> do
    let fp = toFilePath $ dir </> $(mkRelFile "Main.hs")
    liftIO $ writeFile fp $ unlines
        [ "import Distribution.Simple" -- ensure Cabal library is present
        , "main = putStrLn \"Hello World\""
        ]
    let exeName = compilerExeName wc
    ghc <- join $ findExecutable menv exeName
    $logDebug $ "Performing a sanity check on: " <> T.pack (toFilePath ghc)
    eres <- tryProcessStdout (Just dir) menv exeName
        [ fp
        , "-no-user-package-db"
        ]
    case eres of
        Left e -> throwM $ GHCSanityCheckCompileFailed e ghc
        Right _ -> return () -- TODO check that the output of running the command is correct

-- Remove potentially confusing environment variables
removeHaskellEnvVars :: Map Text Text -> Map Text Text
removeHaskellEnvVars =
    Map.delete "GHCJS_PACKAGE_PATH" .
    Map.delete "GHC_PACKAGE_PATH" .
    Map.delete "HASKELL_PACKAGE_SANDBOX" .
    Map.delete "HASKELL_PACKAGE_SANDBOXES" .
    Map.delete "HASKELL_DIST_DIR" .
    -- https://github.com/commercialhaskell/stack/issues/1460
    Map.delete "DESTDIR"

-- | Get map of environment variables to set to change the GHC's encoding to UTF-8
getUtf8EnvVars
    :: forall m env.
       (MonadReader env m, HasPlatform env, MonadLogger m, MonadCatch m, MonadBaseControl IO m, MonadIO m)
    => EnvOverride -> CompilerVersion -> m (Map Text Text)
getUtf8EnvVars menv compilerVer =
    if getGhcVersion compilerVer >= $(mkVersion "7.10.3")
        -- GHC_CHARENC supported by GHC >=7.10.3
        then return $ Map.singleton "GHC_CHARENC" "UTF-8"
        else legacyLocale
  where
    legacyLocale = do
        Platform _ os <- asks getPlatform
        if os == Cabal.Windows
            then
                 -- On Windows, locale is controlled by the code page, so we don't set any environment
                 -- variables.
                 return
                     Map.empty
            else do
                let checkedVars = map checkVar (Map.toList $ eoTextMap menv)
                    -- List of environment variables that will need to be updated to set UTF-8 (because
                    -- they currently do not specify UTF-8).
                    needChangeVars = concatMap fst checkedVars
                    -- Set of locale-related environment variables that have already have a value.
                    existingVarNames = Set.unions (map snd checkedVars)
                    -- True if a locale is already specified by one of the "global" locale variables.
                    hasAnyExisting =
                        any (`Set.member` existingVarNames) ["LANG", "LANGUAGE", "LC_ALL"]
                if null needChangeVars && hasAnyExisting
                    then
                         -- If no variables need changes and at least one "global" variable is set, no
                         -- changes to environment need to be made.
                         return
                             Map.empty
                    else do
                        -- Get a list of known locales by running @locale -a@.
                        elocales <- tryProcessStdout Nothing menv "locale" ["-a"]
                        let
                            -- Filter the list to only include locales with UTF-8 encoding.
                            utf8Locales =
                                case elocales of
                                    Left _ -> []
                                    Right locales ->
                                        filter
                                            isUtf8Locale
                                            (T.lines $
                                             T.decodeUtf8With
                                                 T.lenientDecode
                                                 locales)
                            mfallback = getFallbackLocale utf8Locales
                        when
                            (isNothing mfallback)
                            ($logWarn
                                 "Warning: unable to set locale to UTF-8 encoding; GHC may fail with 'invalid character'")
                        let
                            -- Get the new values of variables to adjust.
                            changes =
                                Map.unions $
                                map
                                    (adjustedVarValue utf8Locales mfallback)
                                    needChangeVars
                            -- Get the values of variables to add.
                            adds
                              | hasAnyExisting =
                                  -- If we already have a "global" variable, then nothing needs
                                  -- to be added.
                                  Map.empty
                              | otherwise =
                                  -- If we don't already have a "global" variable, then set LANG to the
                                  -- fallback.
                                  case mfallback of
                                      Nothing -> Map.empty
                                      Just fallback ->
                                          Map.singleton "LANG" fallback
                        return (Map.union changes adds)
    -- Determines whether an environment variable is locale-related and, if so, whether it needs to
    -- be adjusted.
    checkVar
        :: (Text, Text) -> ([Text], Set Text)
    checkVar (k,v) =
        if k `elem` ["LANG", "LANGUAGE"] || "LC_" `T.isPrefixOf` k
            then if isUtf8Locale v
                     then ([], Set.singleton k)
                     else ([k], Set.singleton k)
            else ([], Set.empty)
    -- Adjusted value of an existing locale variable.  Looks for valid UTF-8 encodings with
    -- same language /and/ territory, then with same language, and finally the first UTF-8 locale
    -- returned by @locale -a@.
    adjustedVarValue
        :: [Text] -> Maybe Text -> Text -> Map Text Text
    adjustedVarValue utf8Locales mfallback k =
        case Map.lookup k (eoTextMap menv) of
            Nothing -> Map.empty
            Just v ->
                case concatMap
                         (matchingLocales utf8Locales)
                         [ T.takeWhile (/= '.') v <> "."
                         , T.takeWhile (/= '_') v <> "_"] of
                    (v':_) -> Map.singleton k v'
                    [] ->
                        case mfallback of
                            Just fallback -> Map.singleton k fallback
                            Nothing -> Map.empty
    -- Determine the fallback locale, by looking for any UTF-8 locale prefixed with the list in
    -- @fallbackPrefixes@, and if not found, picking the first UTF-8 encoding returned by @locale
    -- -a@.
    getFallbackLocale
        :: [Text] -> Maybe Text
    getFallbackLocale utf8Locales =
        case concatMap (matchingLocales utf8Locales) fallbackPrefixes of
            (v:_) -> Just v
            [] ->
                case utf8Locales of
                    [] -> Nothing
                    (v:_) -> Just v
    -- Filter the list of locales for any with the given prefixes (case-insitive).
    matchingLocales
        :: [Text] -> Text -> [Text]
    matchingLocales utf8Locales prefix =
        filter (\v -> T.toLower prefix `T.isPrefixOf` T.toLower v) utf8Locales
    -- Does the locale have one of the encodings in @utf8Suffixes@ (case-insensitive)?
    isUtf8Locale locale =
      any (\ v -> T.toLower v `T.isSuffixOf` T.toLower locale) utf8Suffixes
    -- Prefixes of fallback locales (case-insensitive)
    fallbackPrefixes = ["C.", "en_US.", "en_"]
    -- Suffixes of UTF-8 locales (case-insensitive)
    utf8Suffixes = [".UTF-8", ".utf8"]
