{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
  , getCabalInstallVersion
  , SetupOpts (..)
  , defaultSetupInfoYaml
  , removeHaskellEnvVars

  -- * Stack binary download
  , StackReleaseInfo
  , getDownloadVersion
  , stackVersion
  , preferredPlatforms
  , downloadStackReleaseInfo
  , downloadStackExe
  ) where

import qualified    Codec.Archive.Tar as Tar
import              Control.Applicative (empty)
import              Control.Monad.State (get, put, modify)
import "cryptonite" Crypto.Hash (SHA1(..), SHA256(..))
import              Data.Aeson.Extended
import qualified    Data.ByteString as S
import qualified    Data.ByteString.Lazy as LBS
import qualified    Data.ByteString.Lazy.Char8 as BL8
import              Data.Char (isSpace)
import              Data.Conduit (await, yield, awaitForever)
import qualified    Data.Conduit.Binary as CB
import              Data.Conduit.Lazy (lazyConsume)
import              Data.Conduit.Lift (evalStateC)
import qualified    Data.Conduit.List as CL
import              Data.Conduit.Process.Typed (eceStderr)
import              Data.Conduit.Zlib          (ungzip)
import              Data.Foldable (maximumBy)
import qualified    Data.HashMap.Strict as HashMap
import              Data.IORef.RunOnce (runOnce)
import              Data.List hiding (concat, elem, maximumBy, any)
import qualified    Data.Map as Map
import qualified    Data.Set as Set
import qualified    Data.Text as T
import qualified    Data.Text.Encoding as T
import qualified    Data.Text.Encoding.Error as T
import              Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import qualified    Data.Yaml as Yaml
import              Distribution.System (OS, Arch (..), Platform (..))
import qualified    Distribution.System as Cabal
import              Distribution.Text (simpleParse)
import              Lens.Micro (set)
import              Network.HTTP.Simple (getResponseBody, getResponseStatusCode)
import              Network.HTTP.Download
import              Path
import              Path.CheckInstall (warnInstallSearchPathIssues)
import              Path.Extra (toFilePathNoTrailingSep)
import              Path.IO hiding (findExecutable, withSystemTempDir)
import              Prelude (getLine, putStr, putStrLn, until)
import qualified    RIO
import              Stack.Build (build)
import              Stack.Config (loadConfig)
import              Stack.Constants (stackProgName)
import              Stack.Constants.Config (distRelativeDir)
import              Stack.Fetch
import              Stack.GhcPkg (createDatabase, getCabalPkgVer, getGlobalDB, mkGhcPackagePath, ghcPkgPathEnvVar)
import              Stack.Prelude hiding (Display (..))
import              Stack.PrettyPrint
import              Stack.Setup.Installed
import              Stack.Snapshot (loadSnapshot)
import              Stack.Types.Build
import              Stack.Types.Compiler
import              Stack.Types.CompilerBuild
import              Stack.Types.Config
import              Stack.Types.Docker
import              Stack.Types.PackageIdentifier
import              Stack.Types.PackageName
import              Stack.Types.Runner
import              Stack.Types.Version
import qualified    System.Directory as D
import              System.Environment (getExecutablePath, lookupEnv)
import              System.Exit (ExitCode (..), exitFailure)
import              System.IO (stdout)
import              System.IO.Error (isPermissionError)
import              System.FilePath (searchPathSeparator)
import qualified    System.FilePath as FP
import              RIO.Process
import              Text.Printf (printf)

#if !WINDOWS
import              Bindings.Uname (uname, release)
import              Data.List.Split (splitOn)
import              Foreign.C (throwErrnoIfMinus1_, peekCString)
import              Foreign.Marshal (alloca)
import              System.Posix.Files (setFileMode)
#endif

-- | Default location of the stack-setup.yaml file
defaultSetupInfoYaml :: String
defaultSetupInfoYaml =
    "https://raw.githubusercontent.com/fpco/stackage-content/master/stack/stack-setup-2.yaml"

data SetupOpts = SetupOpts
    { soptsInstallIfMissing :: !Bool
    , soptsUseSystem :: !Bool
    -- ^ Should we use a system compiler installation, if available?
    , soptsWantedCompiler :: !(CompilerVersion 'CVWanted)
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
    , soptsUpgradeCabal :: !(Maybe UpgradeTo)
    -- ^ Upgrade the global Cabal library in the database to the newest
    -- version. Only works reliably with a stack-managed installation.
    , soptsResolveMissingGHC :: !(Maybe Text)
    -- ^ Message shown to user for how to resolve the missing GHC
    , soptsSetupInfoYaml :: !FilePath
    -- ^ Location of the main stack-setup.yaml file
    , soptsGHCBindistURL :: !(Maybe String)
    -- ^ Alternate GHC binary distribution (requires custom GHCVariant)
    , soptsGHCJSBootOpts :: [String]
    -- ^ Additional ghcjs-boot options, the default is "--clean"
    }
    deriving Show
data SetupException = UnsupportedSetupCombo OS Arch
                    | MissingDependencies [String]
                    | UnknownCompilerVersion (Set.Set Text) (CompilerVersion 'CVWanted) (Set.Set (CompilerVersion 'CVActual))
                    | UnknownOSKey Text
                    | GHCSanityCheckCompileFailed SomeException (Path Abs File)
                    | WantedMustBeGHC
                    | RequireCustomGHCVariant
                    | ProblemWhileDecompressing (Path Abs File)
                    | SetupInfoMissingSevenz
                    | GHCJSRequiresStandardVariant
                    | GHCJSNotBooted
                    | DockerStackExeNotFound Version Text
                    | UnsupportedSetupConfiguration
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
    show (UnknownCompilerVersion oskeys wanted known) = concat
        [ "No setup information found for "
        , compilerVersionString wanted
        , " on your platform.\nThis probably means a GHC bindist has not yet been added for OS key '"
        , T.unpack (T.intercalate "', '" (sort $ Set.toList oskeys))
        , "'.\nSupported versions: "
        , T.unpack (T.intercalate ", " (map compilerVersionText (sort $ Set.toList known)))
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
    show (DockerStackExeNotFound stackVersion' osKey) = concat
        [ stackProgName
        , "-"
        , versionString stackVersion'
        , " executable not found for "
        , T.unpack osKey
        , "\nUse the '"
        , T.unpack dockerStackExeArgName
        , "' option to specify a location"]
    show UnsupportedSetupConfiguration =
        "I don't know how to install GHC on your system configuration, please install manually"

-- | Modify the environment variables (like PATH) appropriately, possibly doing installation too
setupEnv :: (HasBuildConfig env, HasGHCVariant env)
         => Maybe Text -- ^ Message to give user when necessary GHC is not available
         -> RIO env EnvConfig
setupEnv mResolveMissingGHC = do
    config <- view configL
    bconfig <- view buildConfigL
    let stackYaml = bcStackYaml bconfig
    platform <- view platformL
    wcVersion <- view wantedCompilerVersionL
    wc <- view $ wantedCompilerVersionL.whichCompilerL
    let sopts = SetupOpts
            { soptsInstallIfMissing = configInstallGHC config
            , soptsUseSystem = configSystemGHC config
            , soptsWantedCompiler = wcVersion
            , soptsCompilerCheck = configCompilerCheck config
            , soptsStackYaml = Just stackYaml
            , soptsForceReinstall = False
            , soptsSanityCheck = False
            , soptsSkipGhcCheck = configSkipGHCCheck config
            , soptsSkipMsys = configSkipMsys config
            , soptsUpgradeCabal = Nothing
            , soptsResolveMissingGHC = mResolveMissingGHC
            , soptsSetupInfoYaml = defaultSetupInfoYaml
            , soptsGHCBindistURL = Nothing
            , soptsGHCJSBootOpts = ["--clean"]
            }

    (mghcBin, compilerBuild, _) <- ensureCompiler sopts

    -- Modify the initial environment to include the GHC path, if a local GHC
    -- is being used
    menv0 <- view processContextL
    env <- either throwM (return . removeHaskellEnvVars)
               $ augmentPathMap
                    (map toFilePath $ maybe [] edBins mghcBin)
                    (view envVarsL menv0)
    menv <- mkProcessContext env

    (compilerVer, cabalVer, globaldb) <- withProcessContext menv $ runConcurrently $ (,,)
        <$> Concurrently (getCompilerVersion wc)
        <*> Concurrently (getCabalPkgVer wc)
        <*> Concurrently (getGlobalDB wc)

    logDebug "Resolving package entries"
    packagesRef <- liftIO $ newIORef Nothing
    bc <- view buildConfigL

    -- Set up a modified environment which includes the modified PATH
    -- that GHC can be found on. This is needed for looking up global
    -- package information in loadSnapshot.
    let bcPath :: BuildConfig
        bcPath = set processContextL menv bc

    ls <- runRIO bcPath $ loadSnapshot
      (Just compilerVer)
      (view projectRootL bc)
      (bcSnapshotDef bc)
    let envConfig0 = EnvConfig
            { envConfigBuildConfig = bc
            , envConfigCabalVersion = cabalVer
            , envConfigCompilerVersion = compilerVer
            , envConfigCompilerBuild = compilerBuild
            , envConfigPackagesRef = packagesRef
            , envConfigLoadedSnapshot = ls
            }

    -- extra installation bin directories
    mkDirs <- runReaderT extraBinDirs envConfig0
    let mpath = Map.lookup "PATH" env
    depsPath <- either throwM return $ augmentPath (toFilePath <$> mkDirs False) mpath
    localsPath <- either throwM return $ augmentPath (toFilePath <$> mkDirs True) mpath

    deps <- runReaderT packageDatabaseDeps envConfig0
    withProcessContext menv $ createDatabase wc deps
    localdb <- runReaderT packageDatabaseLocal envConfig0
    withProcessContext menv $ createDatabase wc localdb
    extras <- runReaderT packageDatabaseExtra envConfig0
    let mkGPP locals = mkGhcPackagePath locals localdb deps extras globaldb

    distDir <- runReaderT distRelativeDir envConfig0

    executablePath <- liftIO getExecutablePath

    utf8EnvVars <- withProcessContext menv $ getUtf8EnvVars compilerVer

    mGhcRtsEnvVar <- liftIO $ lookupEnv "GHCRTS"

    envRef <- liftIO $ newIORef Map.empty
    let getProcessContext' es = do
            m <- readIORef envRef
            case Map.lookup es m of
                Just eo -> return eo
                Nothing -> do
                    eo <- mkProcessContext
                        $ Map.insert "PATH" (if esIncludeLocals es then localsPath else depsPath)
                        $ (if esIncludeGhcPackagePath es
                                then Map.insert (ghcPkgPathEnvVar wc) (mkGPP (esIncludeLocals es))
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

                        -- See https://github.com/commercialhaskell/stack/issues/3444
                        $ case (esKeepGhcRts es, mGhcRtsEnvVar) of
                            (True, Just ghcRts) -> Map.insert "GHCRTS" (T.pack ghcRts)
                            _ -> id

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

    envOverride <- liftIO $ getProcessContext' minimalEnvSettings
    return EnvConfig
        { envConfigBuildConfig = bconfig
            { bcConfig = maybe id addIncludeLib mghcBin
                       $ set processContextL envOverride
                         (view configL bconfig)
                { configProcessContextSettings = getProcessContext'
                }
            }
        , envConfigCabalVersion = cabalVer
        , envConfigCompilerVersion = compilerVer
        , envConfigCompilerBuild = compilerBuild
        , envConfigPackagesRef = envConfigPackagesRef envConfig0
        , envConfigLoadedSnapshot = ls
        }

-- | Add the include and lib paths to the given Config
addIncludeLib :: ExtraDirs -> Config -> Config
addIncludeLib (ExtraDirs _bins includes libs) config = config
    { configExtraIncludeDirs = Set.union
        (configExtraIncludeDirs config)
        (Set.fromList (map toFilePathNoTrailingSep includes))
    , configExtraLibDirs = Set.union
        (configExtraLibDirs config)
        (Set.fromList (map toFilePathNoTrailingSep libs))
    }

-- | Ensure compiler (ghc or ghcjs) is installed and provide the PATHs to add if necessary
ensureCompiler :: (HasConfig env, HasGHCVariant env)
               => SetupOpts
               -> RIO env (Maybe ExtraDirs, CompilerBuild, Bool)
ensureCompiler sopts = do
    let wc = whichCompiler (soptsWantedCompiler sopts)
    when (getGhcVersion (soptsWantedCompiler sopts) < $(mkVersion "7.8")) $ do
        logWarn "Stack will almost certainly fail with GHC below version 7.8"
        logWarn "Valiantly attempting to run anyway, but I know this is doomed"
        logWarn "For more information, see: https://github.com/commercialhaskell/stack/issues/648"
        logWarn ""

    msystem <-
        if soptsUseSystem sopts
            then do
                logDebug "Getting system compiler version"
                getSystemCompiler wc
            else return Nothing

    Platform expectedArch _ <- view platformL

    let canUseCompiler compilerVersion arch
            | soptsSkipGhcCheck sopts = True
            | otherwise = isWanted compilerVersion && arch == expectedArch
        isWanted = isWantedCompiler (soptsCompilerCheck sopts) (soptsWantedCompiler sopts)
        needLocal = not (any (uncurry canUseCompiler) msystem)

    getSetupInfo' <- runOnce (getSetupInfo (soptsSetupInfoYaml sopts))

    let getMmsys2Tool = do
            platform <- view platformL
            localPrograms <- view $ configL.to configLocalPrograms
            installed <- listInstalled localPrograms

            case platform of
                Platform _ Cabal.Windows | not (soptsSkipMsys sopts) ->
                    case getInstalledTool installed $(mkPackageName "msys2") (const True) of
                        Just tool -> return (Just tool)
                        Nothing
                            | soptsInstallIfMissing sopts -> do
                                si <- getSetupInfo'
                                osKey <- getOSKey platform
                                config <- view configL
                                VersionedDownloadInfo version info <-
                                    case Map.lookup osKey $ siMsys2 si of
                                        Just x -> return x
                                        Nothing -> throwString $ "MSYS2 not found for " ++ T.unpack osKey
                                let tool = Tool (PackageIdentifier $(mkPackageName "msys2") version)
                                Just <$> downloadAndInstallTool (configLocalPrograms config) si info tool (installMsys2Windows osKey)
                            | otherwise -> do
                                logWarn "Continuing despite missing tool: msys2"
                                return Nothing
                _ -> return Nothing


        -- If we need to install a GHC or MSYS, try to do so
        -- Return the additional directory paths of GHC & MSYS.
    (mtools, compilerBuild) <- if needLocal
        then do

            -- Install GHC
            ghcVariant <- view ghcVariantL
            config <- view configL
            let localPrograms = configLocalPrograms config
            installed <- listInstalled localPrograms

            possibleCompilers <-
                    case wc of
                        Ghc -> do
                            ghcBuilds <- getGhcBuilds
                            forM ghcBuilds $ \ghcBuild -> do
                                ghcPkgName <- parsePackageNameFromString ("ghc" ++ ghcVariantSuffix ghcVariant ++ compilerBuildSuffix ghcBuild)
                                return (getInstalledTool installed ghcPkgName (isWanted . GhcVersion), ghcBuild)
                        Ghcjs -> return [(getInstalledGhcjs installed isWanted, CompilerBuildStandard)]
            let existingCompilers = concatMap
                    (\(installedCompiler, compilerBuild) ->
                        case (installedCompiler, soptsForceReinstall sopts) of
                            (Just tool, False) -> [(tool, compilerBuild)]
                            _ -> [])
                    possibleCompilers
            logDebug $
              "Found already installed GHC builds: " <>
              mconcat (intersperse ", " (map (fromString . compilerBuildName . snd) existingCompilers))
            (compilerTool, compilerBuild) <- case existingCompilers of
                (tool, build_):_ -> return (tool, build_)
                []
                    | soptsInstallIfMissing sopts -> do
                        si <- getSetupInfo'
                        downloadAndInstallPossibleCompilers
                            (map snd possibleCompilers)
                            si
                            (soptsWantedCompiler sopts)
                            (soptsCompilerCheck sopts)
                            (soptsGHCBindistURL sopts)
                    | otherwise -> do
                        recommendSystemGhc <-
                            if soptsUseSystem sopts
                                then return False
                                else do
                                    msystemGhc <- getSystemCompiler wc
                                    return (any (uncurry canUseCompiler) msystemGhc)
                        let suggestion = fromMaybe
                                (mconcat
                                     ([ "To install the correct GHC into "
                                      , T.pack (toFilePath (configLocalPrograms config))
                                      , ", try running \"stack setup\" or use the \"--install-ghc\" flag."
                                      ] ++
                                      [ " To use your system GHC installation, run \"stack config set system-ghc --global true\", or use the \"--system-ghc\" flag."
                                      | recommendSystemGhc
                                      ]))
                                (soptsResolveMissingGHC sopts)
                        throwM $ CompilerVersionMismatch
                            msystem
                            (soptsWantedCompiler sopts, expectedArch)
                            ghcVariant
                            (case possibleCompilers of
                                [] -> CompilerBuildStandard
                                (_, compilerBuild):_ -> compilerBuild)
                            (soptsCompilerCheck sopts)
                            (soptsStackYaml sopts)
                            suggestion

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
            Nothing -> view processContextL
            Just ed -> do
                menv0 <- view processContextL
                m <- either throwM return
                   $ augmentPathMap (toFilePath <$> edBins ed) (view envVarsL menv0)
                mkProcessContext (removeHaskellEnvVars m)

    forM_ (soptsUpgradeCabal sopts) $ \version -> do
        unless needLocal $ do
            logWarn "Trying to change a Cabal library on a GHC not installed by stack."
            logWarn "This may fail, caveat emptor!"
        withProcessContext menv $ upgradeCabal wc version

    case mtools of
        Just (Just (ToolGhcjs cv), _) ->
            withProcessContext menv
          $ ensureGhcjsBooted cv (soptsInstallIfMissing sopts) (soptsGHCJSBootOpts sopts)
        _ -> return ()

    when (soptsSanityCheck sopts) $ withProcessContext menv $ sanityCheck wc

    return (mpaths, compilerBuild, needLocal)

-- | Determine which GHC builds to use depending on which shared libraries are available
-- on the system.
getGhcBuilds :: HasConfig env => RIO env [CompilerBuild]
getGhcBuilds = do

    config <- view configL
    case configGHCBuild config of
        Just ghcBuild -> return [ghcBuild]
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

        platform <- view platformL
        case platform of
            Platform _ Cabal.Linux -> do
                -- Some systems don't have ldconfig in the PATH, so make sure to look in /sbin and /usr/sbin as well
                let sbinEnv m = Map.insert
                      "PATH"
                      ("/sbin:/usr/sbin" <> maybe "" (":" <>) (Map.lookup "PATH" m))
                      m
                eldconfigOut
                  <- withModifyEnvVars sbinEnv
                   $ proc "ldconfig" ["-p"]
                   $ tryAny . readProcessStdout_
                let firstWords = case eldconfigOut of
                        Right ldconfigOut -> mapMaybe (listToMaybe . T.words) $
                            T.lines $ T.decodeUtf8With T.lenientDecode
                                    $ LBS.toStrict ldconfigOut
                        Left _ -> []
                    checkLib lib
                        | libT `elem` firstWords = do
                            logDebug ("Found shared library " <> libD <> " in 'ldconfig -p' output")
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
                                then logDebug ("Found shared library " <> libD <> " in /usr/lib")
                                else logDebug ("Did not find shared library " <> libD)
                            return e
#endif
                      where
                        libT = T.pack (toFilePath lib)
                        libD = fromString (toFilePath lib)
                hastinfo5 <- checkLib $(mkRelFile "libtinfo.so.5")
                hastinfo6 <- checkLib $(mkRelFile "libtinfo.so.6")
                hasncurses6 <- checkLib $(mkRelFile "libncursesw.so.6")
                hasgmp5 <- checkLib $(mkRelFile "libgmp.so.10")
                hasgmp4 <- checkLib $(mkRelFile "libgmp.so.3")
                let libComponents = concat
                        [ [["tinfo6"] | hastinfo6 && hasgmp5]
                        , [[] | hastinfo5 && hasgmp5]
                        , [["ncurses6"] | hasncurses6 && hasgmp5 ]
                        , [["gmp4"] | hasgmp4 ]
                        ]
                useBuilds $ map
                    (\c -> case c of
                        [] -> CompilerBuildStandard
                        _ -> CompilerBuildSpecialized (intercalate "-" c))
                    libComponents
#if !WINDOWS
            Platform _ Cabal.OpenBSD -> do
                releaseStr <- mungeRelease <$> sysRelease
                useBuilds [CompilerBuildSpecialized releaseStr]
#endif
            _ -> useBuilds [CompilerBuildStandard]
    useBuilds builds = do
        logDebug $
          "Potential GHC builds: " <>
          mconcat (intersperse ", " (map (fromString . compilerBuildName) builds))
        return builds

#if !WINDOWS
-- | Encode an OpenBSD version (like "6.1") into a valid argument for
-- CompilerBuildSpecialized, so "maj6-min1". Later version numbers are prefixed
-- with "r".
-- The result r must be such that "ghc-" ++ r is a valid package name,
-- as recognized by parsePackageNameFromString.
mungeRelease :: String -> String
mungeRelease = intercalate "-" . prefixMaj . splitOn "."
  where
    prefixFst pfx k (rev : revs) = (pfx ++ rev) : k revs
    prefixFst _ _ [] = []
    prefixMaj = prefixFst "maj" prefixMin
    prefixMin = prefixFst "min" (map ('r':))

sysRelease :: HasLogFunc env => RIO env String
sysRelease =
  handleIO (\e -> do
               logWarn $ "Could not query OS version" <> displayShow e
               return "") .
  liftIO .
  alloca $ \ ptr ->
             do throwErrnoIfMinus1_ "uname" $ uname ptr
                peekCString $ release ptr
#endif

-- | Ensure Docker container-compatible 'stack' executable is downloaded
ensureDockerStackExe :: HasConfig env => Platform -> RIO env (Path Abs File)
ensureDockerStackExe containerPlatform = do
    config <- view configL
    containerPlatformDir <- runReaderT platformOnlyRelDir (containerPlatform,PlatformVariantNone)
    let programsPath = configLocalProgramsBase config </> containerPlatformDir
        tool = Tool (PackageIdentifier $(mkPackageName "stack") stackVersion)
    stackExeDir <- installDir programsPath tool
    let stackExePath = stackExeDir </> $(mkRelFile "stack")
    stackExeExists <- doesFileExist stackExePath
    unless stackExeExists $ do
        logInfo $
          "Downloading Docker-compatible " <>
          fromString stackProgName <>
          " executable"
        sri <- downloadStackReleaseInfo Nothing Nothing (Just (versionString stackMinorVersion))
        platforms <- runReaderT preferredPlatforms (containerPlatform, PlatformVariantNone)
        downloadStackExe platforms sri stackExeDir False (const $ return ())
    return stackExePath

-- | Install the newest version or a specific version of Cabal globally
upgradeCabal :: (HasConfig env, HasGHCVariant env)
             => WhichCompiler
             -> UpgradeTo
             -> RIO env ()
upgradeCabal wc upgradeTo = do
    logInfo "Manipulating the global Cabal is only for debugging purposes"
    let name = $(mkPackageName "Cabal")
    rmap <- resolvePackages Nothing mempty (Set.singleton name)
    installed <- getCabalPkgVer wc
    case upgradeTo of
        Specific wantedVersion -> do
            if installed /= wantedVersion then
                doCabalInstall wc installed wantedVersion
            else
                logInfo $
                  "No install necessary. Cabal " <>
                  RIO.display installed <>
                  " is already installed"
        Latest     -> case map rpIdent rmap of
            [] -> throwString "No Cabal library found in index, cannot upgrade"
            [PackageIdentifier name' latestVersion] | name == name' -> do
                if installed < latestVersion then
                    doCabalInstall wc installed latestVersion
                else
                    logInfo $
                        "No upgrade necessary: Cabal-" <>
                        RIO.display latestVersion <>
                        " is the same or newer than latest hackage version " <>
                        RIO.display installed
            x -> error $ "Unexpected results for resolvePackages: " ++ show x

-- Configure and run the necessary commands for a cabal install
doCabalInstall :: (HasConfig env, HasGHCVariant env)
               => WhichCompiler
               -> Version
               -> Version
               -> RIO env ()
doCabalInstall wc installed wantedVersion = do
    withSystemTempDir "stack-cabal-upgrade" $ \tmpdir -> do
        logInfo $
            "Installing Cabal-" <>
            RIO.display wantedVersion <>
            " to replace " <>
            RIO.display installed
        let name = $(mkPackageName "Cabal")
            ident = PackageIdentifier name wantedVersion
        m <- unpackPackageIdents tmpdir Nothing [PackageIdentifierRevision ident CFILatest]
        compilerPath <- findExecutable (compilerExeName wc)
                    >>= either throwM parseAbsFile
        versionDir <- parseRelDir $ versionString wantedVersion
        let installRoot = toFilePath $ parent (parent compilerPath)
                                    </> $(mkRelDir "new-cabal")
                                    </> versionDir
        dir <- case Map.lookup ident m of
            Nothing -> error "upgradeCabal: Invariant violated, dir missing"
            Just dir -> return dir
        withWorkingDir (toFilePath dir) $ proc (compilerExeName wc) ["Setup.hs"] runProcess_
        platform <- view platformL
        let setupExe = toFilePath $ dir </> case platform of
                Platform _ Cabal.Windows -> $(mkRelFile "Setup.exe")
                _                        -> $(mkRelFile "Setup")
            dirArgument name' = concat [ "--"
                                       , name'
                                       , "dir="
                                       , installRoot FP.</> name'
                                       ]
            args = "configure" : map dirArgument (words "lib bin data doc")
        withWorkingDir (toFilePath dir) $ do
          proc setupExe args runProcess_
          proc setupExe ["build"] runProcess_
          proc setupExe ["install"] runProcess_
        logInfo "New Cabal library installed"

-- | Get the version of the system compiler, if available
getSystemCompiler
  :: (HasProcessContext env, HasLogFunc env)
  => WhichCompiler
  -> RIO env (Maybe (CompilerVersion 'CVActual, Arch))
getSystemCompiler wc = do
    let exeName = case wc of
            Ghc -> "ghc"
            Ghcjs -> "ghcjs"
    exists <- doesExecutableExist exeName
    if exists
        then do
            eres <- proc exeName ["--info"] $ tryAny . readProcessStdout_
            let minfo = do
                    Right lbs <- Just eres
                    pairs_ <- readMaybe $ BL8.unpack lbs :: Maybe [(String, String)]
                    version <- lookup "Project version" pairs_ >>= parseVersionFromString
                    arch <- lookup "Target platform" pairs_ >>= simpleParse . takeWhile (/= '-')
                    return (version, arch)
            case (wc, minfo) of
                (Ghc, Just (version, arch)) -> return (Just (GhcVersion version, arch))
                (Ghcjs, Just (_, arch)) -> do
                    eversion <- tryAny $ getCompilerVersion Ghcjs
                    case eversion of
                        Left _ -> return Nothing
                        Right version -> return (Just (version, arch))
                (_, Nothing) -> return Nothing
        else return Nothing

-- | Download the most recent SetupInfo
getSetupInfo :: HasConfig env => String -> RIO env SetupInfo
getSetupInfo stackSetupYaml = do
    config <- view configL
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
                Just req -> liftM (LBS.toStrict . getResponseBody) $ httpLBS req
                Nothing -> liftIO $ S.readFile urlOrFile
        WithJSONWarnings si warnings <- either throwM return (Yaml.decodeEither' bs)
        when (urlOrFile /= defaultSetupInfoYaml) $
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
                  -> (CompilerVersion 'CVActual -> Bool)
                  -> Maybe Tool
getInstalledGhcjs installed goodVersion =
    if null available
        then Nothing
        else Just $ ToolGhcjs $ maximum available
  where
    available = mapMaybe goodPackage installed
    goodPackage (ToolGhcjs cv) = if goodVersion cv then Just cv else Nothing
    goodPackage _ = Nothing

downloadAndInstallTool :: HasRunner env
                       => Path Abs Dir
                       -> SetupInfo
                       -> DownloadInfo
                       -> Tool
                       -> (SetupInfo -> Path Abs File -> ArchiveType -> Path Abs Dir -> Path Abs Dir -> RIO env ())
                       -> RIO env Tool
downloadAndInstallTool programsDir si downloadInfo tool installer = do
    ensureDir programsDir
    (file, at) <- downloadFromInfo programsDir downloadInfo tool
    dir <- installDir programsDir tool
    tempDir <- tempInstallDir programsDir tool
    liftIO $ ignoringAbsence (removeDirRecur tempDir)
    ensureDir tempDir
    unmarkInstalled programsDir tool
    installer si file at tempDir dir
    markInstalled programsDir tool
    liftIO $ ignoringAbsence (removeDirRecur tempDir)
    return tool

downloadAndInstallCompiler :: (HasConfig env, HasGHCVariant env)
                           => CompilerBuild
                           -> SetupInfo
                           -> CompilerVersion 'CVWanted
                           -> VersionCheck
                           -> Maybe String
                           -> RIO env Tool
downloadAndInstallCompiler ghcBuild si wanted@GhcVersion{} versionCheck mbindistURL = do
    ghcVariant <- view ghcVariantL
    (selectedVersion, downloadInfo) <- case mbindistURL of
        Just bindistURL -> do
            case ghcVariant of
                GHCCustom _ -> return ()
                _ -> throwM RequireCustomGHCVariant
            case wanted of
                GhcVersion version ->
                    return (version, GHCDownloadInfo mempty mempty DownloadInfo
                             { downloadInfoUrl = T.pack bindistURL
                             , downloadInfoContentLength = Nothing
                             , downloadInfoSha1 = Nothing
                             , downloadInfoSha256 = Nothing
                             })
                _ ->
                    throwM WantedMustBeGHC
        _ -> do
            ghcKey <- getGhcKey ghcBuild
            case Map.lookup ghcKey $ siGHCs si of
                Nothing -> throwM $ UnknownOSKey ghcKey
                Just pairs_ -> getWantedCompilerInfo ghcKey versionCheck wanted GhcVersion pairs_
    config <- view configL
    let installer =
            case configPlatform config of
                Platform _ Cabal.Windows -> installGHCWindows selectedVersion
                _ -> installGHCPosix selectedVersion downloadInfo
    logInfo $
        "Preparing to install GHC" <>
        (case ghcVariant of
            GHCStandard -> ""
            v -> " (" <> fromString (ghcVariantName v) <> ")") <>
        (case ghcBuild of
            CompilerBuildStandard -> ""
            b -> " (" <> fromString (compilerBuildName b) <> ")") <>
        " to an isolated location."
    logInfo "This will not interfere with any system-level installation."
    ghcPkgName <- parsePackageNameFromString ("ghc" ++ ghcVariantSuffix ghcVariant ++ compilerBuildSuffix ghcBuild)
    let tool = Tool $ PackageIdentifier ghcPkgName selectedVersion
    downloadAndInstallTool (configLocalPrograms config) si (gdiDownloadInfo downloadInfo) tool installer
downloadAndInstallCompiler compilerBuild si wanted versionCheck _mbindistUrl = do
    config <- view configL
    ghcVariant <- view ghcVariantL
    case (ghcVariant, compilerBuild) of
        (GHCStandard, CompilerBuildStandard) -> return ()
        _ -> throwM GHCJSRequiresStandardVariant
    (selectedVersion, downloadInfo) <- case Map.lookup "source" $ siGHCJSs si of
        Nothing -> throwM $ UnknownOSKey "source"
        Just pairs_ -> getWantedCompilerInfo "source" versionCheck wanted id pairs_
    logInfo "Preparing to install GHCJS to an isolated location."
    logInfo "This will not interfere with any system-level installation."
    let tool = ToolGhcjs selectedVersion
    downloadAndInstallTool (configLocalPrograms config) si downloadInfo tool installGHCJS

getWantedCompilerInfo :: (Ord k, MonadThrow m)
                      => Text
                      -> VersionCheck
                      -> CompilerVersion 'CVWanted
                      -> (k -> CompilerVersion 'CVActual)
                      -> Map k a
                      -> m (k, a)
getWantedCompilerInfo key versionCheck wanted toCV pairs_ =
    case mpair of
        Just pair -> return pair
        Nothing -> throwM $ UnknownCompilerVersion (Set.singleton key) wanted (Set.fromList $ map toCV (Map.keys pairs_))
  where
    mpair =
        listToMaybe $
        sortBy (flip (comparing fst)) $
        filter (isWantedCompiler versionCheck wanted . toCV . fst) (Map.toList pairs_)

-- | Download and install the first available compiler build.
downloadAndInstallPossibleCompilers
    :: (HasGHCVariant env, HasConfig env)
    => [CompilerBuild]
    -> SetupInfo
    -> CompilerVersion 'CVWanted
    -> VersionCheck
    -> Maybe String
    -> RIO env (Tool, CompilerBuild)
downloadAndInstallPossibleCompilers possibleCompilers si wanted versionCheck mbindistURL =
    go possibleCompilers Nothing
  where
    -- This will stop as soon as one of the builds doesn't throw an @UnknownOSKey@ or
    -- @UnknownCompilerVersion@ exception (so it will only try subsequent builds if one is non-existent,
    -- not if the download or install fails for some other reason).
    -- The @Unknown*@ exceptions thrown by each attempt are combined into a single exception
    -- (if only @UnknownOSKey@ is thrown, then the first of those is rethrown, but if any
    -- @UnknownCompilerVersion@s are thrown then the attempted OS keys and available versions
    -- are unioned).
    go [] Nothing = throwM UnsupportedSetupConfiguration
    go [] (Just e) = throwM e
    go (b:bs) e = do
        logDebug $ "Trying to setup GHC build: " <> fromString (compilerBuildName b)
        er <- try $ downloadAndInstallCompiler b si wanted versionCheck mbindistURL
        case er of
            Left e'@(UnknownCompilerVersion ks' w' vs') ->
                case e of
                    Nothing -> go bs (Just e')
                    Just (UnknownOSKey k) ->
                        go bs $ Just $ UnknownCompilerVersion (Set.insert k ks') w' vs'
                    Just (UnknownCompilerVersion ks _ vs) ->
                        go bs $ Just $ UnknownCompilerVersion (Set.union ks' ks) w' (Set.union vs' vs)
                    Just x -> throwM x
            Left e'@(UnknownOSKey k') ->
                case e of
                    Nothing -> go bs (Just e')
                    Just (UnknownOSKey _) -> go bs e
                    Just (UnknownCompilerVersion ks w vs) ->
                        go bs $ Just $ UnknownCompilerVersion (Set.insert k' ks) w vs
                    Just x -> throwM x
            Left e' -> throwM e'
            Right r -> return (r, b)

getGhcKey :: (MonadReader env m, HasPlatform env, HasGHCVariant env, MonadThrow m)
          => CompilerBuild -> m Text
getGhcKey ghcBuild = do
    ghcVariant <- view ghcVariantL
    platform <- view platformL
    osKey <- getOSKey platform
    return $ osKey <> T.pack (ghcVariantSuffix ghcVariant) <> T.pack (compilerBuildSuffix ghcBuild)

getOSKey :: (MonadThrow m)
         => Platform -> m Text
getOSKey platform =
    case platform of
        Platform I386                  Cabal.Linux   -> return "linux32"
        Platform X86_64                Cabal.Linux   -> return "linux64"
        Platform I386                  Cabal.OSX     -> return "macosx"
        Platform X86_64                Cabal.OSX     -> return "macosx"
        Platform I386                  Cabal.FreeBSD -> return "freebsd32"
        Platform X86_64                Cabal.FreeBSD -> return "freebsd64"
        Platform I386                  Cabal.OpenBSD -> return "openbsd32"
        Platform X86_64                Cabal.OpenBSD -> return "openbsd64"
        Platform I386                  Cabal.Windows -> return "windows32"
        Platform X86_64                Cabal.Windows -> return "windows64"
        Platform Arm                   Cabal.Linux   -> return "linux-armv7"
        Platform (OtherArch "aarch64") Cabal.Linux   -> return "linux-aarch64"
        Platform arch os -> throwM $ UnsupportedSetupCombo os arch

downloadFromInfo
    :: HasRunner env
    => Path Abs Dir -> DownloadInfo -> Tool -> RIO env (Path Abs File, ArchiveType)
downloadFromInfo programsDir downloadInfo tool = do
    at <-
        case extension of
            ".tar.xz" -> return TarXz
            ".tar.bz2" -> return TarBz2
            ".tar.gz" -> return TarGz
            ".7z.exe" -> return SevenZ
            _ -> throwString $ "Error: Unknown extension for url: " ++ url
    relativeFile <- parseRelFile $ toolString tool ++ extension
    path <- case url of
        (parseUrlThrow -> Just _) -> do
            let path = programsDir </> relativeFile
            ensureDir programsDir
            chattyDownload (T.pack (toolString tool)) downloadInfo path
            return path
        (parseAbsFile -> Just path) -> do
            let DownloadInfo{downloadInfoContentLength=contentLength, downloadInfoSha1=sha1,
                             downloadInfoSha256=sha256} =
                    downloadInfo
            when (isJust contentLength) $
                logWarn ("`content-length` in not checked \n" <>
                          "and should not be specified when `url` is a file path")
            when (isJust sha1) $
                logWarn ("`sha1` is not checked and \n" <>
                          "should not be specified when `url` is a file path")
            when (isJust sha256) $
                logWarn ("`sha256` is not checked and \n" <>
                          "should not be specified when `url` is a file path")
            return path
        _ ->
            throwString $ "Error: `url` must be either an HTTP URL or absolute file path: " ++ url
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

installGHCPosix :: HasConfig env
                => Version
                -> GHCDownloadInfo
                -> SetupInfo
                -> Path Abs File
                -> ArchiveType
                -> Path Abs Dir
                -> Path Abs Dir
                -> RIO env ()
installGHCPosix version downloadInfo _ archiveFile archiveType tempDir destDir = do
    platform <- view platformL
    menv0 <- view processContextL
    menv <- mkProcessContext (removeHaskellEnvVars (view envVarsL menv0))
    logDebug $ "menv = " <> displayShow (view envVarsL menv)
    (zipTool', compOpt) <-
        case archiveType of
            TarXz -> return ("xz", 'J')
            TarBz2 -> return ("bzip2", 'j')
            TarGz -> return ("gzip", 'z')
            SevenZ -> throwString "Don't know how to deal with .7z files on non-Windows"
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

    logDebug $ "ziptool: " <> fromString zipTool
    logDebug $ "make: " <> fromString makeTool
    logDebug $ "tar: " <> fromString tarTool

    dir <-
        liftM (tempDir </>) $
        parseRelDir $
        "ghc-" ++ versionString version

    let runStep step wd env cmd args = do
            menv' <- modifyEnvVars menv (Map.union env)
            result <- do
                let logLines = CB.lines .| CL.mapM_ (logDebug . displayBytesUtf8)
                withWorkingDir (toFilePath wd)
                  $ withProcessContext menv'
                  $ try
                  $ sinkProcessStderrStdout cmd args logLines logLines

            case result of
                Right ((), ()) -> return ()
                Left ex -> do
                    logError (displayShow (ex :: ProcessException))
                    prettyError $
                        hang 2
                          ("Error encountered while" <+> step <+> "GHC with" <> line <>
                           styleShell (fromString (unwords (cmd : args))) <> line <>
                           -- TODO: Figure out how to insert \ in the appropriate spots
                           -- hang 2 (shellColor (fillSep (fromString cmd : map fromString args))) <> line <>
                           "run in " <> display wd) <> line <> line <>
                        "The following directories may now contain files, but won't be used by stack:" <> line <>
                        "  -" <+> display tempDir <> line <>
                        "  -" <+> display destDir <> line
                    liftIO exitFailure

    logSticky $
      "Unpacking GHC into " <>
      fromString (toFilePath tempDir) <>
      " ..."
    logDebug $ "Unpacking " <> fromString (toFilePath archiveFile)
    runStep "unpacking" tempDir mempty tarTool [compOpt : "xf", toFilePath archiveFile]

    logSticky "Configuring GHC ..."
    runStep "configuring" dir
        (gdiConfigureEnv downloadInfo)
        (toFilePath $ dir </> $(mkRelFile "configure"))
        (("--prefix=" ++ toFilePath destDir) : map T.unpack (gdiConfigureOpts downloadInfo))

    logSticky "Installing GHC ..."
    runStep "installing" dir mempty makeTool ["install"]

    logStickyDone $ "Installed GHC."
    logDebug $ "GHC installed to " <> fromString (toFilePath destDir)

installGHCJS :: HasConfig env
             => SetupInfo
             -> Path Abs File
             -> ArchiveType
             -> Path Abs Dir
             -> Path Abs Dir
             -> RIO env ()
installGHCJS si archiveFile archiveType _tempDir destDir = do
    platform <- view platformL
    menv0 <- view processContextL
    -- This ensures that locking is disabled for the invocations of
    -- stack below.
    let removeLockVar = Map.delete "STACK_LOCK"
    menv <- mkProcessContext (removeLockVar (removeHaskellEnvVars (view envVarsL menv0)))
    logDebug $ "menv = " <> displayShow (view envVarsL menv)

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
                    SevenZ -> throwString "Don't know how to deal with .7z files on non-Windows"
            (zipTool, tarTool) <- checkDependencies $ (,)
                <$> checkDependency zipTool'
                <*> checkDependency "tar"
            logDebug $ "ziptool: " <> fromString zipTool
            logDebug $ "tar: " <> fromString tarTool
            return $ do
                liftIO $ ignoringAbsence (removeDirRecur destDir)
                liftIO $ ignoringAbsence (removeDirRecur unpackDir)
                withProcessContext menv $ withWorkingDir (toFilePath destDir) $ readProcessNull tarTool ["xf", toFilePath archiveFile]
                innerDir <- expectSingleUnpackedDir archiveFile destDir
                renameDir innerDir unpackDir

    logSticky $
      "Unpacking GHCJS into " <>
      fromString (toFilePath unpackDir) <>
      " ..."
    logDebug $ "Unpacking " <> fromString (toFilePath archiveFile)
    runUnpack

    logSticky "Setting up GHCJS build environment"
    let stackYaml = unpackDir </> $(mkRelFile "stack.yaml")
        destBinDir = destDir </> $(mkRelDir "bin")
    ensureDir destBinDir
    envConfig' <- loadGhcjsEnvConfig stackYaml destBinDir

    -- On windows we need to copy options files out of the install dir.  Argh!
    -- This is done before the build, so that if it fails, things fail
    -- earlier.
    mwindowsInstallDir <- case platform of
        Platform _ Cabal.Windows ->
            liftM Just $ runRIO envConfig' installationRootLocal
        _ -> return Nothing

    logSticky "Installing GHCJS (this will take a long time) ..."
    buildInGhcjsEnv envConfig' defaultBuildOptsCLI
    -- Copy over *.options files needed on windows.
    forM_ mwindowsInstallDir $ \dir -> do
        (_, files) <- listDir (dir </> $(mkRelDir "bin"))
        forM_ (filter ((".options" `isSuffixOf`). toFilePath) files) $ \optionsFile -> do
            let dest = destDir </> $(mkRelDir "bin") </> filename optionsFile
            liftIO $ ignoringAbsence (removeFile dest)
            copyFile optionsFile dest
    logStickyDone "Installed GHCJS."

ensureGhcjsBooted :: HasConfig env
                  => CompilerVersion 'CVActual -> Bool -> [String]
                  -> RIO env ()
ensureGhcjsBooted cv shouldBoot bootOpts = do
    eres <- try $ sinkProcessStdout "ghcjs" [] (return ())
    case eres of
        Right () -> return ()
        Left ece | "no input files" `S.isInfixOf` LBS.toStrict (eceStderr ece) ->
            return ()
        Left ece | "ghcjs_boot.completed" `S.isInfixOf` LBS.toStrict (eceStderr ece) ->
            if not shouldBoot then throwM GHCJSNotBooted else do
                config <- view configL
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
                        _ -> error "ensureGhcjsBooted invoked on non GhcjsVersion"
                actualStackYaml <- if stackYamlExists then return stackYaml
                    else
                        liftM ((destDir </> $(mkRelDir "src")) </>) $
                        parseRelFile $ "ghcjs-" ++ versionString ghcjsVersion ++ "/stack.yaml"
                actualStackYamlExists <- doesFileExist actualStackYaml
                unless actualStackYamlExists $
                    throwString "Error: Couldn't find GHCJS stack.yaml in old or new location."
                bootGhcjs ghcjsVersion actualStackYaml destDir bootOpts
        Left ece -> throwIO ece

bootGhcjs :: (HasRunner env, HasProcessContext env)
          => Version -> Path Abs File -> Path Abs Dir -> [String] -> RIO env ()
bootGhcjs ghcjsVersion stackYaml destDir bootOpts = do
    envConfig <- loadGhcjsEnvConfig stackYaml (destDir </> $(mkRelDir "bin"))
    menv <- liftIO $ configProcessContextSettings (view configL envConfig) defaultEnvSettings
    -- Install cabal-install if missing, or if the installed one is old.
    mcabal <- withProcessContext menv getCabalInstallVersion
    shouldInstallCabal <- case mcabal of
        Nothing -> do
            logInfo "No cabal-install binary found for use with GHCJS."
            return True
        Just v
            | v < $(mkVersion "1.22.4") -> do
                logInfo $
                    "The cabal-install found on PATH is too old to be used for booting GHCJS (version " <>
                    RIO.display v <>
                    ")."
                return True
            | v >= $(mkVersion "1.23") -> do
                logWarn $
                    "The cabal-install found on PATH is a version stack doesn't know about, version " <>
                    RIO.display v <>
                    ". This may or may not work.\n" <>
                    "See this issue: https://github.com/ghcjs/ghcjs/issues/470"
                return False
            | ghcjsVersion >= $(mkVersion "0.2.0.20160413") && v >= $(mkVersion "1.22.8") -> do
                logWarn $
                    "The cabal-install found on PATH, version " <>
                    RIO.display v <>
                    ", is >= 1.22.8.\n" <>
                    "That version has a bug preventing ghcjs < 0.2.0.20160413 from booting.\n" <>
                    "See this issue: https://github.com/ghcjs/ghcjs/issues/470"
                return True
            | otherwise -> return False
    let envSettings = EnvSettings
          { esIncludeLocals = True
          , esIncludeGhcPackagePath = False
          , esStackExe = True
          , esLocaleUtf8 = True
          , esKeepGhcRts = False
          }
    menv' <- liftIO $ configProcessContextSettings (view configL envConfig) envSettings
    shouldInstallAlex <- runRIO menv $ not <$> doesExecutableExist "alex"
    shouldInstallHappy <- runRIO menv $ not <$> doesExecutableExist "happy"
    let bootDepsToInstall =
          [ "cabal-install" | shouldInstallCabal ] ++
          [ "alex" | shouldInstallAlex ] ++
          [ "happy" | shouldInstallHappy ]
    when (not (null bootDepsToInstall)) $ do
        logInfo $ "Building tools from source, needed for ghcjs-boot: " <> displayShow bootDepsToInstall
        buildInGhcjsEnv envConfig $ defaultBuildOptsCLI { boptsCLITargets = bootDepsToInstall }
        let failedToFindErr = do
                logError "This shouldn't happen, because it gets built to the snapshot bin directory, which should be treated as being on the PATH."
                liftIO exitFailure
        when shouldInstallCabal $ do
            mcabal' <- withProcessContext menv' getCabalInstallVersion
            case mcabal' of
                Nothing -> do
                    logError "Failed to get cabal-install version after installing it."
                    failedToFindErr
                Just v | v >= $(mkVersion "1.22.8") && v < $(mkVersion "1.23") ->
                    logWarn $
                        "Installed version of cabal-install is in a version range which may not work.\n" <>
                        "See this issue: https://github.com/ghcjs/ghcjs/issues/470\n" <>
                        "This version is specified by the stack.yaml file included in the ghcjs tarball.\n"
                _ -> return ()
        when shouldInstallAlex $ do
            alexInstalled <- runRIO menv $ doesExecutableExist "alex"
            when (not alexInstalled) $ do
                logError "Failed to find 'alex' executable after installing it."
                failedToFindErr
        when shouldInstallHappy $ do
            happyInstalled <- runRIO menv $ doesExecutableExist "happy"
            when (not happyInstalled) $ do
                logError "Failed to find 'happy' executable after installing it."
                failedToFindErr
    logSticky "Booting GHCJS (this will take a long time) ..."
    withProcessContext menv' $ proc "ghcjs-boot" bootOpts logProcessStderrStdout
    logStickyDone "GHCJS booted."

loadGhcjsEnvConfig :: HasRunner env
                   => Path Abs File -> Path b t -> RIO env EnvConfig
loadGhcjsEnvConfig stackYaml binPath = do
    lc <- loadConfig
        (mempty
            { configMonoidInstallGHC = First (Just True)
            , configMonoidLocalBinPath = First (Just (toFilePath binPath))
            })
        Nothing
        (SYLOverride stackYaml)
    bconfig <- liftIO $ lcLoadBuildConfig lc Nothing
    runRIO bconfig $ setupEnv Nothing

buildInGhcjsEnv :: (HasEnvConfig env, MonadIO m) => env -> BuildOptsCLI -> m ()
buildInGhcjsEnv envConfig boptsCli = do
    runRIO (set (buildOptsL.buildOptsInstallExesL) True $
            set (buildOptsL.buildOptsHaddockL) False envConfig) $
        build (\_ -> return ()) Nothing boptsCli

getCabalInstallVersion :: (HasProcessContext env, HasLogFunc env) => RIO env (Maybe Version)
getCabalInstallVersion = do
    ebs <- proc "cabal" ["--numeric-version"] $ tryAny . readProcessStdout_
    case ebs of
        Left _ -> return Nothing
        Right bs -> Just <$> parseVersion (T.dropWhileEnd isSpace (T.decodeUtf8 (LBS.toStrict bs)))

-- | Check if given processes appear to be present, throwing an exception if
-- missing.
checkDependencies :: CheckDependency env a -> RIO env a
checkDependencies (CheckDependency f) = f >>= either (throwIO . MissingDependencies) return

checkDependency :: HasProcessContext env => String -> CheckDependency env String
checkDependency tool = CheckDependency $ do
    exists <- doesExecutableExist tool
    return $ if exists then Right tool else Left [tool]

newtype CheckDependency env a = CheckDependency (RIO env (Either [String] a))
    deriving Functor
instance Applicative (CheckDependency env) where
    pure x = CheckDependency $ return (Right x)
    CheckDependency f <*> CheckDependency x = CheckDependency $ do
        f' <- f
        x' <- x
        return $
            case (f', x') of
                (Left e1, Left e2) -> Left $ e1 ++ e2
                (Left e, Right _) -> Left e
                (Right _, Left e) -> Left e
                (Right f'', Right x'') -> Right $ f'' x''
instance Alternative (CheckDependency env) where
    empty = CheckDependency $ return $ Left []
    CheckDependency x <|> CheckDependency y = CheckDependency $ do
        res1 <- x
        case res1 of
            Left _ -> y
            Right x' -> return $ Right x'

installGHCWindows :: HasConfig env
                  => Version
                  -> SetupInfo
                  -> Path Abs File
                  -> ArchiveType
                  -> Path Abs Dir
                  -> Path Abs Dir
                  -> RIO env ()
installGHCWindows version si archiveFile archiveType _tempDir destDir = do
    tarComponent <- parseRelDir $ "ghc-" ++ versionString version
    withUnpackedTarball7z "GHC" si archiveFile archiveType (Just tarComponent) destDir
    logInfo $ "GHC installed to " <> fromString (toFilePath destDir)

installMsys2Windows :: HasConfig env
                  => Text -- ^ OS Key
                  -> SetupInfo
                  -> Path Abs File
                  -> ArchiveType
                  -> Path Abs Dir
                  -> Path Abs Dir
                  -> RIO env ()
installMsys2Windows osKey si archiveFile archiveType _tempDir destDir = do
    exists <- liftIO $ D.doesDirectoryExist $ toFilePath destDir
    when exists $ liftIO (D.removeDirectoryRecursive $ toFilePath destDir) `catchIO` \e -> do
        logError $
            "Could not delete existing msys directory: " <>
            fromString (toFilePath destDir)
        throwM e

    msys <- parseRelDir $ "msys" ++ T.unpack (fromMaybe "32" $ T.stripPrefix "windows" osKey)
    withUnpackedTarball7z "MSYS2" si archiveFile archiveType (Just msys) destDir


    -- I couldn't find this officially documented anywhere, but you need to run
    -- the MSYS shell once in order to initialize some pacman stuff. Once that
    -- run happens, you can just run commands as usual.
    menv0 <- view processContextL
    newEnv0 <- modifyEnvVars menv0 $ Map.insert "MSYSTEM" "MSYS"
    newEnv <- either throwM return $ augmentPathMap
                  [toFilePath $ destDir </> $(mkRelDir "usr") </> $(mkRelDir "bin")]
                  (view envVarsL newEnv0)
    menv <- mkProcessContext newEnv
    withWorkingDir (toFilePath destDir) $ withProcessContext menv
      $ proc "sh" ["--login", "-c", "true"] runProcess_

    -- No longer installing git, it's unreliable
    -- (https://github.com/commercialhaskell/stack/issues/1046) and the
    -- MSYS2-installed version has bad CRLF defaults.
    --
    -- Install git. We could install other useful things in the future too.
    -- runCmd (Cmd (Just destDir) "pacman" menv ["-Sy", "--noconfirm", "git"]) Nothing

-- | Unpack a compressed tarball using 7zip.  Expects a single directory in
-- the unpacked results, which is renamed to the destination directory.
withUnpackedTarball7z :: HasConfig env
                      => String -- ^ Name of tool, used in error messages
                      -> SetupInfo
                      -> Path Abs File -- ^ Path to archive file
                      -> ArchiveType
                      -> Maybe (Path Rel Dir) -- ^ Name of directory expected in archive.  If Nothing, expects a single folder.
                      -> Path Abs Dir -- ^ Destination directory.
                      -> RIO env ()
withUnpackedTarball7z name si archiveFile archiveType msrcDir destDir = do
    suffix <-
        case archiveType of
            TarXz -> return ".xz"
            TarBz2 -> return ".bz2"
            TarGz -> return ".gz"
            _ -> throwString $ name ++ " must be a tarball file"
    tarFile <-
        case T.stripSuffix suffix $ T.pack $ toFilePath (filename archiveFile) of
            Nothing -> throwString $ "Invalid " ++ name ++ " filename: " ++ show archiveFile
            Just x -> parseRelFile $ T.unpack x
    run7z <- setup7z si
    let tmpName = toFilePathNoTrailingSep (dirname destDir) ++ "-tmp"
    ensureDir (parent destDir)
    withRunInIO $ \run -> withTempDir (parent destDir) tmpName $ \tmpDir -> run $ do
        liftIO $ ignoringAbsence (removeDirRecur destDir)
        run7z tmpDir archiveFile
        run7z tmpDir (tmpDir </> tarFile)
        absSrcDir <- case msrcDir of
            Just srcDir -> return $ tmpDir </> srcDir
            Nothing -> expectSingleUnpackedDir archiveFile tmpDir
        renameDir absSrcDir destDir

expectSingleUnpackedDir :: (MonadIO m, MonadThrow m) => Path Abs File -> Path Abs Dir -> m (Path Abs Dir)
expectSingleUnpackedDir archiveFile destDir = do
    contents <- listDir destDir
    case contents of
        ([dir], _ ) -> return dir
        _ -> throwString $ "Expected a single directory within unpacked " ++ toFilePath archiveFile

-- | Download 7z as necessary, and get a function for unpacking things.
--
-- Returned function takes an unpack directory and archive.
setup7z :: (HasConfig env, MonadIO m)
        => SetupInfo
        -> RIO env (Path Abs Dir -> Path Abs File -> m ())
setup7z si = do
    dir <- view $ configL.to configLocalPrograms
    ensureDir dir
    let exe = dir </> $(mkRelFile "7z.exe")
        dll = dir </> $(mkRelFile "7z.dll")
    case (siSevenzDll si, siSevenzExe si) of
        (Just sevenzDll, Just sevenzExe) -> do
            chattyDownload "7z.dll" sevenzDll dll
            chattyDownload "7z.exe" sevenzExe exe
            withRunInIO $ \run -> return $ \outdir archive -> liftIO $ run $ do
                let cmd = toFilePath exe
                    args =
                        [ "x"
                        , "-o" ++ toFilePath outdir
                        , "-y"
                        , toFilePath archive
                        ]
                ec <- proc cmd args runProcess
                when (ec /= ExitSuccess)
                    $ liftIO $ throwM (ProblemWhileDecompressing archive)
        _ -> throwM SetupInfoMissingSevenz

chattyDownload :: HasRunner env
               => Text          -- ^ label
               -> DownloadInfo  -- ^ URL, content-length, sha1, and sha256
               -> Path Abs File -- ^ destination
               -> RIO env ()
chattyDownload label downloadInfo path = do
    let url = downloadInfoUrl downloadInfo
    req <- parseUrlThrow $ T.unpack url
    logSticky $
      "Preparing to download " <>
      RIO.display label <>
      " ..."
    logDebug $
      "Downloading from " <>
      RIO.display url <>
      " to " <>
      fromString (toFilePath path) <>
      " ..."
    hashChecks <- fmap catMaybes $ forM
      [ ("sha1",   HashCheck SHA1,   downloadInfoSha1)
      , ("sha256", HashCheck SHA256, downloadInfoSha256)
      ]
      $ \(name, constr, getter) ->
        case getter downloadInfo of
          Just bs -> do
            logDebug $
                "Will check against " <>
                name <>
                " hash: " <>
                displayBytesUtf8 bs
            return $ Just $ constr $ CheckHexDigestByteString bs
          Nothing -> return Nothing
    when (null hashChecks) $ logWarn $
        "No sha1 or sha256 found in metadata," <>
        " download hash won't be checked."
    let dReq = DownloadRequest
            { drRequest = req
            , drHashChecks = hashChecks
            , drLengthCheck = mtotalSize
            , drRetryPolicy = drRetryPolicyDefault
            }
    x <- verifiedDownload dReq path chattyDownloadProgress
    if x
        then logStickyDone ("Downloaded " <> RIO.display label <> ".")
        else logStickyDone "Already downloaded."
  where
    mtotalSize = downloadInfoContentLength downloadInfo
    chattyDownloadProgress _ = do
        _ <- logSticky $ RIO.display label <> ": download has begun"
        CL.map (Sum . S.length)
          .| chunksOverTime 1
          .| go
      where
        go = evalStateC 0 $ awaitForever $ \(Sum size) -> do
            modify (+ size)
            totalSoFar <- get
            logSticky $ fromString $
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
chunksOverTime :: (Monoid a, Semigroup a, MonadIO m) => NominalDiffTime -> ConduitM a a m ()
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
sanityCheck :: (HasProcessContext env, HasLogFunc env)
            => WhichCompiler
            -> RIO env ()
sanityCheck wc = withSystemTempDir "stack-sanity-check" $ \dir -> do
    let fp = toFilePath $ dir </> $(mkRelFile "Main.hs")
    liftIO $ S.writeFile fp $ T.encodeUtf8 $ T.pack $ unlines
        [ "import Distribution.Simple" -- ensure Cabal library is present
        , "main = putStrLn \"Hello World\""
        ]
    let exeName = compilerExeName wc
    ghc <- findExecutable exeName >>= either throwM parseAbsFile
    logDebug $ "Performing a sanity check on: " <> fromString (toFilePath ghc)
    eres <- withWorkingDir (toFilePath dir) $ proc exeName
        [ fp
        , "-no-user-package-db"
        ] $ try . readProcessStdout_
    case eres of
        Left e -> throwIO $ GHCSanityCheckCompileFailed e ghc
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
    Map.delete "DESTDIR" .
    -- https://github.com/commercialhaskell/stack/issues/3444
    Map.delete "GHCRTS"

-- | Get map of environment variables to set to change the GHC's encoding to UTF-8
getUtf8EnvVars
    :: (HasProcessContext env, HasPlatform env, HasLogFunc env)
    => CompilerVersion 'CVActual
    -> RIO env (Map Text Text)
getUtf8EnvVars compilerVer =
    if getGhcVersion compilerVer >= $(mkVersion "7.10.3")
        -- GHC_CHARENC supported by GHC >=7.10.3
        then return $ Map.singleton "GHC_CHARENC" "UTF-8"
        else legacyLocale
  where
    legacyLocale = do
        menv <- view processContextL
        Platform _ os <- view platformL
        if os == Cabal.Windows
            then
                 -- On Windows, locale is controlled by the code page, so we don't set any environment
                 -- variables.
                 return
                     Map.empty
            else do
                let checkedVars = map checkVar (Map.toList $ view envVarsL menv)
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
                        elocales <- tryAny $ proc "locale" ["-a"] readProcessStdout_
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
                                                 T.lenientDecode $
                                                 LBS.toStrict locales)
                            mfallback = getFallbackLocale utf8Locales
                        when
                            (isNothing mfallback)
                            (logWarn
                                 "Warning: unable to set locale to UTF-8 encoding; GHC may fail with 'invalid character'")
                        let
                            -- Get the new values of variables to adjust.
                            changes =
                                Map.unions $
                                map
                                    (adjustedVarValue menv utf8Locales mfallback)
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
        :: ProcessContext -> [Text] -> Maybe Text -> Text -> Map Text Text
    adjustedVarValue menv utf8Locales mfallback k =
        case Map.lookup k (view envVarsL menv) of
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

-- Binary Stack upgrades

newtype StackReleaseInfo = StackReleaseInfo Value

downloadStackReleaseInfo :: (MonadIO m, MonadThrow m)
                         => Maybe String -- Github org
                         -> Maybe String -- Github repo
                         -> Maybe String -- ^ optional version
                         -> m StackReleaseInfo
downloadStackReleaseInfo morg mrepo mver = liftIO $ do
    let org = fromMaybe "commercialhaskell" morg
        repo = fromMaybe "stack" mrepo
    let url = concat
            [ "https://api.github.com/repos/"
            , org
            , "/"
            , repo
            , "/releases/"
            , case mver of
                Nothing -> "latest"
                Just ver -> "tags/v" ++ ver
            ]
    req <- parseRequest url
    res <- httpJSON $ setGithubHeaders req
    let code = getResponseStatusCode res
    if code >= 200 && code < 300
        then return $ StackReleaseInfo $ getResponseBody res
        else throwString $ "Could not get release information for Stack from: " ++ url

preferredPlatforms :: (MonadReader env m, HasPlatform env, MonadThrow m)
                   => m [(Bool, String)]
preferredPlatforms = do
    Platform arch' os' <- view platformL
    (isWindows, os) <-
      case os' of
        Cabal.Linux -> return (False, "linux")
        Cabal.Windows -> return (True, "windows")
        Cabal.OSX -> return (False, "osx")
        Cabal.FreeBSD -> return (False, "freebsd")
        _ -> throwM $ stringException $ "Binary upgrade not yet supported on OS: " ++ show os'
    arch <-
      case arch' of
        I386 -> return "i386"
        X86_64 -> return "x86_64"
        Arm -> return "arm"
        _ -> throwM $ stringException $ "Binary upgrade not yet supported on arch: " ++ show arch'
    hasgmp4 <- return False -- FIXME import relevant code from Stack.Setup? checkLib $(mkRelFile "libgmp.so.3")
    let suffixes
          | hasgmp4 = ["-static", "-gmp4", ""]
          | otherwise = ["-static", ""]
    return $ map (\suffix -> (isWindows, concat [os, "-", arch, suffix])) suffixes

downloadStackExe
    :: HasConfig env
    => [(Bool, String)] -- ^ acceptable platforms
    -> StackReleaseInfo
    -> Path Abs Dir -- ^ destination directory
    -> Bool -- ^ perform PATH-aware checking, see #3232
    -> (Path Abs File -> IO ()) -- ^ test the temp exe before renaming
    -> RIO env ()
downloadStackExe platforms0 archiveInfo destDir checkPath testExe = do
    (isWindows, archiveURL) <-
      let loop [] = throwString $ "Unable to find binary Stack archive for platforms: "
                                ++ unwords (map snd platforms0)
          loop ((isWindows, p'):ps) = do
            let p = T.pack p'
            logInfo $ "Querying for archive location for platform: " <> fromString p'
            case findArchive archiveInfo p of
              Just x -> return (isWindows, x)
              Nothing -> loop ps
       in loop platforms0

    let (destFile, tmpFile)
            | isWindows =
                ( destDir </> $(mkRelFile "stack.exe")
                , destDir </> $(mkRelFile "stack.tmp.exe")
                )
            | otherwise =
                ( destDir </> $(mkRelFile "stack")
                , destDir </> $(mkRelFile "stack.tmp")
                )

    logInfo $ "Downloading from: " <> RIO.display archiveURL

    liftIO $ do
      case () of
        ()
          | ".tar.gz" `T.isSuffixOf` archiveURL -> handleTarball tmpFile isWindows archiveURL
          | ".zip" `T.isSuffixOf` archiveURL -> error "FIXME: Handle zip files"
          | otherwise -> error $ "Unknown archive format for Stack archive: " ++ T.unpack archiveURL

    logInfo "Download complete, testing executable"

    platform <- view platformL

    liftIO $ do
#if !WINDOWS
      setFileMode (toFilePath tmpFile) 0o755
#endif

      testExe tmpFile

      currExe <- getExecutablePath
      case platform of
          Platform _ Cabal.Windows | FP.equalFilePath (toFilePath destFile) currExe -> do
              old <- parseAbsFile (toFilePath destFile ++ ".old")
              renameFile destFile old
              renameFile tmpFile destFile
          _ -> renameFile tmpFile destFile

    destDir' <- liftIO . D.canonicalizePath . toFilePath $ destDir
    warnInstallSearchPathIssues destDir' ["stack"]

    logInfo $ "New stack executable available at " <> fromString (toFilePath destFile)

    when checkPath $ performPathChecking destFile
      `catchAny` (logError . displayShow)
  where

    findArchive (StackReleaseInfo val) pattern = do
        Object top <- return val
        Array assets <- HashMap.lookup "assets" top
        getFirst $ fold $ fmap (First . findMatch pattern') assets
      where
        pattern' = mconcat ["-", pattern, "."]

        findMatch pattern'' (Object o) = do
            String name <- HashMap.lookup "name" o
            guard $ not $ ".asc" `T.isSuffixOf` name
            guard $ pattern'' `T.isInfixOf` name
            String url <- HashMap.lookup "browser_download_url" o
            Just url
        findMatch _ _ = Nothing

    handleTarball :: Path Abs File -> Bool -> T.Text -> IO ()
    handleTarball tmpFile isWindows url = do
        req <- fmap setGithubHeaders $ parseUrlThrow $ T.unpack url
        withResponse req $ \res -> do
            entries <- fmap (Tar.read . LBS.fromChunks)
                     $ lazyConsume
                     $ getResponseBody res .| ungzip
            let loop Tar.Done = error $ concat
                    [ "Stack executable "
                    , show exeName
                    , " not found in archive from "
                    , T.unpack url
                    ]
                loop (Tar.Fail e) = throwM e
                loop (Tar.Next e es)
                    | Tar.entryPath e == exeName =
                        case Tar.entryContent e of
                            Tar.NormalFile lbs _ -> do
                              ensureDir destDir
                              LBS.writeFile (toFilePath tmpFile) lbs
                            _ -> error $ concat
                                [ "Invalid file type for tar entry named "
                                , exeName
                                , " downloaded from "
                                , T.unpack url
                                ]
                    | otherwise = loop es
            loop entries
      where
        -- The takeBaseName drops the .gz, dropExtension drops the .tar
        exeName =
            let base = FP.dropExtension (FP.takeBaseName (T.unpack url)) FP.</> "stack"
             in if isWindows then base FP.<.> "exe" else base

-- | Ensure that the Stack executable download is in the same location
-- as the currently running executable. See:
-- https://github.com/commercialhaskell/stack/issues/3232
performPathChecking
    :: HasConfig env
    => Path Abs File -- ^ location of the newly downloaded file
    -> RIO env ()
performPathChecking newFile = do
  executablePath <- liftIO getExecutablePath
  executablePath' <- parseAbsFile executablePath
  unless (toFilePath newFile == executablePath) $ do
    logInfo $ "Also copying stack executable to " <> fromString executablePath
    tmpFile <- parseAbsFile $ executablePath ++ ".tmp"
    eres <- tryIO $ do
      liftIO $ copyFile newFile tmpFile
#if !WINDOWS
      liftIO $ setFileMode (toFilePath tmpFile) 0o755
#endif
      liftIO $ renameFile tmpFile executablePath'
      logInfo "Stack executable copied successfully!"
    case eres of
      Right () -> return ()
      Left e
        | isPermissionError e -> do
            logWarn $ "Permission error when trying to copy: " <> displayShow e
            logWarn "Should I try to perform the file copy using sudo? This may fail"
            toSudo <- prompt "Try using sudo? (y/n) "
            when toSudo $ do
              let run cmd args = do
                    ec <- proc cmd args runProcess
                    when (ec /= ExitSuccess) $ error $ concat
                          [ "Process exited with "
                          , show ec
                          , ": "
                          , unwords (cmd:args)
                          ]
                  commands =
                    [ ("sudo",
                        [ "cp"
                        , toFilePath newFile
                        , toFilePath tmpFile
                        ])
                    , ("sudo",
                        [ "mv"
                        , toFilePath tmpFile
                        , executablePath
                        ])
                    ]
              logInfo "Going to run the following commands:"
              logInfo ""
              forM_ commands $ \(cmd, args) ->
                logInfo $ "-  " <> mconcat (intersperse " " (fromString <$> (cmd:args)))
              mapM_ (uncurry run) commands
              logInfo ""
              logInfo "sudo file copy worked!"
        | otherwise -> throwM e

prompt :: MonadIO m => String -> m Bool
prompt str =
    liftIO go
  where
    go = do
      putStr str
      hFlush stdout
      l <- getLine
      case l of
        'y':_ -> return True
        'n':_ -> return False
        _ -> putStrLn "Invalid entry, try again" >> go

getDownloadVersion :: StackReleaseInfo -> Maybe Version
getDownloadVersion (StackReleaseInfo val) = do
    Object o <- Just val
    String rawName <- HashMap.lookup "name" o
    -- drop the "v" at the beginning of the name
    parseVersion $ T.drop 1 rawName
