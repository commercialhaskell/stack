{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}

module Stack.Setup
  ( setupEnv
  , ensureCompilerAndMsys
  , ensureDockerStackExe
  , SetupOpts (..)
  , defaultSetupInfoYaml
  , withNewLocalBuildTargets

  -- * Stack binary download
  , StackReleaseInfo
  , getDownloadVersion
  , stackVersion
  , preferredPlatforms
  , downloadStackReleaseInfo
  , downloadStackExe
  ) where

import qualified    Codec.Archive.Tar as Tar
import              Conduit
import              Control.Applicative (empty)
import "cryptonite" Crypto.Hash (SHA1(..), SHA256(..))
import              Pantry.Internal.AesonExtended
import qualified    Data.ByteString as S
import qualified    Data.ByteString.Lazy as LBS
import qualified    Data.Conduit.Binary as CB
import              Data.Conduit.Lazy (lazyConsume)
import qualified    Data.Conduit.List as CL
import              Data.Conduit.Process.Typed (createSource)
import              Data.Conduit.Zlib          (ungzip)
import              Data.Foldable (maximumBy)
import qualified    Data.HashMap.Strict as HashMap
import              Data.List hiding (concat, elem, maximumBy, any)
import qualified    Data.Map as Map
import qualified    Data.Set as Set
import qualified    Data.Text as T
import qualified    Data.Text.Encoding as T
import qualified    Data.Text.Encoding.Error as T
import qualified    Data.Yaml as Yaml
import              Distribution.System (OS, Arch (..), Platform (..))
import qualified    Distribution.System as Cabal
import              Distribution.Text (simpleParse)
import              Distribution.Types.PackageName (mkPackageName)
import              Distribution.Version (mkVersion)
import              Network.HTTP.Client (redirectCount)
import              Network.HTTP.StackClient (CheckHexDigest (..), HashCheck (..),
                                              getResponseBody, getResponseStatusCode, httpLbs, httpJSON,
                                              mkDownloadRequest, parseRequest, parseUrlThrow, setGithubHeaders,
                                              setHashChecks, setLengthCheck, verifiedDownloadWithProgress, withResponse,
                                              setRequestMethod)
import              Network.HTTP.Simple (getResponseHeader)
import              Path hiding (fileExtension)
import              Path.CheckInstall (warnInstallSearchPathIssues)
import              Path.Extended (fileExtension)
import              Path.Extra (toFilePathNoTrailingSep)
import              Path.IO hiding (findExecutable, withSystemTempDir)
import qualified    Pantry
import qualified    RIO
import              RIO.List
import              RIO.PrettyPrint
import              RIO.Process
import              Stack.Build.Haddock (shouldHaddockDeps)
import              Stack.Build.Source (loadSourceMap, hashSourceMapData)
import              Stack.Build.Target (NeedTargets(..), parseTargets)
import              Stack.Constants
import              Stack.Constants.Config (distRelativeDir)
import              Stack.GhcPkg (createDatabase, getGlobalDB, mkGhcPackagePath, ghcPkgPathEnvVar)
import              Stack.Prelude hiding (Display (..))
import              Stack.SourceMap
import              Stack.Setup.Installed
import              Stack.Storage.User (loadCompilerPaths, saveCompilerPaths)
import              Stack.Types.Build
import              Stack.Types.Compiler
import              Stack.Types.CompilerBuild
import              Stack.Types.Config
import              Stack.Types.Docker
import              Stack.Types.SourceMap
import              Stack.Types.Version
import qualified    System.Directory as D
import              System.Environment (getExecutablePath, lookupEnv)
import              System.IO.Error (isPermissionError)
import              System.FilePath (searchPathSeparator)
import qualified    System.FilePath as FP
import              System.Permissions (setFileExecutable)
import              System.Uname (getRelease)
import              Data.List.Split (splitOn)

-- | Default location of the stack-setup.yaml file
defaultSetupInfoYaml :: String
defaultSetupInfoYaml =
    "https://raw.githubusercontent.com/fpco/stackage-content/master/stack/stack-setup-2.yaml"

data SetupOpts = SetupOpts
    { soptsInstallIfMissing :: !Bool
    , soptsUseSystem :: !Bool
    -- ^ Should we use a system compiler installation, if available?
    , soptsWantedCompiler :: !WantedCompiler
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
    , soptsResolveMissingGHC :: !(Maybe Text)
    -- ^ Message shown to user for how to resolve the missing GHC
    , soptsGHCBindistURL :: !(Maybe String)
    -- ^ Alternate GHC binary distribution (requires custom GHCVariant)
    }
    deriving Show
data SetupException = UnsupportedSetupCombo OS Arch
                    | MissingDependencies [String]
                    | UnknownCompilerVersion (Set.Set Text) WantedCompiler (Set.Set ActualCompiler)
                    | UnknownOSKey Text
                    | GHCSanityCheckCompileFailed SomeException (Path Abs File)
                    | WantedMustBeGHC
                    | RequireCustomGHCVariant
                    | ProblemWhileDecompressing (Path Abs File)
                    | SetupInfoMissingSevenz
                    | DockerStackExeNotFound Version Text
                    | UnsupportedSetupConfiguration
                    | InvalidGhcAt (Path Abs File) SomeException
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
        , T.unpack $ utf8BuilderToText $ RIO.display wanted
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
    show (InvalidGhcAt compiler e) =
        "Found an invalid compiler at " ++ show (toFilePath compiler) ++ ": " ++ displayException e

-- | Modify the environment variables (like PATH) appropriately, possibly doing installation too
setupEnv :: NeedTargets
         -> BuildOptsCLI
         -> Maybe Text -- ^ Message to give user when necessary GHC is not available
         -> RIO BuildConfig EnvConfig
setupEnv needTargets boptsCLI mResolveMissingGHC = do
    config <- view configL
    bc <- view buildConfigL
    let stackYaml = bcStackYaml bc
    platform <- view platformL
    wcVersion <- view wantedCompilerVersionL
    wanted <- view wantedCompilerVersionL
    actual <- either throwIO pure $ wantedToActual wanted
    let wc = actual^.whichCompilerL
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
            , soptsResolveMissingGHC = mResolveMissingGHC
            , soptsGHCBindistURL = Nothing
            }

    (compilerPaths, ghcBin) <- ensureCompilerAndMsys sopts
    let compilerVer = cpCompilerVersion compilerPaths

    -- Modify the initial environment to include the GHC path, if a local GHC
    -- is being used
    menv0 <- view processContextL
    env <- either throwM (return . removeHaskellEnvVars)
               $ augmentPathMap
                    (map toFilePath $ edBins ghcBin)
                    (view envVarsL menv0)
    menv <- mkProcessContext env

    logDebug "Resolving package entries"

    (sourceMap, sourceMapHash) <- runWithGHC menv compilerPaths $ do
      smActual <- actualFromGhc (bcSMWanted bc) compilerVer
      let actualPkgs = Map.keysSet (smaDeps smActual) <>
                       Map.keysSet (smaProject smActual)
          prunedActual = smActual { smaGlobal = pruneGlobals (smaGlobal smActual) actualPkgs }
          haddockDeps = shouldHaddockDeps (configBuild config)
      targets <- parseTargets needTargets haddockDeps boptsCLI prunedActual
      sourceMap <- loadSourceMap targets boptsCLI smActual
      sourceMapHash <- hashSourceMapData boptsCLI sourceMap
      pure (sourceMap, sourceMapHash)

    let envConfig0 = EnvConfig
            { envConfigBuildConfig = bc
            , envConfigBuildOptsCLI = boptsCLI
            , envConfigSourceMap = sourceMap
            , envConfigSourceMapHash = sourceMapHash
            , envConfigCompilerPaths = compilerPaths
            }

    -- extra installation bin directories
    mkDirs <- runRIO envConfig0 extraBinDirs
    let mpath = Map.lookup "PATH" env
    depsPath <- either throwM return $ augmentPath (toFilePath <$> mkDirs False) mpath
    localsPath <- either throwM return $ augmentPath (toFilePath <$> mkDirs True) mpath

    deps <- runRIO envConfig0 packageDatabaseDeps
    runWithGHC menv compilerPaths $ createDatabase (cpPkg compilerPaths) deps
    localdb <- runRIO envConfig0 packageDatabaseLocal
    runWithGHC menv compilerPaths $ createDatabase (cpPkg compilerPaths) localdb
    extras <- runReaderT packageDatabaseExtra envConfig0
    let mkGPP locals = mkGhcPackagePath locals localdb deps extras $ cpGlobalDB compilerPaths

    distDir <- runReaderT distRelativeDir envConfig0 >>= canonicalizePath

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
                        $ Map.insert "HASKELL_DIST_DIR" (T.pack $ toFilePathNoTrailingSep distDir)

                          -- Make sure that any .ghc.environment files
                          -- are ignored, since we're settting up our
                          -- own package databases. See
                          -- https://github.com/commercialhaskell/stack/issues/4706
                        $ (case cpCompilerVersion compilerPaths of
                             ACGhc version | version >= mkVersion [8, 4, 4] ->
                               Map.insert "GHC_ENVIRONMENT" "-"
                             _ -> id)

                          env

                    () <- atomicModifyIORef envRef $ \m' ->
                        (Map.insert es eo m', ())
                    return eo

    envOverride <- liftIO $ getProcessContext' minimalEnvSettings
    return EnvConfig
        { envConfigBuildConfig = bc
            { bcConfig = addIncludeLib ghcBin
                       $ set processContextL envOverride
                         (view configL bc)
                { configProcessContextSettings = getProcessContext'
                }
            }
        , envConfigBuildOptsCLI = boptsCLI
        , envConfigSourceMap = sourceMap
        , envConfigSourceMapHash = sourceMapHash
        , envConfigCompilerPaths = compilerPaths
        }

-- | A modified env which we know has an installed compiler on the PATH.
data WithGHC env = WithGHC !CompilerPaths !env

insideL :: Lens' (WithGHC env) env
insideL = lens (\(WithGHC _ x) -> x) (\(WithGHC cp _) -> WithGHC cp)

instance HasLogFunc env => HasLogFunc (WithGHC env) where
  logFuncL = insideL.logFuncL
instance HasRunner env => HasRunner (WithGHC env) where
  runnerL = insideL.runnerL
instance HasProcessContext env => HasProcessContext (WithGHC env) where
  processContextL = insideL.processContextL
instance HasStylesUpdate env => HasStylesUpdate (WithGHC env) where
  stylesUpdateL = insideL.stylesUpdateL
instance HasTerm env => HasTerm (WithGHC env) where
  useColorL = insideL.useColorL
  termWidthL = insideL.termWidthL
instance HasPantryConfig env => HasPantryConfig (WithGHC env) where
  pantryConfigL = insideL.pantryConfigL
instance HasConfig env => HasPlatform (WithGHC env)
instance HasConfig env => HasGHCVariant (WithGHC env)
instance HasConfig env => HasConfig (WithGHC env) where
  configL = insideL.configL
instance HasBuildConfig env => HasBuildConfig (WithGHC env) where
  buildConfigL = insideL.buildConfigL
instance HasCompiler (WithGHC env) where
  compilerPathsL = to (\(WithGHC cp _) -> cp)

-- | Set up a modified environment which includes the modified PATH
-- that GHC can be found on. This is needed for looking up global
-- package information and ghc fingerprint (result from 'ghc --info').
runWithGHC :: HasConfig env => ProcessContext -> CompilerPaths -> RIO (WithGHC env) a -> RIO env a
runWithGHC pc cp inner = do
  env <- ask
  let envg
        = WithGHC cp $
          set envOverrideSettingsL (\_ -> return pc) $
          set processContextL pc env
  runRIO envg inner

-- | special helper for GHCJS which needs an updated source map
-- only project dependencies should get included otherwise source map hash will
-- get changed and EnvConfig will become inconsistent
rebuildEnv :: EnvConfig
    -> NeedTargets
    -> Bool
    -> BuildOptsCLI
    -> RIO env EnvConfig
rebuildEnv envConfig needTargets haddockDeps boptsCLI = do
    let bc = envConfigBuildConfig envConfig
        cp = envConfigCompilerPaths envConfig
        compilerVer = smCompiler $ envConfigSourceMap envConfig
    runRIO (WithGHC cp bc) $ do
        smActual <- actualFromGhc (bcSMWanted bc) compilerVer
        let actualPkgs = Map.keysSet (smaDeps smActual) <> Map.keysSet (smaProject smActual)
            prunedActual = smActual {
              smaGlobal = pruneGlobals (smaGlobal smActual) actualPkgs
              }
        targets <- parseTargets needTargets haddockDeps boptsCLI prunedActual
        sourceMap <- loadSourceMap targets boptsCLI smActual
        return $
            envConfig
            {envConfigSourceMap = sourceMap, envConfigBuildOptsCLI = boptsCLI}

-- | Some commands (script, ghci and exec) set targets dynamically
-- see also the note about only local targets for rebuildEnv
withNewLocalBuildTargets :: HasEnvConfig  env => [Text] -> RIO env a -> RIO env a
withNewLocalBuildTargets targets f = do
    envConfig <- view $ envConfigL
    haddockDeps <- view $ configL.to configBuild.to shouldHaddockDeps
    let boptsCLI = envConfigBuildOptsCLI envConfig
    envConfig' <- rebuildEnv envConfig NeedTargets haddockDeps $
                  boptsCLI {boptsCLITargets = targets}
    local (set envConfigL envConfig') f

-- | Add the include and lib paths to the given Config
addIncludeLib :: ExtraDirs -> Config -> Config
addIncludeLib (ExtraDirs _bins includes libs) config = config
    { configExtraIncludeDirs =
        configExtraIncludeDirs config ++
        map toFilePathNoTrailingSep includes
    , configExtraLibDirs =
        configExtraLibDirs config ++
        map toFilePathNoTrailingSep libs
    }

-- | Ensure both the compiler and the msys toolchain are installed and
-- provide the PATHs to add if necessary
ensureCompilerAndMsys
  :: (HasBuildConfig env, HasGHCVariant env)
  => SetupOpts
  -> RIO env (CompilerPaths, ExtraDirs)
ensureCompilerAndMsys sopts = do
  actual <- either throwIO pure $ wantedToActual $ soptsWantedCompiler sopts
  didWarn <- warnUnsupportedCompiler $ getGhcVersion actual

  getSetupInfo' <- memoizeRef getSetupInfo
  (cp, ghcPaths) <- ensureCompiler sopts getSetupInfo'

  warnUnsupportedCompilerCabal cp didWarn

  mmsys2Tool <- ensureMsys sopts getSetupInfo'
  paths <-
    case mmsys2Tool of
      Nothing -> pure ghcPaths
      Just msys2Tool -> do
        msys2Paths <- extraDirs msys2Tool
        pure $ ghcPaths <> msys2Paths
  pure (cp, paths)

-- | See <https://github.com/commercialhaskell/stack/issues/4246>
warnUnsupportedCompiler :: HasLogFunc env => Version -> RIO env Bool
warnUnsupportedCompiler ghcVersion = do
  if
    | ghcVersion < mkVersion [7, 8] -> do
        logWarn $
          "Stack will almost certainly fail with GHC below version 7.8, requested " <>
          fromString (versionString ghcVersion)
        logWarn "Valiantly attempting to run anyway, but I know this is doomed"
        logWarn "For more information, see: https://github.com/commercialhaskell/stack/issues/648"
        logWarn ""
        pure True
    | ghcVersion >= mkVersion [9, 1] -> do
        logWarn $
          "Stack has not been tested with GHC versions above 9.0, and using " <>
          fromString (versionString ghcVersion) <>
          ", this may fail"
        pure True
    | otherwise -> do
        logDebug "Asking for a supported GHC version"
        pure False

-- | See <https://github.com/commercialhaskell/stack/issues/4246>
warnUnsupportedCompilerCabal
  :: HasLogFunc env
  => CompilerPaths
  -> Bool -- ^ already warned about GHC?
  -> RIO env ()
warnUnsupportedCompilerCabal cp didWarn = do
  unless didWarn $ void $ warnUnsupportedCompiler $ getGhcVersion $ cpCompilerVersion cp
  let cabalVersion = cpCabalVersion cp

  if
    | cabalVersion < mkVersion [1, 19, 2] -> do
        logWarn $ "Stack no longer supports Cabal versions below 1.19.2,"
        logWarn $ "but version " <> fromString (versionString cabalVersion) <> " was found."
        logWarn "This invocation will most likely fail."
        logWarn "To fix this, either use an older version of Stack or a newer resolver"
        logWarn "Acceptable resolvers: lts-3.0/nightly-2015-05-05 or later"
    | cabalVersion >= mkVersion [3, 5] ->
        logWarn $
          "Stack has not been tested with Cabal versions above 3.4, but version " <>
          fromString (versionString cabalVersion) <>
          " was found, this may fail"
    | otherwise -> pure ()

-- | Ensure that the msys toolchain is installed if necessary and
-- provide the PATHs to add if necessary
ensureMsys
  :: HasBuildConfig env
  => SetupOpts
  -> Memoized SetupInfo
  -> RIO env (Maybe Tool)
ensureMsys sopts getSetupInfo' = do
  platform <- view platformL
  localPrograms <- view $ configL.to configLocalPrograms
  installed <- listInstalled localPrograms

  case platform of
      Platform _ Cabal.Windows | not (soptsSkipMsys sopts) ->
          case getInstalledTool installed (mkPackageName "msys2") (const True) of
              Just tool -> return (Just tool)
              Nothing
                  | soptsInstallIfMissing sopts -> do
                      si <- runMemoized getSetupInfo'
                      osKey <- getOSKey platform
                      config <- view configL
                      VersionedDownloadInfo version info <-
                          case Map.lookup osKey $ siMsys2 si of
                              Just x -> return x
                              Nothing -> throwString $ "MSYS2 not found for " ++ T.unpack osKey
                      let tool = Tool (PackageIdentifier (mkPackageName "msys2") version)
                      Just <$> downloadAndInstallTool (configLocalPrograms config) info tool (installMsys2Windows si)
                  | otherwise -> do
                      logWarn "Continuing despite missing tool: msys2"
                      return Nothing
      _ -> return Nothing

installGhcBindist
  :: HasBuildConfig env
  => SetupOpts
  -> Memoized SetupInfo
  -> [Tool]
  -> RIO env (Tool, CompilerBuild)
installGhcBindist sopts getSetupInfo' installed = do
    Platform expectedArch _ <- view platformL
    let wanted = soptsWantedCompiler sopts
        isWanted = isWantedCompiler (soptsCompilerCheck sopts) (soptsWantedCompiler sopts)
    config <- view configL
    ghcVariant <- view ghcVariantL
    wc <- either throwIO (pure . whichCompiler) $ wantedToActual wanted
    possibleCompilers <-
            case wc of
                Ghc -> do
                    ghcBuilds <- getGhcBuilds
                    forM ghcBuilds $ \ghcBuild -> do
                        ghcPkgName <- parsePackageNameThrowing ("ghc" ++ ghcVariantSuffix ghcVariant ++ compilerBuildSuffix ghcBuild)
                        return (getInstalledTool installed ghcPkgName (isWanted . ACGhc), ghcBuild)
    let existingCompilers = concatMap
            (\(installedCompiler, compilerBuild) ->
                case (installedCompiler, soptsForceReinstall sopts) of
                    (Just tool, False) -> [(tool, compilerBuild)]
                    _ -> [])
            possibleCompilers
    logDebug $
      "Found already installed GHC builds: " <>
      mconcat (intersperse ", " (map (fromString . compilerBuildName . snd) existingCompilers))
    case existingCompilers of
        (tool, build_):_ -> return (tool, build_)
        []
            | soptsInstallIfMissing sopts -> do
                si <- runMemoized getSetupInfo'
                downloadAndInstallPossibleCompilers
                    (map snd possibleCompilers)
                    si
                    (soptsWantedCompiler sopts)
                    (soptsCompilerCheck sopts)
                    (soptsGHCBindistURL sopts)
            | otherwise -> do
                let suggestion = fromMaybe
                        (mconcat
                             [ "To install the correct GHC into "
                             , T.pack (toFilePath (configLocalPrograms config))
                             , ", try running \"stack setup\" or use the \"--install-ghc\" flag."
                             , " To use your system GHC installation, run \"stack config set system-ghc --global true\", or use the \"--system-ghc\" flag."
                             ])
                        (soptsResolveMissingGHC sopts)
                throwM $ CompilerVersionMismatch
                    Nothing -- FIXME ((\(x, y, _) -> (x, y)) <$> msystem)
                    (soptsWantedCompiler sopts, expectedArch)
                    ghcVariant
                    (case possibleCompilers of
                        [] -> CompilerBuildStandard
                        (_, compilerBuild):_ -> compilerBuild)
                    (soptsCompilerCheck sopts)
                    (soptsStackYaml sopts)
                    suggestion

-- | Ensure compiler is installed, without worrying about msys
ensureCompiler
  :: forall env. (HasBuildConfig env, HasGHCVariant env)
  => SetupOpts
  -> Memoized SetupInfo
  -> RIO env (CompilerPaths, ExtraDirs)
ensureCompiler sopts getSetupInfo' = do
    let wanted = soptsWantedCompiler sopts
    wc <- either throwIO (pure . whichCompiler) $ wantedToActual wanted

    Platform expectedArch _ <- view platformL

    let canUseCompiler cp
            | soptsSkipGhcCheck sopts = pure cp
            | not $ isWanted $ cpCompilerVersion cp = throwString "Not the compiler version we want"
            | cpArch cp /= expectedArch = throwString "Not the architecture we want"
            | otherwise = pure cp
        isWanted = isWantedCompiler (soptsCompilerCheck sopts) (soptsWantedCompiler sopts)

    let checkCompiler :: Path Abs File -> RIO env (Maybe CompilerPaths)
        checkCompiler compiler = do
          eres <- tryAny $ pathsFromCompiler wc CompilerBuildStandard False compiler >>= canUseCompiler
          case eres of
            Left e -> do
              logDebug $ "Not using compiler at " <> displayShow (toFilePath compiler) <> ": " <> displayShow e
              pure Nothing
            Right cp -> pure $ Just cp

    mcp <-
        if soptsUseSystem sopts
            then do
                logDebug "Getting system compiler version"
                runConduit $
                  sourceSystemCompilers wanted .|
                  concatMapMC checkCompiler .|
                  await
            else return Nothing
    case mcp of
      Nothing -> ensureSandboxedCompiler sopts getSetupInfo'
      Just cp -> do
        let paths = ExtraDirs { edBins = [parent $ cpCompiler cp], edInclude = [], edLib = [] }
        pure (cp, paths)

ensureSandboxedCompiler
  :: HasBuildConfig env
  => SetupOpts
  -> Memoized SetupInfo
  -> RIO env (CompilerPaths, ExtraDirs)
ensureSandboxedCompiler sopts getSetupInfo' = do
    let wanted = soptsWantedCompiler sopts
    -- List installed tools
    config <- view configL
    let localPrograms = configLocalPrograms config
    installed <- listInstalled localPrograms
    logDebug $ "Installed tools: \n - " <> mconcat (intersperse "\n - " (map (fromString . toolString) installed))
    (compilerTool, compilerBuild) <-
      case soptsWantedCompiler sopts of
       -- shall we build GHC from source?
       WCGhcGit commitId flavour -> buildGhcFromSource getSetupInfo' installed  (configCompilerRepository config) commitId flavour
       _ -> installGhcBindist sopts getSetupInfo' installed
    paths <- extraDirs compilerTool

    wc <- either throwIO (pure . whichCompiler) $ wantedToActual wanted
    menv0 <- view processContextL
    m <- either throwM return
       $ augmentPathMap (toFilePath <$> edBins paths) (view envVarsL menv0)
    menv <- mkProcessContext (removeHaskellEnvVars m)

    names <-
      case wanted of
        WCGhc version -> pure ["ghc-" ++ versionString version, "ghc"]
        WCGhcGit{} -> pure ["ghc"]
        WCGhcjs{} -> throwIO GhcjsNotSupported
    let loop [] = do
          logError $ "Looked for sandboxed compiler named one of: " <> displayShow names
          logError $ "Could not find it on the paths " <> displayShow (edBins paths)
          throwString "Could not find sandboxed compiler"
        loop (x:xs) = do
          res <- findExecutable x
          case res of
            Left _ -> loop xs
            Right y -> parseAbsFile y
    compiler <- withProcessContext menv $ do
      compiler <- loop names

      -- Run this here to ensure that the sanity check uses the modified
      -- environment, otherwise we may infect GHC_PACKAGE_PATH and break sanity
      -- checks.
      when (soptsSanityCheck sopts) $ sanityCheck compiler

      pure compiler

    cp <- pathsFromCompiler wc compilerBuild True compiler
    pure (cp, paths)

pathsFromCompiler
  :: forall env. HasConfig env
  => WhichCompiler
  -> CompilerBuild
  -> Bool
  -> Path Abs File -- ^ executable filepath
  -> RIO env CompilerPaths
pathsFromCompiler wc compilerBuild isSandboxed compiler = withCache $ handleAny onErr $ do
    let dir = toFilePath $ parent compiler
        suffixNoVersion
          | osIsWindows = ".exe"
          | otherwise = ""
        msuffixWithVersion = do
          let prefix =
                case wc of
                  Ghc -> "ghc-"
          fmap ("-" ++) $ stripPrefix prefix $ toFilePath $ filename compiler
        suffixes = maybe id (:) msuffixWithVersion [suffixNoVersion]
        findHelper :: (WhichCompiler -> [String]) -> RIO env (Path Abs File)
        findHelper getNames = do
          let toTry = [dir ++ name ++ suffix | suffix <- suffixes, name <- getNames wc]
              loop [] = throwString $ "Could not find any of: " <> show toTry
              loop (guessedPath':rest) = do
                guessedPath <- parseAbsFile guessedPath'
                exists <- doesFileExist guessedPath
                if exists
                  then pure guessedPath
                  else loop rest
          logDebug $ "Looking for executable(s): " <> displayShow toTry
          loop toTry
    pkg <- fmap GhcPkgExe $ findHelper $ \case
                               Ghc -> ["ghc-pkg"]

    menv0 <- view processContextL
    menv <- mkProcessContext (removeHaskellEnvVars (view envVarsL menv0))

    interpreter <- findHelper $
                   \case
                      Ghc -> ["runghc"]
    haddock <- findHelper $
               \case
                  Ghc -> ["haddock", "haddock-ghc"]
    infobs <- proc (toFilePath compiler) ["--info"]
            $ fmap (toStrictBytes . fst) . readProcess_
    infotext <-
      case decodeUtf8' infobs of
        Left e -> throwString $ "GHC info is not valid UTF-8: " ++ show e
        Right info -> pure info
    infoPairs :: [(String, String)] <-
      case readMaybe $ T.unpack infotext of
        Nothing -> throwString "GHC info does not parse as a list of pairs"
        Just infoPairs -> pure infoPairs
    let infoMap = Map.fromList infoPairs

    eglobaldb <- tryAny $
      case Map.lookup "Global Package DB" infoMap of
        Nothing -> throwString "Key 'Global Package DB' not found in GHC info"
        Just db -> parseAbsDir db

    arch <-
      case Map.lookup "Target platform" infoMap of
        Nothing -> throwString "Key 'Target platform' not found in GHC info"
        Just targetPlatform ->
          case simpleParse $ takeWhile (/= '-') targetPlatform of
            Nothing -> throwString $ "Invalid target platform in GHC info: " ++ show targetPlatform
            Just arch -> pure arch
    compilerVer <-
      case wc of
        Ghc ->
          case Map.lookup "Project version" infoMap of
            Nothing -> do
              logWarn "Key 'Project version' not found in GHC info"
              getCompilerVersion wc compiler
            Just versionString' -> ACGhc <$> parseVersionThrowing versionString'
    globaldb <-
      case eglobaldb of
        Left e -> do
          logWarn "Parsing global DB from GHC info failed"
          logWarn $ displayShow e
          logWarn "Asking ghc-pkg directly"
          withProcessContext menv $ getGlobalDB pkg
        Right x -> pure x

    globalDump <- withProcessContext menv $ globalsFromDump pkg
    cabalPkgVer <-
      case Map.lookup cabalPackageName globalDump of
        Nothing -> throwString $ "Cabal library not found in global package database for " ++ toFilePath compiler
        Just dp -> pure $ pkgVersion $ dpPackageIdent dp

    return CompilerPaths
      { cpBuild = compilerBuild
      , cpArch = arch
      , cpSandboxed = isSandboxed
      , cpCompilerVersion = compilerVer
      , cpCompiler = compiler
      , cpPkg = pkg
      , cpInterpreter = interpreter
      , cpHaddock = haddock
      , cpCabalVersion = cabalPkgVer
      , cpGlobalDB = globaldb
      , cpGhcInfo = infobs
      , cpGlobalDump = globalDump
      }
  where
    onErr = throwIO . InvalidGhcAt compiler

    withCache inner = do
      eres <- tryAny $ loadCompilerPaths compiler compilerBuild isSandboxed
      mres <-
        case eres of
          Left e -> do
            logWarn $ "Trouble loading CompilerPaths cache: " <> displayShow e
            pure Nothing
          Right x -> pure x
      case mres of
        Just cp -> cp <$ logDebug "Loaded compiler information from cache"
        Nothing -> do
          cp <- inner
          saveCompilerPaths cp `catchAny` \e ->
            logWarn ("Unable to save CompilerPaths cache: " <> displayShow e)
          pure cp

buildGhcFromSource :: forall env.
   ( HasTerm env
   , HasProcessContext env
   , HasBuildConfig env
   ) => Memoized SetupInfo -> [Tool] -> CompilerRepository -> Text -> Text
   -> RIO env (Tool, CompilerBuild)
buildGhcFromSource getSetupInfo' installed (CompilerRepository url) commitId flavour = do
   config <- view configL
   let compilerTool = ToolGhcGit commitId flavour

   -- detect when the correct GHC is already installed
   if compilerTool `elem` installed
     then return (compilerTool,CompilerBuildStandard)
     else do
       let repo = Repo
            { repoCommit = commitId
            , repoUrl    = url
            , repoType   = RepoGit
            , repoSubdir = mempty
            }

       -- clone the repository and execute the given commands
       Pantry.withRepo repo $ do
         -- withRepo is guaranteed to set workingDirL, so let's get it
         mcwd <- traverse parseAbsDir =<< view workingDirL
         let cwd = fromMaybe (error "Invalid working directory") mcwd

         threads <- view $ configL.to configJobs
         let
           hadrianArgs = fmap T.unpack
               [ "-c"                    -- run ./boot and ./configure
               , "-j" <> tshow threads   -- parallel build
               , "--flavour=" <> flavour -- selected flavour
               , "binary-dist"
               ]
           hadrianScripts
             | osIsWindows = hadrianScriptsWindows
             | otherwise   = hadrianScriptsPosix

         foundHadrianPaths <- filterM doesFileExist $ (cwd </>) <$> hadrianScripts
         hadrianPath <- maybe (throwString "No Hadrian build script found") pure $ listToMaybe foundHadrianPaths

         logSticky $ "Building GHC from source with `"
            <> RIO.display flavour
            <> "` flavour. It can take a long time (more than one hour)..."

         -- We need to provide an absolute path to the script since
         -- the process package only sets working directory _after_
         -- discovering the executable
         proc (toFilePath hadrianPath) hadrianArgs runProcess_

         -- find the bindist and install it
         bindistPath <- parseRelDir "_build/bindist"
         (_,files) <- listDir (cwd </> bindistPath)
         let
           isBindist p = do
             extension <- fileExtension (filename p)

             return $ "ghc-" `isPrefixOf` (toFilePath (filename p))
                         && extension == ".xz"

         mbindist <- filterM isBindist files
         case mbindist of
           [bindist] -> do
               let bindist' = T.pack (toFilePath bindist)
                   dlinfo = DownloadInfo
                             { downloadInfoUrl           = bindist'
                               -- we can specify a filepath instead of a URL
                             , downloadInfoContentLength = Nothing
                             , downloadInfoSha1          = Nothing
                             , downloadInfoSha256        = Nothing
                             }
                   ghcdlinfo = GHCDownloadInfo mempty mempty dlinfo
                   installer
                      | osIsWindows = installGHCWindows
                      | otherwise   = installGHCPosix ghcdlinfo
               si <- runMemoized getSetupInfo'
               _ <- downloadAndInstallTool
                 (configLocalPrograms config)
                 dlinfo
                 compilerTool
                 (installer si)
               return (compilerTool, CompilerBuildStandard)
           _ -> do
              forM_ files (logDebug . fromString . (" - " ++) . toFilePath)
              error "Can't find hadrian generated bindist"


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
                   $ tryAny . fmap fst . readProcess_
                let firstWords = case eldconfigOut of
                        Right ldconfigOut -> mapMaybe (listToMaybe . T.words) $
                            T.lines $ T.decodeUtf8With T.lenientDecode
                                    $ LBS.toStrict ldconfigOut
                        Left _ -> []
                    checkLib lib
                        | libT `elem` firstWords = do
                            logDebug ("Found shared library " <> libD <> " in 'ldconfig -p' output")
                            return True
                        | osIsWindows =
                            -- Cannot parse /usr/lib on Windows
                            return False
                        | otherwise = do
                        -- This is a workaround for the fact that libtinfo.so.x doesn't appear in
                        -- the 'ldconfig -p' output on Arch or Slackware even when it exists.
                        -- There doesn't seem to be an easy way to get the true list of directories
                        -- to scan for shared libs, but this works for our particular cases.
                            matches <- filterM (doesFileExist .(</> lib)) usrLibDirs
                            case matches of
                                [] -> logDebug ("Did not find shared library " <> libD)
                                    >> return False
                                (path:_) -> logDebug ("Found shared library " <> libD
                                        <> " in " <> fromString (Path.toFilePath path))
                                    >> return True
                      where
                        libT = T.pack (toFilePath lib)
                        libD = fromString (toFilePath lib)
                hastinfo5 <- checkLib relFileLibtinfoSo5
                hastinfo6 <- checkLib relFileLibtinfoSo6
                hasncurses6 <- checkLib relFileLibncurseswSo6
                hasgmp5 <- checkLib relFileLibgmpSo10
                hasgmp4 <- checkLib relFileLibgmpSo3
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
            Platform _ Cabal.FreeBSD -> do
                let getMajorVer = readMaybe <=< headMaybe . (splitOn ".")
                majorVer <- getMajorVer <$> sysRelease
                if majorVer >= Just (12 :: Int) then
                  useBuilds [CompilerBuildSpecialized "ino64"]
                else
                  useBuilds [CompilerBuildStandard]
            Platform _ Cabal.OpenBSD -> do
                releaseStr <- mungeRelease <$> sysRelease
                useBuilds [CompilerBuildSpecialized releaseStr]
            _ -> useBuilds [CompilerBuildStandard]
    useBuilds builds = do
        logDebug $
          "Potential GHC builds: " <>
          mconcat (intersperse ", " (map (fromString . compilerBuildName) builds))
        return builds

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
               logWarn $ "Could not query OS version: " <> displayShow e
               return "")
  (liftIO getRelease)

-- | Ensure Docker container-compatible 'stack' executable is downloaded
ensureDockerStackExe :: HasConfig env => Platform -> RIO env (Path Abs File)
ensureDockerStackExe containerPlatform = do
    config <- view configL
    containerPlatformDir <- runReaderT platformOnlyRelDir (containerPlatform,PlatformVariantNone)
    let programsPath = configLocalProgramsBase config </> containerPlatformDir
        tool = Tool (PackageIdentifier (mkPackageName "stack") stackVersion)
    stackExeDir <- installDir programsPath tool
    let stackExePath = stackExeDir </> relFileStack
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

-- | Get all executables on the path that might match the wanted compiler
sourceSystemCompilers
  :: (HasProcessContext env, HasLogFunc env)
  => WantedCompiler
  -> ConduitT i (Path Abs File) (RIO env) ()
sourceSystemCompilers wanted = do
  searchPath <- view exeSearchPathL
  names <-
    case wanted of
      WCGhc version -> pure
        [ "ghc-" ++ versionString version
        , "ghc"
        ]
      WCGhcjs{} -> throwIO GhcjsNotSupported
      WCGhcGit{} -> pure [] -- only use sandboxed versions
  for_ names $ \name -> for_ searchPath $ \dir -> do
    fp <- resolveFile' $ addExe $ dir FP.</> name
    exists <- doesFileExist fp
    when exists $ yield fp
  where
    addExe
      | osIsWindows = (++ ".exe")
      | otherwise = id

-- | Download the most recent SetupInfo
getSetupInfo :: HasConfig env => RIO env SetupInfo
getSetupInfo = do
    config <- view configL
    let inlineSetupInfo = configSetupInfoInline config
        locations' = configSetupInfoLocations config
        locations = if null locations' then [defaultSetupInfoYaml] else locations'

    resolvedSetupInfos <- mapM loadSetupInfo locations
    return (inlineSetupInfo <> mconcat resolvedSetupInfos)
  where
    loadSetupInfo urlOrFile = do
      bs <-
          case parseUrlThrow urlOrFile of
              Just req -> liftM (LBS.toStrict . getResponseBody) $ httpLbs req
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
        else Just $ Tool $ maximumBy (comparing pkgVersion) available
  where
    available = mapMaybe goodPackage installed
    goodPackage (Tool pi') =
        if pkgName pi' == name &&
           goodVersion (pkgVersion pi')
            then Just pi'
            else Nothing
    goodPackage _ = Nothing

downloadAndInstallTool :: (HasTerm env, HasBuildConfig env)
                       => Path Abs Dir
                       -> DownloadInfo
                       -> Tool
                       -> (Path Abs File -> ArchiveType -> Path Abs Dir -> Path Abs Dir -> RIO env ())
                       -> RIO env Tool
downloadAndInstallTool programsDir downloadInfo tool installer = do
    ensureDir programsDir
    (file, at) <- downloadFromInfo programsDir downloadInfo tool
    dir <- installDir programsDir tool
    tempDir <- tempInstallDir programsDir tool
    liftIO $ ignoringAbsence (removeDirRecur tempDir)
    ensureDir tempDir
    unmarkInstalled programsDir tool
    installer file at tempDir dir
    markInstalled programsDir tool
    liftIO $ ignoringAbsence (removeDirRecur tempDir)
    return tool

downloadAndInstallCompiler :: (HasBuildConfig env, HasGHCVariant env)
                           => CompilerBuild
                           -> SetupInfo
                           -> WantedCompiler
                           -> VersionCheck
                           -> Maybe String
                           -> RIO env Tool
downloadAndInstallCompiler ghcBuild si wanted@(WCGhc version) versionCheck mbindistURL = do
    ghcVariant <- view ghcVariantL
    (selectedVersion, downloadInfo) <- case mbindistURL of
        Just bindistURL -> do
            case ghcVariant of
                GHCCustom _ -> return ()
                _ -> throwM RequireCustomGHCVariant
            return (version, GHCDownloadInfo mempty mempty DownloadInfo
                     { downloadInfoUrl = T.pack bindistURL
                     , downloadInfoContentLength = Nothing
                     , downloadInfoSha1 = Nothing
                     , downloadInfoSha256 = Nothing
                     })
        _ -> do
            ghcKey <- getGhcKey ghcBuild
            case Map.lookup ghcKey $ siGHCs si of
                Nothing -> throwM $ UnknownOSKey ghcKey
                Just pairs_ -> getWantedCompilerInfo ghcKey versionCheck wanted ACGhc pairs_
    config <- view configL
    let installer =
            case configPlatform config of
                Platform _ Cabal.Windows -> installGHCWindows
                _ -> installGHCPosix downloadInfo
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
    ghcPkgName <- parsePackageNameThrowing ("ghc" ++ ghcVariantSuffix ghcVariant ++ compilerBuildSuffix ghcBuild)
    let tool = Tool $ PackageIdentifier ghcPkgName selectedVersion
    downloadAndInstallTool (configLocalPrograms config) (gdiDownloadInfo downloadInfo) tool (installer si)

downloadAndInstallCompiler _ _ WCGhcjs{} _ _ = throwIO GhcjsNotSupported

downloadAndInstallCompiler _ _ WCGhcGit{} _ _ =
    error "downloadAndInstallCompiler: shouldn't be reached with ghc-git"

getWantedCompilerInfo :: (Ord k, MonadThrow m)
                      => Text
                      -> VersionCheck
                      -> WantedCompiler
                      -> (k -> ActualCompiler)
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
    :: (HasGHCVariant env, HasBuildConfig env)
    => [CompilerBuild]
    -> SetupInfo
    -> WantedCompiler
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
        Platform AArch64               Cabal.Linux   -> return "linux-aarch64"
        Platform Sparc                 Cabal.Linux   -> return "linux-sparc"
        Platform AArch64               Cabal.OSX     -> return "macosx-aarch64"
        Platform arch os -> throwM $ UnsupportedSetupCombo os arch

downloadOrUseLocal
    :: (HasTerm env, HasBuildConfig env)
    => Text -> DownloadInfo -> Path Abs File -> RIO env (Path Abs File)
downloadOrUseLocal downloadLabel downloadInfo destination =
  case url of
    (parseUrlThrow -> Just _) -> do
        ensureDir (parent destination)
        chattyDownload downloadLabel downloadInfo destination
        return destination
    (parseAbsFile -> Just path) -> do
        warnOnIgnoredChecks
        return path
    (parseRelFile -> Just path) -> do
        warnOnIgnoredChecks
        root <- view projectRootL
        return (root </> path)
    _ ->
        throwString $ "Error: `url` must be either an HTTP URL or a file path: " ++ url
  where
    url = T.unpack $ downloadInfoUrl downloadInfo
    warnOnIgnoredChecks = do
      let DownloadInfo{downloadInfoContentLength=contentLength, downloadInfoSha1=sha1,
                       downloadInfoSha256=sha256} = downloadInfo
      when (isJust contentLength) $
        logWarn "`content-length` is not checked and should not be specified when `url` is a file path"
      when (isJust sha1) $
        logWarn "`sha1` is not checked and should not be specified when `url` is a file path"
      when (isJust sha256) $
        logWarn "`sha256` is not checked and should not be specified when `url` is a file path"

downloadFromInfo
    :: (HasTerm env, HasBuildConfig env)
    => Path Abs Dir -> DownloadInfo -> Tool -> RIO env (Path Abs File, ArchiveType)
downloadFromInfo programsDir downloadInfo tool = do
    archiveType <-
        case extension of
            ".tar.xz" -> return TarXz
            ".tar.bz2" -> return TarBz2
            ".tar.gz" -> return TarGz
            ".7z.exe" -> return SevenZ
            _ -> throwString $ "Error: Unknown extension for url: " ++ url

    relativeFile <- parseRelFile $ toolString tool ++ extension
    let destinationPath = programsDir </> relativeFile
    localPath <- downloadOrUseLocal (T.pack (toolString tool)) downloadInfo destinationPath
    return (localPath, archiveType)

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
                => GHCDownloadInfo
                -> SetupInfo
                -> Path Abs File
                -> ArchiveType
                -> Path Abs Dir
                -> Path Abs Dir
                -> RIO env ()
installGHCPosix downloadInfo _ archiveFile archiveType tempDir destDir = do
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

    let runStep step wd env cmd args = do
          menv' <- modifyEnvVars menv (Map.union env)
          let logLines lvl = CB.lines .| CL.mapM_ (lvl . displayBytesUtf8)
              logStdout = logLines logDebug
              logStderr = logLines logError
          void $ withWorkingDir (toFilePath wd) $
                withProcessContext menv' $
                sinkProcessStderrStdout cmd args logStderr logStdout
                `catchAny` \ex -> do
                  logError $ displayShow ex
                  prettyError $ hang 2 (
                      "Error encountered while" <+> step <+> "GHC with"
                      <> line <>
                      style Shell (fromString (unwords (cmd : args)))
                      <> line <>
                      -- TODO: Figure out how to insert \ in the appropriate spots
                      -- hang 2 (shellColor (fillSep (fromString cmd : map fromString args))) <> line <>
                      "run in " <> pretty wd
                      )
                    <> line <> line <>
                    "The following directories may now contain files, but won't be used by stack:"
                    <> line <>
                    "  -" <+> pretty tempDir
                    <> line <>
                    "  -" <+> pretty destDir
                    <> line <> line <>
                    "For more information consider rerunning with --verbose flag"
                    <> line
                  exitFailure

    logSticky $
      "Unpacking GHC into " <>
      fromString (toFilePath tempDir) <>
      " ..."
    logDebug $ "Unpacking " <> fromString (toFilePath archiveFile)
    runStep "unpacking" tempDir mempty tarTool [compOpt : "xf", toFilePath archiveFile]

    dir <- expectSingleUnpackedDir archiveFile tempDir

    logSticky "Configuring GHC ..."
    runStep "configuring" dir
        (gdiConfigureEnv downloadInfo)
        (toFilePath $ dir </> relFileConfigure)
        (("--prefix=" ++ toFilePath destDir) : map T.unpack (gdiConfigureOpts downloadInfo))

    logSticky "Installing GHC ..."
    runStep "installing" dir mempty makeTool ["install"]

    logStickyDone $ "Installed GHC."
    logDebug $ "GHC installed to " <> fromString (toFilePath destDir)

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

installGHCWindows :: HasBuildConfig env
                  => SetupInfo
                  -> Path Abs File
                  -> ArchiveType
                  -> Path Abs Dir
                  -> Path Abs Dir
                  -> RIO env ()
installGHCWindows si archiveFile archiveType _tempDir destDir = do
    withUnpackedTarball7z "GHC" si archiveFile archiveType destDir
    logInfo $ "GHC installed to " <> fromString (toFilePath destDir)

installMsys2Windows :: HasBuildConfig env
                  => SetupInfo
                  -> Path Abs File
                  -> ArchiveType
                  -> Path Abs Dir
                  -> Path Abs Dir
                  -> RIO env ()
installMsys2Windows si archiveFile archiveType _tempDir destDir = do
    exists <- liftIO $ D.doesDirectoryExist $ toFilePath destDir
    when exists $ liftIO (D.removeDirectoryRecursive $ toFilePath destDir) `catchIO` \e -> do
        logError $
            "Could not delete existing msys directory: " <>
            fromString (toFilePath destDir)
        throwM e

    withUnpackedTarball7z "MSYS2" si archiveFile archiveType destDir


    -- I couldn't find this officially documented anywhere, but you need to run
    -- the MSYS shell once in order to initialize some pacman stuff. Once that
    -- run happens, you can just run commands as usual.
    menv0 <- view processContextL
    newEnv0 <- modifyEnvVars menv0 $ Map.insert "MSYSTEM" "MSYS"
    newEnv <- either throwM return $ augmentPathMap
                  [toFilePath $ destDir </> relDirUsr </> relDirBin]
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
withUnpackedTarball7z :: HasBuildConfig env
                      => String -- ^ Name of tool, used in error messages
                      -> SetupInfo
                      -> Path Abs File -- ^ Path to archive file
                      -> ArchiveType
                      -> Path Abs Dir -- ^ Destination directory.
                      -> RIO env ()
withUnpackedTarball7z name si archiveFile archiveType destDir = do
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
        absSrcDir <- expectSingleUnpackedDir archiveFile tmpDir
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
setup7z :: (HasBuildConfig env, MonadIO m)
        => SetupInfo
        -> RIO env (Path Abs Dir -> Path Abs File -> m ())
setup7z si = do
    dir <- view $ configL.to configLocalPrograms
    ensureDir dir
    let exeDestination = dir </> relFile7zexe
        dllDestination = dir </> relFile7zdll
    case (siSevenzDll si, siSevenzExe si) of
        (Just sevenzDll, Just sevenzExe) -> do
            _ <- downloadOrUseLocal "7z.dll" sevenzDll dllDestination
            exePath <- downloadOrUseLocal "7z.exe" sevenzExe exeDestination
            withRunInIO $ \run -> return $ \outdir archive -> liftIO $ run $ do
                let cmd = toFilePath exePath
                    args =
                        [ "x"
                        , "-o" ++ toFilePath outdir
                        , "-y"
                        , toFilePath archive
                        ]
                let archiveDisplay = fromString $ FP.takeFileName $ toFilePath archive
                    isExtract = FP.takeExtension (toFilePath archive) == ".tar"
                logInfo $
                  (if isExtract then "Extracting " else "Decompressing ") <>
                  archiveDisplay <> "..."
                ec <-
                  proc cmd args $ \pc ->
                  if isExtract
                    then withProcessWait (setStdout createSource pc) $ \p -> do
                        total <- runConduit
                            $ getStdout p
                           .| filterCE (== 10) -- newline characters
                           .| foldMC
                                (\count bs -> do
                                    let count' = count + S.length bs
                                    logSticky $ "Extracted " <> RIO.display count' <> " files"
                                    pure count'
                                )
                                0
                        logStickyDone $
                          "Extracted total of " <>
                          RIO.display total <>
                          " files from " <>
                          archiveDisplay
                        waitExitCode p
                    else runProcess pc
                when (ec /= ExitSuccess)
                    $ liftIO $ throwM (ProblemWhileDecompressing archive)
        _ -> throwM SetupInfoMissingSevenz

chattyDownload :: HasTerm env
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
    let dReq = setHashChecks hashChecks $
               setLengthCheck mtotalSize $
               mkDownloadRequest req
    x <- verifiedDownloadWithProgress dReq path label mtotalSize
    if x
        then logStickyDone ("Downloaded " <> RIO.display label <> ".")
        else logStickyDone "Already downloaded."
  where
    mtotalSize = downloadInfoContentLength downloadInfo

-- | Perform a basic sanity check of GHC
sanityCheck :: (HasProcessContext env, HasLogFunc env)
            => Path Abs File -> RIO env ()
sanityCheck ghc = withSystemTempDir "stack-sanity-check" $ \dir -> do
    let fp = toFilePath $ dir </> relFileMainHs
    liftIO $ S.writeFile fp $ T.encodeUtf8 $ T.pack $ unlines
        [ "import Distribution.Simple" -- ensure Cabal library is present
        , "main = putStrLn \"Hello World\""
        ]
    logDebug $ "Performing a sanity check on: " <> fromString (toFilePath ghc)
    eres <- withWorkingDir (toFilePath dir) $ proc (toFilePath ghc)
        [ fp
        , "-no-user-package-db"
        ] $ try . readProcess_
    case eres of
        Left e -> throwIO $ GHCSanityCheckCompileFailed e ghc
        Right _ -> return () -- TODO check that the output of running the command is correct

-- Remove potentially confusing environment variables
removeHaskellEnvVars :: Map Text Text -> Map Text Text
removeHaskellEnvVars =
    Map.delete "GHC_PACKAGE_PATH" .
    Map.delete "GHC_ENVIRONMENT" .
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
    => ActualCompiler
    -> RIO env (Map Text Text)
getUtf8EnvVars compilerVer =
    if getGhcVersion compilerVer >= mkVersion [7, 10, 3]
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
                        elocales <- tryAny $ fmap fst $ proc "locale" ["-a"] readProcess_
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

-- | Information on a binary release of Stack
data StackReleaseInfo
  = SRIGithub !Value
  -- ^ Metadata downloaded from GitHub releases about available binaries.
  | SRIHaskellStackOrg !HaskellStackOrg
  -- ^ Information on the latest available binary for the current platforms.

data HaskellStackOrg = HaskellStackOrg
  { hsoUrl :: !Text
  , hsoVersion :: !Version
  }
  deriving Show

downloadStackReleaseInfo
  :: (HasPlatform env, HasLogFunc env)
  => Maybe String -- Github org
  -> Maybe String -- Github repo
  -> Maybe String -- ^ optional version
  -> RIO env StackReleaseInfo
downloadStackReleaseInfo Nothing Nothing Nothing = do
    platform <- view platformL
    -- Fallback list of URLs to try for upgrading.
    let urls0 =
          case platform of
            Platform X86_64 Cabal.Linux ->
              [ "https://get.haskellstack.org/upgrade/linux-x86_64-static.tar.gz"
              , "https://get.haskellstack.org/upgrade/linux-x86_64.tar.gz"
              ]
            Platform X86_64 Cabal.OSX ->
              [ "https://get.haskellstack.org/upgrade/osx-x86_64.tar.gz"
              ]
            Platform X86_64 Cabal.Windows ->
              [ "https://get.haskellstack.org/upgrade/windows-x86_64.tar.gz"
              ]
            _ -> []
        -- Helper function: extract the version from a GitHub releases URL.
    let extractVersion loc = do
          version0 <-
            case reverse $ splitOn "/" $ T.unpack loc of
              _final:version0:_ -> Right version0
              _ -> Left $ "Insufficient pieces in location: " ++ show loc
          version1 <- maybe (Left "no leading v on version") Right $ stripPrefix "v" version0
          maybe (Left $ "Invalid version: " ++ show version1) Right $ parseVersion version1

        -- Try out different URLs. If we've exhausted all of them, fall back to GitHub.
        loop [] = do
          logDebug "Could not get binary from haskellstack.org, trying GitHub"
          downloadStackReleaseInfoGithub Nothing Nothing Nothing

        -- Try the next URL
        loop (url:urls) = do
          -- Make a HEAD request without any redirects
          req <- setRequestMethod "HEAD" <$> parseRequest (T.unpack url)
          res <- httpLbs req { redirectCount = 0 }

          -- Look for a redirect. We're looking for a standard GitHub releases
          -- URL where we can extract version information from.
          case getResponseHeader "location" res of
            [] -> logDebug "No location header found, continuing" *> loop urls
            -- Exactly one location header.
            [locBS] ->
              case decodeUtf8' locBS of
                Left e -> logDebug ("Invalid UTF8: " <> displayShow (locBS, e)) *> loop urls
                Right loc ->
                  case extractVersion loc of
                    Left s -> logDebug ("No version found: " <> displayShow (url, loc, s)) *> loop (loc:urls)
                    -- We found a valid URL, let's use it!
                    Right version -> do
                      let hso = HaskellStackOrg
                                  { hsoUrl = loc
                                  , hsoVersion = version
                                  }
                      logDebug $ "Downloading from haskellstack.org: " <> displayShow hso
                      pure $ SRIHaskellStackOrg hso
            locs -> logDebug ("Multiple location headers found: " <> displayShow locs) *> loop urls
    loop urls0
downloadStackReleaseInfo morg mrepo mver = downloadStackReleaseInfoGithub morg mrepo mver

-- | Same as above, but always uses Github
downloadStackReleaseInfoGithub
  :: (MonadIO m, MonadThrow m)
  => Maybe String -- Github org
  -> Maybe String -- Github repo
  -> Maybe String -- ^ optional version
  -> m StackReleaseInfo
downloadStackReleaseInfoGithub morg mrepo mver = liftIO $ do
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
        then return $ SRIGithub $ getResponseBody res
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
                ( destDir </> relFileStackDotExe
                , destDir </> relFileStackDotTmpDotExe
                )
            | otherwise =
                ( destDir </> relFileStack
                , destDir </> relFileStackDotTmp
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

    -- We need to call getExecutablePath before we overwrite the
    -- currently running binary: after that, Linux will append
    -- (deleted) to the filename.
    currExe <- liftIO getExecutablePath

    liftIO $ do
      setFileExecutable (toFilePath tmpFile)

      testExe tmpFile

      case platform of
          Platform _ Cabal.Windows | FP.equalFilePath (toFilePath destFile) currExe -> do
              old <- parseAbsFile (toFilePath destFile ++ ".old")
              renameFile destFile old
              renameFile tmpFile destFile
          _ -> renameFile tmpFile destFile

    destDir' <- liftIO . D.canonicalizePath . toFilePath $ destDir
    warnInstallSearchPathIssues destDir' ["stack"]

    logInfo $ "New stack executable available at " <> fromString (toFilePath destFile)

    when checkPath $ performPathChecking destFile currExe
      `catchAny` (logError . displayShow)
  where

    findArchive (SRIGithub val) pattern = do
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
    findArchive (SRIHaskellStackOrg hso) _ = pure $ hsoUrl hso

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
                loop (Tar.Next e es) =
                    case FP.splitPath (Tar.entryPath e) of
                        -- Ignore the first component, see: https://github.com/commercialhaskell/stack/issues/5288
                        [_ignored, name] | name == exeName -> do
                            case Tar.entryContent e of
                                Tar.NormalFile lbs _ -> do
                                  ensureDir destDir
                                  LBS.writeFile (toFilePath tmpFile) lbs
                                _ -> error $ concat
                                    [ "Invalid file type for tar entry named "
                                    , Tar.entryPath e
                                    , " downloaded from "
                                    , T.unpack url
                                    ]
                        _ -> loop es
            loop entries
      where
        exeName
          | isWindows = "stack.exe"
          | otherwise = "stack"

-- | Ensure that the Stack executable download is in the same location
-- as the currently running executable. See:
-- https://github.com/commercialhaskell/stack/issues/3232
performPathChecking
    :: HasConfig env
    => Path Abs File -- ^ location of the newly downloaded file
    -> String -- ^ currently running executable
    -> RIO env ()
performPathChecking newFile executablePath = do
  executablePath' <- parseAbsFile executablePath
  unless (toFilePath newFile == executablePath) $ do
    logInfo $ "Also copying stack executable to " <> fromString executablePath
    tmpFile <- parseAbsFile $ executablePath ++ ".tmp"
    eres <- tryIO $ do
      liftIO $ copyFile newFile tmpFile
      setFileExecutable (toFilePath tmpFile)
      liftIO $ renameFile tmpFile executablePath'
      logInfo "Stack executable copied successfully!"
    case eres of
      Right () -> return ()
      Left e
        | isPermissionError e -> do
            logWarn $ "Permission error when trying to copy: " <> displayShow e
            logWarn "Should I try to perform the file copy using sudo? This may fail"
            toSudo <- promptBool "Try using sudo? (y/n) "
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

getDownloadVersion :: StackReleaseInfo -> Maybe Version
getDownloadVersion (SRIGithub val) = do
    Object o <- Just val
    String rawName <- HashMap.lookup "name" o
    -- drop the "v" at the beginning of the name
    parseVersion $ T.unpack (T.drop 1 rawName)
getDownloadVersion (SRIHaskellStackOrg hso) = Just $ hsoVersion hso
