{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Main stack tool entry point.

module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (ask)
import           Data.Attoparsec.Args (withInterpreterArgs)
import           Data.Char (toLower)
import           Data.List
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable
import           Network.HTTP.Client
import           Options.Applicative.Args
import           Options.Applicative.Builder.Extra
import           Options.Applicative.Simple
import           Options.Applicative.Types (readerAsk)
import           Path
import qualified Paths_stack as Meta
import           Plugins
import           Prelude hiding (pi)
import           Stack.Build
import           Stack.Build.Types
import           Stack.Config
import           Stack.Constants
import qualified Stack.Docker as Docker
import           Stack.Dot
import           Stack.Exec
import           Stack.Fetch
import           Stack.FileWatch
import           Stack.Init
import           Stack.New
import qualified Stack.PackageIndex
import           Stack.Repl
import           Stack.Ide
import           Stack.Setup
import           Stack.Solver (solveExtraDeps)
import           Stack.Types
import           Stack.Types.Internal
import           Stack.Types.StackT
import           Stack.Upgrade
import qualified Stack.Upload as Upload
import           System.Directory (canonicalizePath)
import           System.Environment (getArgs, getProgName)
import           System.Exit
import           System.FilePath (searchPathSeparator,dropTrailingPathSeparator)
import           System.IO (hIsTerminalDevice, stderr, stdin, stdout, hSetBuffering, BufferMode(..))
import           System.Process.Read

-- | Commandline dispatcher.
main :: IO ()
main = withInterpreterArgs stackProgName $ \args isInterpreter ->
  do -- Line buffer the output by default, particularly for non-terminal runs.
     -- See https://github.com/commercialhaskell/stack/pull/360
     hSetBuffering stdout LineBuffering
     hSetBuffering stdin  LineBuffering
     hSetBuffering stderr NoBuffering
     when False $ do -- https://github.com/commercialhaskell/stack/issues/322
       plugins <- findPlugins (T.pack stackProgName)
       tryRunPlugin plugins
     progName <- getProgName
     isTerminal <- hIsTerminalDevice stdout
     execExtraHelp args
                   dockerHelpOptName
                   (Docker.dockerOptsParser True)
                   ("Only showing --" ++ Docker.dockerCmdName ++ "* options.")
     let versionString' = $(simpleVersion Meta.version)
     eGlobalRun <- try $
       simpleOptions
         versionString'
         "stack - The Haskell Tool Stack"
         ""
         (extraHelpOption progName (Docker.dockerCmdName ++ "*") dockerHelpOptName <*>
          globalOpts isTerminal)
         (do addCommand "build"
                        "Build the project(s) in this directory/configuration"
                        (buildCmd DoNothing)
                        (buildOpts Build)
             addCommand "install"
                        "Build executables and install to a user path"
                        installCmd
                        (buildOpts Build)
             addCommand "test"
                        "Build and test the project(s) in this directory/configuration"
                        (\(bopts, topts) ->
                             let bopts' = if toCoverage topts
                                             then bopts { boptsExeProfile = True
                                                        , boptsLibProfile = True
                                                        , boptsGhcOptions = "-fhpc" : boptsGhcOptions bopts}
                                             else bopts
                             in buildCmd (DoTests topts) bopts')
                        ((,) <$> buildOpts Test <*> testOpts)
             addCommand "bench"
                        "Build and benchmark the project(s) in this directory/configuration"
                        (\(bopts, beopts) -> buildCmd (DoBenchmarks beopts) bopts)
                        ((,) <$> buildOpts Bench <*> benchOpts)
             addCommand "haddock"
                        "Generate haddocks for the project(s) in this directory/configuration"
                        (buildCmd DoNothing)
                        (buildOpts Haddock)
             addCommand "new"
                        "Create a brand new project"
                        newCmd
                        initOptsParser
             addCommand "init"
                        "Initialize a stack project based on one or more cabal packages"
                        initCmd
                        initOptsParser
             addCommand "solver"
                        "Use a dependency solver to try and determine missing extra-deps"
                        solverCmd
                        solverOptsParser
             addCommand "setup"
                        "Get the appropriate ghc for your project"
                        setupCmd
                        setupParser
             addCommand "path"
                        "Print out handy path information"
                        pathCmd
                        (fmap
                             catMaybes
                             (sequenceA
                                  (map
                                      (\(desc,name,_) ->
                                           flag Nothing
                                                (Just name)
                                                (long (T.unpack name) <>
                                                 help desc))
                                      paths)))
             addCommand "unpack"
                        "Unpack one or more packages locally"
                        unpackCmd
                        (some $ strArgument $ metavar "PACKAGE")
             addCommand "update"
                        "Update the package index"
                        updateCmd
                        (pure ())
             addCommand "upgrade"
                        "Upgrade to the latest stack (experimental)"
                        upgradeCmd
                        (switch
                            ( long "git"
                           <> help "Clone from Git instead of downloading from Hackage (more dangerous)"
                            ))
             addCommand "upload"
                        "Upload a package to Hackage"
                        uploadCmd
                        (many $ strArgument $ metavar "TARBALL/DIR")
             addCommand "dot"
                        "Visualize your project's dependency graph using Graphviz dot"
                        dotCmd
                        dotOptsParser
             addCommand "exec"
                        "Execute a command"
                        execCmd
                        (execOptsParser Nothing)
             addCommand "ghc"
                        "Run ghc"
                        execCmd
                        (execOptsParser $ Just "ghc")
             addCommand "ghci"
                        "Run ghci in the context of project(s)"
                        replCmd
                        ((,,,,) <$>
                         fmap (map T.pack)
                              (many (strArgument
                                       (metavar "TARGET" <>
                                        help "If none specified, use all packages defined in current directory"))) <*>
                         argsOption (long "ghc-options" <>
                                      metavar "OPTION" <>
                                      help "Additional options passed to GHCi" <>
                                      value []) <*>
                         strOption (long "with-ghc" <>
                                    metavar "GHC" <>
                                    help "Use this command for the GHC to run" <>
                                    value "ghc" <>
                                    showDefault) <*>
                         flag False True (long "no-load" <>
                                         help "Don't load modules on start-up") <*>
                         packagesParser)
             addCommand "ide"
                        "Run ide-backend-client with the correct arguments"
                        ideCmd
                        ((,) <$>
                         fmap (map T.pack)
                              (many (strArgument
                                       (metavar "TARGET" <>
                                        help "If none specified, use all packages defined in current directory"))) <*>
                         argsOption (long "ghc-options" <>
                                     metavar "OPTION" <>
                                     help "Additional options passed to GHCi" <>
                                     value []))
             addCommand "runghc"
                        "Run runghc"
                        execCmd
                        (execOptsParser $ Just "runghc")
             addCommand "clean"
                        "Clean the local packages"
                        cleanCmd
                        (pure ())
             addSubCommands
               Docker.dockerCmdName
               "Subcommands specific to Docker use"
               (do addCommand Docker.dockerPullCmdName
                              "Pull latest version of Docker image from registry"
                              dockerPullCmd
                              (pure ())
                   addCommand "reset"
                              "Reset the Docker sandbox"
                              dockerResetCmd
                              (flag False True (long "keep-home" <>
                                               help "Do not delete sandbox's home directory"))
                   addCommand Docker.dockerCleanupCmdName
                              "Clean up Docker images and containers"
                              dockerCleanupCmd
                              dockerCleanupOpts)
             )
             -- commandsFromPlugins plugins pluginShouldHaveRun) https://github.com/commercialhaskell/stack/issues/322
     case eGlobalRun of
       Left (exitCode :: ExitCode) -> do
         when isInterpreter $
           putStrLn $ concat
             [ "\nIf you are trying to use "
             , stackProgName
             , " as a script interpreter, a\n'-- "
             , stackProgName
             , " [options] runghc [options]' comment is required."
             , "\nSee https://github.com/commercialhaskell/stack/wiki/Script-interpreter" ]
         throwIO exitCode
       Right (global,run) -> do
         when (globalLogLevel global == LevelDebug) $ putStrLn versionString'
         run global `catch` \e -> do
            -- This special handler stops "stack: " from being printed before the
            -- exception
            case fromException e of
                Just ec -> exitWith ec
                Nothing -> do
                    printExceptionStderr e
                    exitFailure
  where
    dockerHelpOptName = Docker.dockerCmdName ++ "-help"

-- Try to run a plugin
tryRunPlugin :: Plugins -> IO ()
tryRunPlugin plugins = do
  args <- getArgs
  case dropWhile (List.isPrefixOf "-") args of
    ((T.pack -> name):args')
      | isJust (lookupPlugin plugins name) -> do
          callPlugin plugins name args' `catch` onPluginErr
          exitSuccess
    _ -> return ()
-- TODO(danburton): use logger
onPluginErr :: PluginException -> IO ()
onPluginErr (PluginNotFound _ name) = do
  T.hPutStr stderr $ "Stack plugin not found: " <> name
  exitFailure
onPluginErr (PluginExitFailure _ i) = do
  exitWith (ExitFailure i)

-- TODO(danburton): improve this, although it should never happen
pluginShouldHaveRun :: Plugin -> GlobalOpts -> IO ()
pluginShouldHaveRun _plugin _globalOpts = do
  fail "Plugin should have run"

-- | Print out useful path information in a human-readable format (and
-- support others later).
pathCmd :: [Text] -> GlobalOpts -> IO ()
pathCmd keys go =
    withBuildConfig
        go
        ExecStrategy
        (do env <- ask
            let cfg = envConfig env
                bc = envConfigBuildConfig cfg
            menv <- getMinimalEnvOverride
            snap <- packageDatabaseDeps
            local <- packageDatabaseLocal
            snaproot <- installationRootDeps
            localroot <- installationRootLocal
            distDir <- distRelativeDir
            forM_
                (filter
                     (\(_,key,_) ->
                           null keys || elem key keys)
                     paths)
                (\(_,key,path) ->
                      $logInfo
                          ((if length keys == 1
                               then ""
                               else key <> ": ") <>
                           path
                               (PathInfo
                                    bc
                                    menv
                                    snap
                                    local
                                    snaproot
                                    localroot
                                    distDir))))

-- | Passed to all the path printers as a source of info.
data PathInfo = PathInfo
    {piBuildConfig :: BuildConfig
    ,piEnvOverride :: EnvOverride
    ,piSnapDb :: Path Abs Dir
    ,piLocalDb :: Path Abs Dir
    ,piSnapRoot :: Path Abs Dir
    ,piLocalRoot :: Path Abs Dir
    ,piDistDir :: Path Rel Dir
    }

-- | The paths of interest to a user. The first tuple string is used
-- for a description that the optparse flag uses, and the second
-- string as a machine-readable key and also for @--foo@ flags. The user
-- can choose a specific path to list like @--global-stack-root@. But
-- really it's mainly for the documentation aspect.
--
-- When printing output we generate @PathInfo@ and pass it to the
-- function to generate an appropriate string.  Trailing slashes are
-- removed, see #506
paths :: [(String, Text, PathInfo -> Text)]
paths =
    [ ( "Global stack root directory"
      , "global-stack-root"
      , \pi ->
             T.pack (toFilePathNoTrailing (configStackRoot (bcConfig (piBuildConfig pi)))))
    , ( "Project root (derived from stack.yaml file)"
      , "project-root"
      , \pi ->
             T.pack (toFilePathNoTrailing (bcRoot (piBuildConfig pi))))
    , ( "Configuration location (where the stack.yaml file is)"
      , "config-location"
      , \pi ->
             T.pack (toFilePathNoTrailing (bcStackYaml (piBuildConfig pi))))
    , ( "PATH environment variable"
      , "bin-path"
      , \pi ->
             T.pack (intercalate ":" (eoPath (piEnvOverride pi))))
    , ( "Installed GHCs (unpacked and archives)"
      , "ghc-paths"
      , \pi ->
             T.pack (toFilePathNoTrailing (configLocalPrograms (bcConfig (piBuildConfig pi)))))
    , ( "Local bin path where stack installs executables"
      , "local-bin-path"
      , \pi ->
             T.pack (toFilePathNoTrailing (configLocalBin (bcConfig (piBuildConfig pi)))))
    , ( "Extra include directories"
      , "extra-include-dirs"
      , \pi ->
             T.intercalate
                 ", "
                 (Set.elems (configExtraIncludeDirs (bcConfig (piBuildConfig pi)))))
    , ( "Extra library directories"
      , "extra-library-dirs"
      , \pi ->
             T.intercalate ", " (Set.elems (configExtraLibDirs (bcConfig (piBuildConfig pi)))))
    , ( "Snapshot package database"
      , "snapshot-pkg-db"
      , \pi ->
             T.pack (toFilePathNoTrailing (piSnapDb pi)))
    , ( "Local project package database"
      , "local-pkg-db"
      , \pi ->
             T.pack (toFilePathNoTrailing (piLocalDb pi)))
    , ( "Snapshot installation root"
      , "snapshot-install-root"
      , \pi ->
             T.pack (toFilePathNoTrailing (piSnapRoot pi)))
    , ( "Local project installation root"
      , "local-install-root"
      , \pi ->
             T.pack (toFilePathNoTrailing (piLocalRoot pi)))
    , ( "Dist work directory"
      , "dist-dir"
      , \pi ->
             T.pack (toFilePathNoTrailing (piDistDir pi)))]
  where toFilePathNoTrailing = dropTrailingPathSeparator . toFilePath

data SetupCmdOpts = SetupCmdOpts
    { scoGhcVersion :: !(Maybe Version)
    , scoForceReinstall :: !Bool
    }

setupParser :: Parser SetupCmdOpts
setupParser = SetupCmdOpts
    <$> (optional $ argument readVersion (metavar "VERSION"))
    <*> boolFlags False
            "reinstall"
            "Reinstall GHC, even if available (implies no-system-ghc)"
            idm
  where
    readVersion = do
        s <- readerAsk
        case parseVersionFromString s of
            Nothing -> readerError $ "Invalid version: " ++ s
            Just x -> return x

setupCmd :: SetupCmdOpts -> GlobalOpts -> IO ()
setupCmd SetupCmdOpts{..} go@GlobalOpts{..} = do
  (manager,lc) <- loadConfigWithOpts go
  runStackT manager globalLogLevel (lcConfig lc) globalTerminal $
      Docker.reexecWithOptionalContainer
          (lcProjectRoot lc)
          (runStackLoggingT manager globalLogLevel globalTerminal $ do
              (ghc, mstack) <-
                  case scoGhcVersion of
                      Just v -> return (v, Nothing)
                      Nothing -> do
                          bc <- lcLoadBuildConfig lc globalResolver ExecStrategy
                          return (bcGhcVersionExpected bc, Just $ bcStackYaml bc)
              mpaths <- runStackT manager globalLogLevel (lcConfig lc) globalTerminal $ ensureGHC SetupOpts
                  { soptsInstallIfMissing = True
                  , soptsUseSystem =
                    (configSystemGHC $ lcConfig lc)
                    && not scoForceReinstall
                  , soptsExpected = ghc
                  , soptsStackYaml = mstack
                  , soptsForceReinstall = scoForceReinstall
                  , soptsSanityCheck = True
                  , soptsSkipGhcCheck = False
                  , soptsSkipMsys = configSkipMsys $ lcConfig lc
                  }
              case mpaths of
                  Nothing -> $logInfo "GHC on PATH would be used"
                  Just ps -> $logInfo $ "Would add the following to PATH: "
                      <> T.pack (intercalate [searchPathSeparator] ps)
                  )

withConfig :: GlobalOpts
           -> StackT Config IO ()
           -> IO ()
withConfig go@GlobalOpts{..} inner = do
    (manager, lc) <- loadConfigWithOpts go
    runStackT manager globalLogLevel (lcConfig lc) globalTerminal $
        Docker.reexecWithOptionalContainer (lcProjectRoot lc) $
            runStackT manager globalLogLevel (lcConfig lc) globalTerminal
                inner

withBuildConfig :: GlobalOpts
                -> NoBuildConfigStrategy
                -> StackT EnvConfig IO ()
                -> IO ()
withBuildConfig go@GlobalOpts{..} strat inner = do
    (manager, lc) <- loadConfigWithOpts go
    runStackT manager globalLogLevel (lcConfig lc) globalTerminal $
        Docker.reexecWithOptionalContainer (lcProjectRoot lc) $ do
            bconfig <- runStackLoggingT manager globalLogLevel globalTerminal $
                lcLoadBuildConfig lc globalResolver strat
            envConfig <-
                runStackT
                    manager globalLogLevel bconfig globalTerminal
                    setupEnv
            runStackT
                manager
                globalLogLevel
                envConfig
                globalTerminal
                inner

cleanCmd :: () -> GlobalOpts -> IO ()
cleanCmd () go = withBuildConfig go ThrowException clean

-- | Parser for package names
readPackageName :: ReadM PackageName
readPackageName = do
    s <- readerAsk
    case parsePackageNameFromString s of
        Nothing -> readerError $ "Invalid package name: " ++ s
        Just x -> return x

-- | Parser for package:[-]flag
readFlag :: ReadM (Map (Maybe PackageName) (Map FlagName Bool))
readFlag = do
    s <- readerAsk
    case break (== ':') s of
        (pn, ':':mflag) -> do
            pn' <-
                case parsePackageNameFromString pn of
                    Nothing
                        | pn == "*" -> return Nothing
                        | otherwise -> readerError $ "Invalid package name: " ++ pn
                    Just x -> return $ Just x
            let (b, flagS) =
                    case mflag of
                        '-':x -> (False, x)
                        _ -> (True, mflag)
            flagN <-
                case parseFlagNameFromString flagS of
                    Nothing -> readerError $ "Invalid flag name: " ++ flagS
                    Just x -> return x
            return $ Map.singleton pn' $ Map.singleton flagN b
        _ -> readerError "Must have a colon"

-- | Helper for build and install commands
buildCmdHelper :: NoBuildConfigStrategy -> FinalAction -> BuildOpts -> GlobalOpts -> IO ()
buildCmdHelper strat finalAction opts go
    | boptsFileWatch opts = fileWatch inner
    | otherwise = inner $ const $ return ()
  where
    inner setLocalFiles =
        withBuildConfig go strat $
        Stack.Build.build setLocalFiles opts { boptsFinalAction = finalAction }

-- | Build the project.
buildCmd :: FinalAction -> BuildOpts -> GlobalOpts -> IO ()
buildCmd = buildCmdHelper ThrowException

-- | Install
installCmd :: BuildOpts -> GlobalOpts -> IO ()
installCmd opts = buildCmdHelper ExecStrategy DoNothing opts { boptsInstallExes = True }

-- | Unpack packages to the filesystem
unpackCmd :: [String] -> GlobalOpts -> IO ()
unpackCmd names go = withConfig go $ do
    menv <- getMinimalEnvOverride
    Stack.Fetch.unpackPackages menv "." names

-- | Update the package index
updateCmd :: () -> GlobalOpts -> IO ()
updateCmd () go = withConfig go $
    getMinimalEnvOverride >>= Stack.PackageIndex.updateAllIndices

upgradeCmd :: Bool -> GlobalOpts -> IO ()
upgradeCmd fromGit go = withConfig go $
    upgrade fromGit (globalResolver go)

-- | Upload to Hackage
uploadCmd :: [String] -> GlobalOpts -> IO ()
uploadCmd args go = do
    (manager,lc) <- loadConfigWithOpts go
    let config = lcConfig lc
    if null args
        then error "To upload the current project, please run 'stack upload .'"
        else liftIO $ do
            uploader <- Upload.mkUploader
                  config
                $ Upload.setGetManager (return manager)
                  Upload.defaultUploadSettings
            mapM_ (Upload.upload uploader) args

packagesParser :: Parser [String]
packagesParser = many (strOption (long "package" <> help "Additional packages that must be installed"))

data ExecOpts = ExecOpts
    { eoCmd :: !String
    , eoArgs :: ![String]
    , eoExtra :: !ExecOptsExtra
    }

data ExecOptsExtra
    = ExecOptsPlain
    | ExecOptsEmbellished
        { eoEnvSettings :: !EnvSettings
        , eoPackages :: ![String]
        }

execOptsParser :: Maybe String -- ^ command
               -> Parser ExecOpts
execOptsParser mcmd =
    ExecOpts
        <$> maybe eoCmdParser pure mcmd
        <*> eoArgsParser
        <*> (eoPlainParser <|>
             ExecOptsEmbellished
                <$> eoEnvSettingsParser
                <*> eoPackagesParser)
  where
    eoCmdParser :: Parser String
    eoCmdParser = strArgument (metavar "CMD")

    eoArgsParser :: Parser [String]
    eoArgsParser = many (strArgument (metavar "-- ARGS (e.g. stack ghc -- X.hs -o x)"))

    eoEnvSettingsParser :: Parser EnvSettings
    eoEnvSettingsParser = EnvSettings
        <$> pure True
        <*> boolFlags True
                "ghc-package-path"
                "setting the GHC_PACKAGE_PATH variable for the subprocess"
                idm
        <*> boolFlags True
                "stack-exe"
                "setting the STACK_EXE environment variable to the path for the stack executable"
                idm

    eoPackagesParser :: Parser [String]
    eoPackagesParser = packagesParser

    eoPlainParser :: Parser ExecOptsExtra
    eoPlainParser = flag' ExecOptsPlain
                          (long "plain" <>
                           help "Use an unmodified environment (only useful with Docker)")

-- | Execute a command.
execCmd :: ExecOpts -> GlobalOpts -> IO ()
execCmd ExecOpts {..} go@GlobalOpts{..} =
    case eoExtra of
        ExecOptsPlain -> do
            (manager,lc) <- liftIO $ loadConfigWithOpts go
            runStackT manager globalLogLevel (lcConfig lc) globalTerminal $
                Docker.execWithOptionalContainer
                    (lcProjectRoot lc)
                    (return (eoCmd, eoArgs, id)) $
                    runStackT manager globalLogLevel (lcConfig lc) globalTerminal $
                        exec plainEnvSettings eoCmd eoArgs
        ExecOptsEmbellished {..} ->
           withBuildConfig go ExecStrategy $ do
               let targets = concatMap words eoPackages
               unless (null targets) $
                   Stack.Build.build (const $ return ()) defaultBuildOpts
                       { boptsTargets = map T.pack targets
                       }
               exec eoEnvSettings eoCmd eoArgs

-- | Run the REPL in the context of a project.
replCmd :: ([Text], [String], FilePath, Bool, [String]) -> GlobalOpts -> IO ()
replCmd (targets,args,path,noload,packages) go@GlobalOpts{..} = do
  withBuildConfig go ExecStrategy $ do
    let packageTargets = concatMap words packages
    unless (null packageTargets) $
       Stack.Build.build (const $ return ()) defaultBuildOpts
           { boptsTargets = map T.pack packageTargets
           }
    repl targets args path noload

-- | Run ide-backend in the context of a project.
ideCmd :: ([Text], [String]) -> GlobalOpts -> IO ()
ideCmd (targets,args) go@GlobalOpts{..} = withBuildConfig go ExecStrategy $ do
      ide targets args

-- | Pull the current Docker image.
dockerPullCmd :: () -> GlobalOpts -> IO ()
dockerPullCmd _ go@GlobalOpts{..} = do
    (manager,lc) <- liftIO $ loadConfigWithOpts go
    runStackT manager globalLogLevel (lcConfig lc) globalTerminal $
        Docker.preventInContainer Docker.pull

-- | Reset the Docker sandbox.
dockerResetCmd :: Bool -> GlobalOpts -> IO ()
dockerResetCmd keepHome go@GlobalOpts{..} = do
    (manager,lc) <- liftIO (loadConfigWithOpts go)
    runStackLoggingT manager globalLogLevel globalTerminal$ Docker.preventInContainer $
        Docker.reset (lcProjectRoot lc) keepHome

-- | Cleanup Docker images and containers.
dockerCleanupCmd :: Docker.CleanupOpts -> GlobalOpts -> IO ()
dockerCleanupCmd cleanupOpts go@GlobalOpts{..} = do
    (manager,lc) <- liftIO $ loadConfigWithOpts go
    runStackT manager globalLogLevel (lcConfig lc) globalTerminal $
        Docker.preventInContainer $
            Docker.cleanup cleanupOpts

-- | Command sum type for conditional arguments.
data Command
    = Build
    | Test
    | Haddock
    | Bench
    deriving (Eq)

-- | Parser for test arguments.
testOpts :: Parser TestOpts
testOpts = TestOpts
       <$> boolFlags True
                     "rerun-tests"
                     "running already successful tests"
                     idm
       <*> argsOption(long "test-arguments" <>
                      metavar "TEST_ARGS" <>
                      help "Arguments passed in to the test suite program" <>
                      value [])
      <*> flag False
               True
               (long "coverage" <>
               help "Generate a code coverage report")
      <*> flag False
               True
               (long "no-run-tests" <>
                help "Disable running of tests. (Tests will still be built.)")



-- | Parser for bench arguments.
benchOpts :: Parser BenchmarkOpts
benchOpts = BenchmarkOpts
        <$> optional (strOption (long "benchmark-arguments" <>
                                 metavar "BENCH_ARGS" <>
                                 help ("Forward BENCH_ARGS to the benchmark suite. " <>
                                       "Supports templates from `cabal bench`")))

-- | Parser for build arguments.
buildOpts :: Command -> Parser BuildOpts
buildOpts cmd =
            BuildOpts <$> target <*> libProfiling <*> exeProfiling <*>
            optimize <*> haddock <*> haddockDeps <*> finalAction <*> dryRun <*> ghcOpts <*>
            flags <*> installExes <*> preFetch <*> onlySnapshot <*>
            fileWatch' <*> keepGoing
  where optimize =
          maybeBoolFlags "optimizations" "optimizations for TARGETs and all its dependencies" idm
        target =
          fmap (map T.pack)
               (many (strArgument
                        (metavar "TARGET" <>
                         help "If none specified, use all packages defined in current directory")))
        libProfiling =
          boolFlags False
                    "library-profiling"
                    "library profiling for TARGETs and all its dependencies"
                    idm
        exeProfiling =
          boolFlags False
                    "executable-profiling"
                    "library profiling for TARGETs and all its dependencies"
                    idm
        haddock =
          boolFlags (cmd == Haddock)
                    "haddock"
                    "building Haddocks"
                    idm
        haddockDeps =
          if cmd == Haddock
             then maybeBoolFlags
                            "haddock-deps"
                            "building Haddocks for dependencies"
                            idm
             else pure Nothing
        finalAction = pure DoNothing
        installExes = pure False
        dryRun = flag False True (long "dry-run" <>
                                  help "Don't build anything, just prepare to")
        ghcOpts = (++)
          <$> flag [] ["-Wall", "-Werror"]
              ( long "pedantic"
             <> help "Turn on -Wall and -Werror (note: option name may change in the future"
              )
          <*> many (fmap T.pack
                     (strOption (long "ghc-options" <>
                                 metavar "OPTION" <>
                                 help "Additional options passed to GHC")))

        flags =
          fmap (Map.unionsWith Map.union) $ many
            (option readFlag
                ( long "flag"
               <> metavar "PACKAGE:[-]FLAG"
               <> help "Override flags set in stack.yaml (applies to local packages and extra-deps)"
                ))

        preFetch = flag False True
            (long "prefetch" <>
             help "Fetch packages necessary for the build immediately, useful with --dry-run")
        onlySnapshot = flag False True
            (long "only-snapshot" <>
             help "Only build packages for the snapshot database, not the local database")

        fileWatch' = flag False True
            (long "file-watch" <>
             help "Watch for changes in local files and automatically rebuild")

        keepGoing = maybeBoolFlags
            "keep-going"
            "continue running after a step fails (default: false for build, true for test/bench)"
            idm

-- | Parser for docker cleanup arguments.
dockerCleanupOpts :: Parser Docker.CleanupOpts
dockerCleanupOpts =
  Docker.CleanupOpts <$>
  (flag' Docker.CleanupInteractive
         (short 'i' <>
          long "interactive" <>
          help "Show cleanup plan in editor and allow changes (default)") <|>
   flag' Docker.CleanupImmediate
         (short 'y' <>
          long "immediate" <>
          help "Immediately execute cleanup plan") <|>
   flag' Docker.CleanupDryRun
         (short 'n' <>
          long "dry-run" <>
          help "Display cleanup plan but do not execute") <|>
   pure Docker.CleanupInteractive) <*>
  opt (Just 14) "known-images" "LAST-USED" <*>
  opt Nothing "unknown-images" "CREATED" <*>
  opt (Just 0) "dangling-images" "CREATED" <*>
  opt Nothing "stopped-containers" "CREATED" <*>
  opt Nothing "running-containers" "CREATED"
  where opt def' name mv =
          fmap Just
               (option auto
                       (long name <>
                        metavar (mv ++ "-DAYS-AGO") <>
                        help ("Remove " ++
                              toDescr name ++
                              " " ++
                              map toLower (toDescr mv) ++
                              " N days ago" ++
                              case def' of
                                Just n -> " (default " ++ show n ++ ")"
                                Nothing -> ""))) <|>
          flag' Nothing
                (long ("no-" ++ name) <>
                 help ("Do not remove " ++
                       toDescr name ++
                       case def' of
                         Just _ -> ""
                         Nothing -> " (default)")) <|>
          pure def'
        toDescr = map (\c -> if c == '-' then ' ' else c)

-- | Parser for global command-line options.
globalOpts :: Bool -> Parser GlobalOpts
globalOpts defaultTerminal =
    GlobalOpts <$> logLevelOpt <*>
    configOptsParser False <*>
    optional resolverParser <*>
    flag
        defaultTerminal
        False
        (long "no-terminal" <>
         help
             "Override terminal detection in the case of running in a false terminal") <*>
    (optional (strOption
        (long "stack-yaml" <>
         metavar "STACK-YAML" <>
         help "Override project stack.yaml file (overrides any STACK_YAML environment variable)")))

-- | Parse for a logging level.
logLevelOpt :: Parser LogLevel
logLevelOpt =
  fmap parse
       (strOption (long "verbosity" <>
                   metavar "VERBOSITY" <>
                   help "Verbosity: silent, error, warn, info, debug")) <|>
  flag defaultLogLevel
       verboseLevel
       (short 'v' <> long "verbose" <>
        help ("Enable verbose mode: verbosity level \"" <> showLevel verboseLevel <> "\""))
  where verboseLevel = LevelDebug
        showLevel l =
          case l of
            LevelDebug -> "debug"
            LevelInfo -> "info"
            LevelWarn -> "warn"
            LevelError -> "error"
            LevelOther x -> T.unpack x
        parse s =
          case s of
            "debug" -> LevelDebug
            "info" -> LevelInfo
            "warn" -> LevelWarn
            "error" -> LevelError
            _ -> LevelOther (T.pack s)

resolverParser :: Parser Resolver
resolverParser =
    option readResolver
        (long "resolver" <>
         metavar "RESOLVER" <>
         help "Override resolver in project file")

-- | Default logging level should be something useful but not crazy.
defaultLogLevel :: LogLevel
defaultLogLevel = LevelInfo

-- | Parsed global command-line options.
data GlobalOpts = GlobalOpts
    { globalLogLevel     :: LogLevel -- ^ Log level
    , globalConfigMonoid :: ConfigMonoid -- ^ Config monoid, for passing into 'loadConfig'
    , globalResolver     :: Maybe Resolver -- ^ Resolver override
    , globalTerminal     :: Bool -- ^ We're in a terminal?
    , globalStackYaml    :: Maybe FilePath -- ^ Override project stack.yaml
    } deriving (Show)

-- | Load the configuration with a manager. Convenience function used
-- throughout this module.
loadConfigWithOpts :: GlobalOpts -> IO (Manager,LoadConfig (StackLoggingT IO))
loadConfigWithOpts GlobalOpts{..} = do
    manager <- newTLSManager
    mstackYaml <-
        case globalStackYaml of
            Nothing -> return Nothing
            Just fp -> do
                path <- canonicalizePath fp >>= parseAbsFile
                return $ Just path
    lc <- runStackLoggingT
              manager
              globalLogLevel
              globalTerminal
              (loadConfig globalConfigMonoid mstackYaml)
    return (manager,lc)

-- | Project initialization
initCmd :: InitOpts -> GlobalOpts -> IO ()
initCmd initOpts go = withConfig go $ initProject initOpts

-- | Project creation
newCmd :: InitOpts -> GlobalOpts -> IO ()
newCmd initOpts go@GlobalOpts{..} = withConfig go $ do
    newProject
    initProject initOpts

-- | Fix up extra-deps for a project
solverCmd :: Bool -- ^ modify stack.yaml automatically?
          -> GlobalOpts
          -> IO ()
solverCmd fixStackYaml go =
    withBuildConfig go ThrowException (solveExtraDeps fixStackYaml)

-- | Parser for @solverCmd@
solverOptsParser :: Parser Bool
solverOptsParser = boolFlags False
    "modify-stack-yaml"
    "Automatically modify stack.yaml with the solver's recommendations"
    idm

-- | Visualize dependencies
dotCmd :: DotOpts -> GlobalOpts -> IO ()
dotCmd dotOpts go = withBuildConfig go ThrowException (dot dotOpts)
