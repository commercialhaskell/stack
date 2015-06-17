{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Main stack tool entry point.

module Main where

import           Control.Exception
import           Control.Monad (join, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader (asks, runReaderT)
import           Data.Char (toLower)
import           Data.List
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network.HTTP.Client
import           Network.HTTP.Client.Conduit (getHttpManager)
import           Options.Applicative.Args
import           Options.Applicative.Builder.Extra
import           Options.Applicative.Simple
import           Options.Applicative.Types (readerAsk)
import           Path (toFilePath)
import qualified Paths_stack as Meta
import           Plugins
import           Stack.Build
import           Stack.Build.Types
import           Stack.Config
import           Stack.Constants
import qualified Stack.Docker as Docker
import           Stack.Exec
import           Stack.Fetch
import           Stack.GhcPkg (getCabalPkgVer)
import           Stack.Init
import           Stack.New
import qualified Stack.PackageIndex
import           Stack.Path
import           Stack.Repl
import           Stack.Setup
import           Stack.Types
import           Stack.Types.StackT
import qualified Stack.Upload as Upload
import           System.Environment (getArgs, getProgName)
import           System.Exit
import           System.FilePath (searchPathSeparator)
import           System.IO (stderr)
import qualified System.Process.Read

-- | Commandline dispatcher.
main :: IO ()
main =
  do when False $ do -- https://github.com/commercialhaskell/stack/issues/322
       plugins <- findPlugins (T.pack stackProgName)
       tryRunPlugin plugins
     progName <- getProgName
     args <- getArgs
     execExtraHelp args
                   dockerHelpOptName
                   (Docker.dockerOptsParser True)
                   ("Only showing --" ++ Docker.dockerCmdName ++ "* options.")
     (level,run) <-
       simpleOptions
         $(simpleVersion Meta.version)
         "stack - The Haskell Tool Stack"
         ""
         (extraHelpOption progName (Docker.dockerCmdName ++ "*") dockerHelpOptName <*> globalOpts)
         (do addCommand "build"
                        "Build the project(s) in this directory/configuration"
                        (buildCmd DoNothing)
                        buildOpts
             addCommand "install"
                        "Build executables and install to a user path"
                        installCmd
                        buildOpts
             addCommand "test"
                        "Build and test the project(s) in this directory/configuration"
                        (buildCmd DoTests)
                        buildOpts
             addCommand "bench"
                        "Build and benchmark the project(s) in this directory/configuration"
                        (buildCmd DoBenchmarks)
                        buildOpts
             addCommand "haddock"
                        "Generate haddocks for the project(s) in this directory/configuration"
                        (buildCmd DoHaddock)
                        buildOpts
             addCommand "new"
                        "Create a brand new project"
                        (\_ _ -> newProject)
                        (pure ())
             addCommand "init"
                        "Initialize a stack project based on one or more cabal packages"
                        initCmd
                        initOptsParser
             addCommand "setup"
                        "Get the appropriate ghc for your project"
                        setupCmd
                        setupParser
             addCommand "unpack"
                        "Unpack one or more packages locally"
                        unpackCmd
                        (some $ strArgument $ metavar "PACKAGE")
             addCommand "update"
                        "Update the package index"
                        updateCmd
                        (pure ())
             addCommand "upload"
                        "Upload a package to Hackage"
                        uploadCmd
                        (many $ strArgument $ metavar "TARBALL/DIR")
             addCommand "exec"
                        "Execute a command"
                        execCmd
                        ((,)
                            <$> strArgument (metavar "CMD")
                            <*> many (strArgument (metavar "-- ARGS (e.g. stack exec -- ghc --version)")))
             addCommand "ghc"
                        "Run ghc"
                        execCmd
                        ((,)
                            <$> pure "ghc"
                            <*> many (strArgument (metavar "-- ARGS (e.g. stack ghc -- X.hs -o x)")))
             addCommand "ghci"
                        "Run ghci in the context of project(s)"
                        replCmd
                        ((,) <$>
                         fmap (map T.pack)
                              (many (strArgument
                                       (metavar "TARGET" <>
                                        help "If none specified, use all packages defined in current directory"))) <*>
                         many (strOption (long "ghc-options" <>
                                          metavar "OPTION" <>
                                          help "Additional options passed to GHCi")))
             addCommand "runghc"
                        "Run runghc"
                        execCmd
                        ((,)
                            <$> pure "runghc"
                            <*> many (strArgument (metavar "-- ARGS (e.g. stack runghc -- X.hs)")))
             addCommand "clean"
                        "Clean the local packages"
                        cleanCmd
                        (pure ())
             addSubCommands
               "path"
               "Print path information for certain things"
               (do addCommand "ghc"
                              "Print path to the ghc executable in use"
                              pathCmd
                              (pure PathGhc)
                   addCommand "log"
                              "Print path to the log directory in use"
                              pathCmd
                              (pure PathLog)
                   addCommand "package-db"
                              "Print the package databases in use"
                              pathCmd
                              (pure PathPackageDb))
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
                              dockerCleanupOpts
                   addCommand "exec"
                              "Execute a command in a Docker container without setting up Haskell environment first"
                              dockerExecCmd
                              ((,) <$> strArgument (metavar "[--] CMD")
                                   <*> many (strArgument (metavar "ARGS"))))
             )
             -- commandsFromPlugins plugins pluginShouldHaveRun) https://github.com/commercialhaskell/stack/issues/322
     run level `catch` \e -> do
        -- This special handler stops "stack: " from being printed before the
        -- exception
        case fromException e of
            Just ec -> exitWith ec
            Nothing -> do
                print e
                exitFailure
  where
    dockerHelpOptName = Docker.dockerCmdName ++ "-help"


pathCmd :: PathArg -> GlobalOpts -> IO ()
pathCmd pathArg go@GlobalOpts{..} = do
  (manager,lc) <- loadConfigWithOpts go
  buildConfig <- runStackLoggingT manager globalLogLevel globalTerminal
    (lcLoadBuildConfig lc globalResolver ExecStrategy)
  runStackT manager globalLogLevel buildConfig globalTerminal (pathString pathArg) >>= putStrLn

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
  runStackLoggingT manager globalLogLevel globalTerminal $
      Docker.rerunWithOptionalContainer
          (lcConfig lc)
          (lcProjectRoot lc)
          (runStackLoggingT manager globalLogLevel globalTerminal $ do
              (ghc, mstack) <-
                  case scoGhcVersion of
                      Just v -> return (v, Nothing)
                      Nothing -> do
                          bc <- lcLoadBuildConfig lc globalResolver ThrowException
                          return (bcGhcVersion bc, Just $ bcStackYaml bc)
              mpaths <- runStackT manager globalLogLevel (lcConfig lc) globalTerminal $ ensureGHC SetupOpts
                  { soptsInstallIfMissing = True
                  , soptsUseSystem =
                    (configSystemGHC $ lcConfig lc)
                    && not scoForceReinstall
                  , soptsExpected = ghc
                  , soptsStackYaml = mstack
                  , soptsForceReinstall = scoForceReinstall
                  , soptsSanityCheck = True
                  }
              case mpaths of
                  Nothing -> $logInfo "GHC on PATH would be used"
                  Just paths -> $logInfo $ "Would add the following to PATH: "
                      <> T.pack (intercalate [searchPathSeparator] paths)
                  )

withBuildConfig :: GlobalOpts
                -> NoBuildConfigStrategy
                -> StackT EnvConfig IO ()
                -> IO ()
withBuildConfig go@GlobalOpts{..} strat inner = do
    (manager, lc) <- loadConfigWithOpts go
    runStackLoggingT manager globalLogLevel globalTerminal $
        Docker.rerunWithOptionalContainer (lcConfig lc) (lcProjectRoot lc) $ do
            bconfig1 <- runStackLoggingT manager globalLogLevel globalTerminal $
                lcLoadBuildConfig lc globalResolver strat
            (bconfig2,cabalVer) <-
                runStackT
                    manager globalLogLevel bconfig1 globalTerminal
                    (do cfg <- setupEnv
                        menv <- runReaderT getMinimalEnvOverride cfg
                        cabalVer <- getCabalPkgVer menv
                        return (cfg,cabalVer))
            runStackT
                manager
                globalLogLevel
                (EnvConfig bconfig2 cabalVer)
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
readFlag :: ReadM (Map PackageName (Map FlagName Bool))
readFlag = do
    s <- readerAsk
    case break (== ':') s of
        (pn, ':':mflag) -> do
            pn' <-
                case parsePackageNameFromString pn of
                    Nothing -> readerError $ "Invalid package name: " ++ pn
                    Just x -> return x
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

-- | Build the project.
buildCmd :: FinalAction -> BuildOpts -> GlobalOpts -> IO ()
buildCmd finalAction opts go@GlobalOpts{..} = withBuildConfig go ThrowException $
    Stack.Build.build opts { boptsFinalAction = finalAction }

-- | Install
installCmd :: BuildOpts -> GlobalOpts -> IO ()
installCmd opts go@GlobalOpts{..} = withBuildConfig go ExecStrategy $
    Stack.Build.build opts { boptsInstallExes = True }

-- | Unpack packages to the filesystem
unpackCmd :: [String] -> GlobalOpts -> IO ()
unpackCmd names go@GlobalOpts{..} = do
    (manager,lc) <- loadConfigWithOpts go
    runStackLoggingT manager globalLogLevel globalTerminal $
        Docker.rerunWithOptionalContainer (lcConfig lc) (lcProjectRoot lc) $
            runStackT manager globalLogLevel (lcConfig lc) globalTerminal $ do
                menv <- getMinimalEnvOverride
                Stack.Fetch.unpackPackages menv "." names

-- | Update the package index
updateCmd :: () -> GlobalOpts -> IO ()
updateCmd () go@GlobalOpts{..} = do
    (manager,lc) <- loadConfigWithOpts go
    runStackLoggingT manager globalLogLevel globalTerminal $
        Docker.rerunWithOptionalContainer (lcConfig lc) (lcProjectRoot lc) $
            runStackT manager globalLogLevel (lcConfig lc) globalTerminal $
                getMinimalEnvOverride >>= Stack.PackageIndex.updateAllIndices

-- | Upload to Hackage
uploadCmd :: [String] -> GlobalOpts -> IO ()
uploadCmd args0 go = withBuildConfig go ExecStrategy $ do
    let args = if null args0 then ["."] else args0
    config <- asks getConfig
    manager <- asks getHttpManager
    menv <- getMinimalEnvOverride
    runghc <- join $ System.Process.Read.findExecutable menv "runghc"
    liftIO $ do
        uploader <- Upload.mkUploader
              (toFilePath runghc)
              config
            $ Upload.setGetManager (return manager)
              Upload.defaultUploadSettings
        mapM_ (Upload.upload uploader) args

-- | Execute a command.
execCmd :: (String, [String]) -> GlobalOpts -> IO ()
execCmd (cmd,args) go@GlobalOpts{..} =
    withBuildConfig go ExecStrategy $
    exec cmd args

-- | Run the REPL in the context of a project, with
replCmd :: ([Text], [String]) -> GlobalOpts -> IO ()
replCmd (targets,args) go@GlobalOpts{..} = withBuildConfig go ExecStrategy $ do
      repl targets args

-- | Pull the current Docker image.
dockerPullCmd :: () -> GlobalOpts -> IO ()
dockerPullCmd _ go@GlobalOpts{..} = do
    (manager,lc) <- liftIO $ loadConfigWithOpts go
    runStackLoggingT manager globalLogLevel globalTerminal $ Docker.preventInContainer $
        Docker.pull (lcConfig lc)

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
    runStackLoggingT manager globalLogLevel globalTerminal$ Docker.preventInContainer $
        Docker.cleanup (lcConfig lc) cleanupOpts

-- | Execute a command
dockerExecCmd :: (String, [String]) -> GlobalOpts -> IO ()
dockerExecCmd (cmd,args) go@GlobalOpts{..} = do
    (manager,lc) <- liftIO $ loadConfigWithOpts go
    runStackLoggingT manager globalLogLevel globalTerminal$ Docker.preventInContainer $
        Docker.rerunCmdWithRequiredContainer (lcConfig lc)
                                             (lcProjectRoot lc)
                                             (return (cmd,args,lcConfig lc))

-- | Parser for build arguments.
buildOpts :: Parser BuildOpts
buildOpts =
            BuildOpts <$> target <*> libProfiling <*> exeProfiling <*>
            optimize <*> finalAction <*> dryRun <*> ghcOpts <*> flags <*>
            installExes <*> preFetch <*> testArgs <*> onlySnapshot
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
        testArgs =
             fmap (fromMaybe [])
                  (optional
                       (argsOption
                            (long "test-arguments" <> metavar "TEST_ARGS" <>
                             help "Arguments passed in to the test suite program")))

        onlySnapshot = flag False True
            (long "only-snapshot" <>
             help "Only build packages for the snapshot database, not the local database")

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
globalOpts :: Parser GlobalOpts
globalOpts =
    GlobalOpts <$> logLevelOpt <*>
    configOptsParser False <*>
    optional resolverParser <*>
    flag
        True
        False
        (long "no-terminal" <>
         help
             "Override terminal detection in the case of running in a false terminal")

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
    } deriving (Show)

-- | Load the configuration with a manager. Convenience function used
-- throughout this module.
loadConfigWithOpts :: GlobalOpts -> IO (Manager,LoadConfig (StackLoggingT IO))
loadConfigWithOpts GlobalOpts{..} = do
    manager <- newTLSManager
    lc <- runStackLoggingT
              manager
              globalLogLevel
              globalTerminal
              (loadConfig globalConfigMonoid)
    return (manager,lc)

-- | Project initialization
initCmd :: InitOpts -> GlobalOpts -> IO ()
initCmd initOpts go@GlobalOpts{..} = do
  (manager,lc) <- loadConfigWithOpts go
  runStackLoggingT manager globalLogLevel globalTerminal $
        Docker.rerunWithOptionalContainer (lcConfig lc) (lcProjectRoot lc) $
            runStackT manager globalLogLevel (lcConfig lc) globalTerminal $
                initProject globalResolver initOpts
