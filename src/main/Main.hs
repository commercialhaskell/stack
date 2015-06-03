{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Main stack tool entry point.

module Main where

import           Control.Exception
import           Control.Monad (join)
import           Control.Monad.Logger
import           Data.Char (toLower)
import           Data.List
import qualified Data.List as List
import           Data.Maybe (isJust)
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Distribution.Text (display)
import           Options.Applicative.Builder.Extra
import           Options.Applicative.Simple
import           Options.Applicative.Types (readerAsk)
import           Path (toFilePath)
import qualified Paths_stack as Meta
import           Plugins
import           Plugins.Commands
import           Stack.Build
import           Stack.Build.Types
import           Stack.Config
import qualified Stack.Docker as Docker
import           Stack.Fetch
import           Stack.GhcPkg (envHelper)
import           Stack.Package
import qualified Stack.PackageIndex
import           Stack.Path
import           Stack.Setup (setupEnv)
import           Stack.Types
import           Stack.Types.StackT
import           System.Environment (getArgs)
import           System.Exit
import           System.IO (stderr)
import qualified System.Process as P
import qualified System.Process.Read

-- | Commandline dispatcher.
main :: IO ()
main =
  do plugins <- findPlugins "stack"
     tryRunPlugin plugins
     Docker.checkVersions
     (level,run) <-
       simpleOptions
         $(simpleVersion Meta.version)
         "stack - The Haskell Tool Stack"
         ""
         globalOpts
         (do addCommand "build"
                        "Build the project(s) in this directory/configuration"
                        (buildCmd DoNothing)
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
             addCommand "setup"
                        "Get the appropriate ghc for your project"
                        (const setupCmd)
                        (pure ())
             addCommand "unpack"
                        "Unpack one or more packages locally"
                        unpackCmd
                        (some $ strArgument $ metavar "PACKAGE")
             addCommand "update"
                        "Update the package index"
                        updateCmd
                        (pure ())
             addCommand "exec"
                        "Execute a command"
                        execCmd
                        ((,)
                            <$> strArgument (metavar "[--] CMD")
                            <*> many (strArgument (metavar "ARGS")))
             addCommand "ghc"
                        "Run ghc"
                        execCmd
                        ((,)
                            <$> pure "ghc"
                            <*> many (strArgument (metavar "ARGS")))
             addCommand "ghci"
                        "Run ghci"
                        execCmd
                        ((,)
                            <$> pure "ghci"
                            <*> many (strArgument (metavar "ARGS")))
             addCommand "runghc"
                        "Run runghc"
                        execCmd
                        ((,)
                            <$> pure "runghc"
                            <*> many (strArgument (metavar "ARGS")))
             addCommand "clean"
                        "Clean the local packages"
                        cleanCmd
                        (pure ())
             addCommand "deps"
                        "Install dependencies"
                        depsCmd
                        ((,)
                            <$> (some (argument readPackageName
                                        (metavar "[PACKAGES]")))
                            <*> (flag False True (long "dry-run" <>
                                                  help "Don't build anything, just prepare to")))
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
                   addCommand "cleanup"
                              "Clean up Docker images and containers"
                              dockerCleanupCmd
                              dockerCleanupOpts
                   addCommand "exec"
                              "Execute a command in a Docker container without setting up Haskell environment first"
                              dockerExecCmd
                              ((,) <$> strArgument (metavar "[--] CMD")
                                   <*> many (strArgument (metavar "ARGS"))))
             commandsFromPlugins plugins pluginShouldHaveRun)
     run level

pathCmd :: PathArg -> GlobalOpts -> IO ()
pathCmd pathArg go@GlobalOpts{..} = do
  (manager,lc) <- loadConfigWithOpts go
  buildConfig <- runStackLoggingT manager globalLogLevel (lcLoadBuildConfig lc)
  runStackT manager globalLogLevel buildConfig (pathString pathArg) >>= putStrLn


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


setupCmd :: GlobalOpts -> IO ()
setupCmd go@GlobalOpts{..} = do
  (manager,lc) <- loadConfigWithOpts go
  Docker.rerunWithOptionalContainer
    (lcConfig lc)
    (lcProjectRoot lc)
    (do _ <- runStackLoggingT manager
                              globalLogLevel
                              (lcLoadBuildConfig lc >>= setupEnv True manager)
        return ())

cleanCmd :: () -> GlobalOpts -> IO ()
cleanCmd _ go@GlobalOpts{..} = do
  (manager,lc) <- loadConfigWithOpts go
  Docker.rerunWithOptionalContainer
    (lcConfig lc)
    (lcProjectRoot lc)
    (do config <- runStackLoggingT manager
                                   globalLogLevel
                                   (lcLoadBuildConfig lc >>= setupEnv False manager)
        runStackT manager globalLogLevel config clean)

-- | Install dependencies
depsCmd :: ([PackageName], Bool) -> GlobalOpts -> IO ()
depsCmd (names, dryRun) go@GlobalOpts{..} = do
    (manager,lc) <- loadConfigWithOpts go
    Docker.rerunWithOptionalContainer (lcConfig lc) (lcProjectRoot lc) $ do
        config <- runStackLoggingT manager globalLogLevel
            (lcLoadBuildConfig lc >>= setupEnv False manager)
        runStackT manager globalLogLevel config $ Stack.Build.build BuildOpts
            { boptsTargets = Right names
            , boptsLibProfile = False
            , boptsExeProfile = False
            , boptsEnableOptimizations = Nothing
            , boptsFinalAction = DoNothing
            , boptsDryrun = dryRun
            , boptsGhcOptions = []
            }

-- | Parser for package names
readPackageName :: ReadM PackageName
readPackageName = do
    s <- readerAsk
    case parsePackageNameFromString s of
        Nothing -> readerError $ "Invalid package name: " ++ s
        Just x -> return x

-- | Build the project.
buildCmd :: FinalAction -> BuildOpts -> GlobalOpts -> IO ()
buildCmd finalAction opts go@GlobalOpts{..} =
  catch
  (do (manager,lc) <- loadConfigWithOpts go
      Docker.rerunWithOptionalContainer
        (lcConfig lc)
        (lcProjectRoot lc)
        (do config <- runStackLoggingT manager
                                       globalLogLevel
                                       (lcLoadBuildConfig lc >>= setupEnv False manager)
            runStackT manager globalLogLevel config $
                      Stack.Build.build opts { boptsFinalAction = finalAction}))
             (error . printBuildException)
  where printBuildException e =
          case e of
            MissingTool dep -> "Missing build tool: " <> display dep
            Couldn'tFindPkgId name ->
              ("After installing " <> packageNameString name <>
               ", the package id couldn't be found " <> "(via ghc-pkg describe " <>
               packageNameString name <> "). This shouldn't happen, " <>
               "please report as a bug")
            MissingDep p d range ->
              "Missing dependency for package " <>
              packageNameString (packageName p) <>
              ": " <>
              packageNameString d <>
              " " <>
              display range
            MissingDep2 user dep range ->
              "Local package " <>
              packageNameString user <>
              " depends on " <>
              packageNameString dep <>
              " (" <>
              display range <>
              "), but it wasn't found. Perhaps add it to your local package list?"
            MismatchedLocalDep dep version user range ->
              "Mismatched local dependencies, " <>
              packageNameString user <>
              " depends on " <>
              packageNameString dep <>
              " (" <>
              display range <>
              "), but " <>
              versionString version <>
              " is provided locally"
            MismatchedDep dep version user range ->
              "Mismatched dependencies, " <>
              packageNameString user <>
              " depends on " <>
              packageNameString dep <>
              " (" <>
              display range <>
              "), but " <>
              versionString version <>
              " is provided by your snapshot"
            StackageDepVerMismatch name ver range ->
              ("The package '" <> packageNameString name <>
               "' in this Stackage snapshot is " <> versionString ver <>
               ", but there is an (unsatisfiable) local constraint of " <>
               display range <> ". Suggestion: " <>
               "Check your local package constraints and make them consistent with your current Stackage")
            StackageVersionMismatch name this that ->
              ("There was a mismatch between an installed package, " <>
               packageNameString name <> "==" <> versionString this <>
               " but this Stackage snapshot should be " <> versionString that)
            DependencyIssues es ->
              ("Dependency issues:\n" ++
               intercalate "\n"
                           (map printBuildException es))
            GHCVersionMismatch Nothing expected -> concat
                [ "No GHC found, expected version "
                , versionString expected
                , ". Try running stack setup"
                ]
            GHCVersionMismatch (Just actual) expected -> concat
                [ "GHC version mismatched, found "
                , versionString actual
                , ", but expected "
                , versionString expected
                , ". Try running stack setup"
                ]
            Couldn'tParseTargets targets -> unlines
                $ "The following targets could not be parsed as package names or directories:"
                : map T.unpack targets
            UnknownTargets targets ->
                "The following target packages were not found: " ++
                intercalate ", " (map packageNameString targets)
            TestSuiteFailure exe mlogFile ec -> concat
                [ "Test suite "
                , toFilePath exe
                , " exited with code "
                , show ec
                , case mlogFile of
                    Nothing -> ""
                    Just logFile ->
                        ", log available at: " ++ toFilePath logFile
                ]

-- | Unpack packages to the filesystem
unpackCmd :: [String] -> GlobalOpts -> IO ()
unpackCmd names go@GlobalOpts{..} = do
    (manager,lc) <- loadConfigWithOpts go
    Docker.rerunWithOptionalContainer (lcConfig lc) (lcProjectRoot lc) $ do
        runStackT manager globalLogLevel (lcConfig lc) $ do
            menv <- getMinimalEnvOverride
            Stack.Fetch.unpackPackages menv "." names

-- | Update the package index
updateCmd :: () -> GlobalOpts -> IO ()
updateCmd () go@GlobalOpts{..} = do
    (manager,lc) <- loadConfigWithOpts go
    Docker.rerunWithOptionalContainer (lcConfig lc) (lcProjectRoot lc) $
        runStackT manager globalLogLevel (lcConfig lc) $
            getMinimalEnvOverride >>= Stack.PackageIndex.updateAllIndices

-- | Execute a command
execCmd :: (String, [String]) -> GlobalOpts -> IO ()
execCmd (cmd, args) go@GlobalOpts{..} = do
    (manager,lc) <- loadConfigWithOpts go
    Docker.rerunWithOptionalContainer
      (lcConfig lc)
      (lcProjectRoot lc)
      (do config <- runStackLoggingT manager
                                     globalLogLevel
                                     (lcLoadBuildConfig lc >>= setupEnv False manager)
          menv <- configEnvOverride (bcConfig config)
                          EnvSettings
                              { esIncludeLocals = True
                              , esIncludeGhcPackagePath = True
                              }
          cmd' <- join $ System.Process.Read.findExecutable menv cmd
          let cp = (P.proc (toFilePath cmd') args)
                  { P.env = envHelper menv
                  }
          (Nothing, Nothing, Nothing, ph) <- P.createProcess cp
          ec <- P.waitForProcess ph
          exitWith ec)

-- | Pull the current Docker image.
dockerPullCmd :: () -> GlobalOpts -> IO ()
dockerPullCmd _ go@GlobalOpts{..} =
  Docker.preventInContainer
    (do (_,lc) <- loadConfigWithOpts go
        Docker.pull (configDocker (lcConfig lc)))

-- | Reset the Docker sandbox.
dockerResetCmd :: Bool -> GlobalOpts -> IO ()
dockerResetCmd keepHome go@GlobalOpts{..} =
  Docker.preventInContainer
    (do (_,lc) <- loadConfigWithOpts go
        Docker.reset (lcProjectRoot lc) keepHome)

-- | Cleanup Docker images and containers.
dockerCleanupCmd :: Docker.CleanupOpts -> GlobalOpts -> IO ()
dockerCleanupCmd cleanupOpts go@GlobalOpts{..} =
  Docker.preventInContainer
    (do (_,lc) <- loadConfigWithOpts go
        Docker.cleanup (lcConfig lc) cleanupOpts)

-- | Execute a command
dockerExecCmd :: (String, [String]) -> GlobalOpts -> IO ()
dockerExecCmd cmdArgs go@GlobalOpts{..} = do
    (_,lc) <- loadConfigWithOpts go
    Docker.preventInContainer
      (Docker.rerunCmdWithRequiredContainer (lcConfig lc) (lcProjectRoot lc) (return cmdArgs))

-- | Parser for build arguments.
buildOpts :: Parser BuildOpts
buildOpts = BuildOpts <$> target <*> libProfiling <*> exeProfiling <*>
            optimize <*> finalAction <*> dryRun <*> ghcOpts
  where optimize =
          maybeBoolFlags "optimizations" "optimizations for TARGETs and all its dependencies"
        target =
          fmap (Left . map T.pack)
               (many (strArgument
                        (metavar "TARGET" <>
                         help "If none specified, use all packages defined in current directory")))
        libProfiling =
          boolFlags False
                    "library-profiling"
                    "library profiling for TARGETs and all its dependencies"
        exeProfiling =
          boolFlags False
                    "executable-profiling"
                    "library profiling for TARGETs and all its dependencies"
        finalAction = pure DoNothing
        dryRun = flag False True (long "dry-run" <>
                                  help "Don't build anything, just prepare to")
        ghcOpts =
          many (fmap T.pack
                     (strOption (long "ghc-options" <>
                                 metavar "OPTION" <>
                                 help "Additional options passed to GHC")))

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
    GlobalOpts
    <$> logLevelOpt
    <*> configOptsParser

-- | Parse for a logging level.
logLevelOpt :: Parser LogLevel
logLevelOpt =
  fmap parse
       (strOption (long "verbosity" <>
                   metavar "VERBOSITY" <>
                   help "Verbosity: silent, error, warn, info, debug")) <|>
  flag defaultLogLevel
       verboseLevel
       (short 'v' <>
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

-- | Default logging level should be something useful but not crazy.
defaultLogLevel :: LogLevel
defaultLogLevel = LevelInfo

-- | Parsed global command-line options.
data GlobalOpts = GlobalOpts
    { globalLogLevel     :: LogLevel -- ^ Log level
    , globalConfigMonoid :: ConfigMonoid -- ^ Config monoid, for passing into 'loadConfig'
    } deriving (Show)

-- | Load the configuration with a manager. Convenience function used
-- throughout this module.
loadConfigWithOpts :: GlobalOpts -> IO (Manager,LoadConfig (StackLoggingT IO))
loadConfigWithOpts GlobalOpts{..} = do
    manager <- newTLSManager
    lc <- runStackLoggingT
              manager
              globalLogLevel
              (loadConfig globalConfigMonoid)
    return (manager,lc)
