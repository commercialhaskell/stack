{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

-- | Main stack tool entry point.

module Main where

import           Control.Exception
import           Control.Monad (join)
import           Control.Monad.Logger
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Distribution.Text (display)
import           Options.Applicative.Builder.Extra
import           Options.Applicative.Simple
import           Options.Applicative.Types (readerAsk)
import           Path (toFilePath)
import           Stack.Build
import           Stack.Build.Types
import           Stack.Config
import qualified Stack.Docker as Docker
import           Stack.Fetch
import           Stack.GhcPkg (envHelper)
import           Stack.Package
import qualified Stack.PackageIndex
import           Stack.Setup (setupEnv)
import           Stack.Types
import           Stack.Types.StackT
import           System.Exit (exitWith)
import qualified System.Process as P
import qualified System.Process.Read
import qualified Paths_stack as Meta

-- | Commandline dispatcher.
main :: IO ()
main =
  do Docker.checkVersions
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
                            <*> fmap (fromMaybe False)
                               (maybeBoolFlags "dry-run" "Don't build anything, just prepare to"))
             addSubCommands
               Docker.dockerCmdName
               "Subcommands specific to Docker use"
               (do addCommand Docker.dockerPullCmdName
                              "Pull latest version of Docker image from registry"
                              dockerPullCmd
                              (pure ())))
     run level

setupCmd :: GlobalOpts -> IO ()
setupCmd GlobalOpts{..} = do
  manager <- newTLSManager
  lc <- runStackLoggingT manager globalLogLevel (loadConfig globalConfigMonoid)
  Docker.rerunWithOptionalContainer
    (lcConfig lc)
    (lcProjectRoot lc)
    (do _ <- runStackLoggingT manager
                              globalLogLevel
                              (lcLoadBuildConfig lc >>= setupEnv True manager)
        return ())

cleanCmd :: () -> GlobalOpts -> IO ()
cleanCmd _ GlobalOpts{..} = do
  manager <- newTLSManager
  lc <- runStackLoggingT manager globalLogLevel (loadConfig globalConfigMonoid)
  Docker.rerunWithOptionalContainer
    (lcConfig lc)
    (lcProjectRoot lc)
    (do config <- runStackLoggingT manager
                                   globalLogLevel
                                   (lcLoadBuildConfig lc >>= setupEnv False manager)
        runStackT manager globalLogLevel config clean)

-- | Install dependencies
depsCmd :: ([PackageName], Bool) -> GlobalOpts -> IO ()
depsCmd (names, dryRun) GlobalOpts{..} = do
    manager <- newTLSManager
    lc <- runStackLoggingT manager globalLogLevel (loadConfig globalConfigMonoid)
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
buildCmd finalAction opts GlobalOpts{..} =
  catch
  (do manager <- newTLSManager
      lc <- runStackLoggingT manager globalLogLevel (loadConfig globalConfigMonoid)
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

-- | Unpack packages to the filesystem
unpackCmd :: [String] -> GlobalOpts -> IO ()
unpackCmd names GlobalOpts{..} = do
    manager <- newTLSManager
    lc <- runStackLoggingT manager globalLogLevel (loadConfig globalConfigMonoid)
    Docker.rerunWithOptionalContainer (lcConfig lc) (lcProjectRoot lc) $ do
        runStackT manager globalLogLevel (lcConfig lc) $ do
            menv <- getMinimalEnvOverride
            Stack.Fetch.unpackPackages menv "." names

-- | Update the package index
updateCmd :: () -> GlobalOpts -> IO ()
updateCmd () GlobalOpts{..} = do
    manager <- newTLSManager
    lc <- runStackLoggingT manager globalLogLevel (loadConfig globalConfigMonoid)
    Docker.rerunWithOptionalContainer (lcConfig lc) (lcProjectRoot lc) $
        runStackT manager globalLogLevel (lcConfig lc) $
            getMinimalEnvOverride >>= Stack.PackageIndex.updateIndex

-- | Execute a command
execCmd :: (String, [String]) -> GlobalOpts -> IO ()
execCmd (cmd, args) GlobalOpts{..} = do
    --EKB FIXME: add a `docker exec` subcommand that just reruns in docker without needing `stack` in image
    manager <- newTLSManager
    lc <- runStackLoggingT manager globalLogLevel (loadConfig globalConfigMonoid)
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
dockerPullCmd _ GlobalOpts{..} =
  Docker.preventInContainer
    (unwords [Docker.dockerCmdName, Docker.dockerPullCmdName])
    (do manager <- newTLSManager
        lc <- runStackLoggingT manager globalLogLevel (loadConfig globalConfigMonoid)
        Docker.pull (configDocker (lcConfig lc)) (lcProjectRoot lc))

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
        dryRun =
          fmap (fromMaybe False)
               (maybeBoolFlags "dry-run" "Don't build anything, just prepare to")
        ghcOpts =
          many (fmap T.pack
                     (strOption (long "ghc-options" <>
                                 metavar "OPTION" <>
                                 help "Additional options passed to GHC")))

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
       LevelDebug
       (short 'v' <>
        help "Enable verbose mode: verbosity level \"info\"")
  where parse s =
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
