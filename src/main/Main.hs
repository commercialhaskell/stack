{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Main stack tool entry point.

module Main where

import           Control.Exception
import           Control.Monad.Logger
import           Data.List
import           Data.Maybe
import qualified Data.Text as T
import           Distribution.Text (display)
import           Options.Applicative.Extra
import           Options.Applicative.Simple
import           Stack.Build
import           Stack.Build.Types
import           Stack.Config
import           Stack.Fetch
import           Stack.GhcPkg (envHelper)
import           Stack.Package
import qualified Stack.PackageIndex
import           Stack.Setup (setupEnv)
import           Stack.Types
import           Stack.Types.StackT
import           System.Exit (exitWith)
import qualified System.Process as P

-- | Commandline dispatcher.
main :: IO ()
main =
  do (level,run) <-
       simpleOptions
         "ver"
         "header"
         "desc"
         logLevelOpt
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
                            <$> strArgument (metavar "CMD")
                            <*> many (strArgument (metavar "[ARGS]")))
             addCommand "clean"
                        "Clean the local packages"
                        cleanCmd
                        (pure ()))
     run level

setupCmd :: LogLevel -> IO ()
setupCmd logLevel = do
  manager <- newTLSManager
  _ <- runStackLoggingT manager logLevel (loadConfig >>= toBuildConfig >>= setupEnv True manager)
  return ()

cleanCmd :: () -> LogLevel -> IO ()
cleanCmd _ logLevel = do
  manager <- newTLSManager
  config <- runStackLoggingT manager logLevel (loadConfig >>= toBuildConfig >>= setupEnv False manager)
  runStackT manager logLevel config clean

-- | Build the project.
buildCmd :: FinalAction -> BuildOpts -> LogLevel -> IO ()
buildCmd finalAction opts logLevel =
  catch
  (do manager <- newTLSManager
      config <-
       runStackLoggingT manager logLevel (loadConfig >>= toBuildConfig >>= setupEnv False manager)
      runStackT manager logLevel config $
                 Stack.Build.build opts { boptsFinalAction = finalAction})
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
              " is provided locally"
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

-- | Unpack packages to the filesystem
unpackCmd :: [String] -> LogLevel -> IO ()
unpackCmd names logLevel = do
    manager <- newTLSManager
    config <- runStackLoggingT manager logLevel loadConfig
    runStackT manager logLevel config $ do
        menv <- getMinimalEnvOverride
        Stack.Fetch.unpackPackages menv "." names

-- | Update the package index
updateCmd :: () -> LogLevel -> IO ()
updateCmd () logLevel = do
    manager <- newTLSManager
    config <- runStackLoggingT manager logLevel loadConfig
    runStackT manager logLevel config $ getMinimalEnvOverride >>= Stack.PackageIndex.updateIndex

-- | Execute a command
execCmd :: (String, [String]) -> LogLevel -> IO ()
execCmd (cmd, args) logLevel = do
    manager <- newTLSManager
    config <- runStackLoggingT manager logLevel (loadConfig >>= toBuildConfig >>= setupEnv False manager)
    let cp = (P.proc cmd args)
            { P.env = envHelper $ configEnvOverride (bcConfig config)
                    EnvSettings
                        { esIncludeLocals = True
                        , esIncludeGhcPackagePath = True
                        }
            }
    (Nothing, Nothing, Nothing, ph) <- P.createProcess cp
    ec <- P.waitForProcess ph
    exitWith ec

-- | Parser for build arguments.
buildOpts :: Parser BuildOpts
buildOpts = BuildOpts <$> target <*> libProfiling <*> exeProfiling <*>
            optimize <*> finalAction <*> dryRun <*> ghcOpts
  where optimize =
          maybeBoolFlags "optimizations" "optimizations for TARGETs and all its dependencies"
        target =
          fmap (map T.pack)
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
