{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Main stack tool entry point.

module Main where

import           Control.Arrow ((***))
import           Control.Exception
import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Development.Shake (Verbosity(..))
import           Distribution.Text (display)
import           Options.Applicative.Extra
import           Options.Applicative.Simple
import           Path
import           Stack.Build
import           Stack.Fetch
import           Stack.Build.Types
import           Stack.Config
import           Stack.Package
import qualified Stack.PackageIndex
import           Stack.Setup
import           Stack.Types
import           Stack.Types.StackT
import           System.Exit (exitWith)
import           System.FilePath (searchPathSeparator)
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
                        buildCmd
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
                            <*> many (strArgument (metavar "[ARGS]"))))
     run level

setupCmd :: LogLevel -> IO ()
setupCmd logLevel = do
  bc <- runStackLoggingT logLevel (loadConfig >>= toBuildConfig)
  runStackT logLevel bc $ setup $ bcGhcVersion bc

-- | Modify the environment variables (like PATH) appropriately, possibly doing installation too
setupEnv :: (MonadIO m, MonadThrow m, MonadLogger m) -- FIXME move this logic into the library itself?
         => Bool -- ^ setup GHC_PACKAGE_PATH? ideally we'd do this always, but Cabal doesn't like that
         -> BuildConfig
         -> m BuildConfig
setupEnv setGhcPackagePath bconfig = do
    dirs <- runReaderT extraBinDirs bconfig
    let env0 = configExternalEnv $ bcConfig bconfig
        env1 = addPath dirs env0
    env2 <- if setGhcPackagePath
        then flip runReaderT bconfig $ do
            -- FIXME make sure the directories exist?
            deps <- packageDatabaseDeps
            local <- packageDatabaseLocal
            global <- return "FIXME get the global database by querying ghc-pkg presumably"
            let v = intercalate [searchPathSeparator]
                    [ toFilePath local
                    , toFilePath deps
                    , global
                    ]
            return $ Map.insert "GHC_PACKAGE_PATH" (T.pack v) env1
        else return env1
    $logDebug $ "New env: " <> T.pack (show env2)
    return bconfig { bcConfig = (bcConfig bconfig) { configExternalEnv = env2 } }
  where
    addPath dirs m = Map.insert "PATH" (mkPath dirs $ Map.lookup "PATH" m) m

    mkPath dirs mpath = T.pack $ intercalate [searchPathSeparator]
        (map toFilePath dirs ++ maybe [] (return . T.unpack) mpath)

-- | Build the project.
buildCmd :: BuildOpts -> LogLevel -> IO ()
buildCmd opts logLevel =
  do config <-
       runStackLoggingT logLevel (loadConfig >>= toBuildConfig >>= setupEnv False)
     catch (runStackT logLevel config $ do
                 checkGHCVersion
                 Stack.Build.build opts)
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
            GHCVersionMismatch actual expected ->
              ("GHC version mismatched, found " ++
               versionString actual ++
               ", but expected " ++
               versionString expected)

-- | Unpack packages to the filesystem
unpackCmd :: [String] -> LogLevel -> IO ()
unpackCmd names logLevel = do
    config <- runStackLoggingT logLevel loadConfig
    runStackT logLevel config (Stack.Fetch.unpackPackages "." names)

-- | Update the package index
updateCmd :: () -> LogLevel -> IO ()
updateCmd () logLevel = do
    config <- runStackLoggingT logLevel loadConfig
    runStackT logLevel config Stack.PackageIndex.updateIndex

-- | Execute a command
execCmd :: (String, [String]) -> LogLevel -> IO ()
execCmd (cmd, args) logLevel = do
    config <- runStackLoggingT logLevel (loadConfig >>= toBuildConfig >>= setupEnv True)
    let cp = (P.proc cmd args) { P.env = Just $ map (T.unpack *** T.unpack) $ Map.toList $ configExternalEnv $ bcConfig config }
    (Nothing, Nothing, Nothing, ph) <- P.createProcess cp
    ec <- P.waitForProcess ph
    exitWith ec

-- | Parser for build arguments.
buildOpts :: Parser BuildOpts
buildOpts = BuildOpts <$> target <*> verbose <*> libProfiling <*> exeProfiling <*>
            optimize <*> finalAction <*> dryRun <*> ghcOpts
  where optimize =
          maybeBoolFlags "optimizations" "optimizations for TARGETs and all its dependencies"
        target =
          fmap (map T.pack)
               (many (strArgument
                        (metavar "TARGET" <>
                         help "If none specified, use all packages defined in current directory")))
        verbose =
          fmap (\v ->
                  if v
                     then Chatty
                     else Quiet)
               (switch (long "verbose" <>
                        short 'v'))
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
       LevelInfo
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
