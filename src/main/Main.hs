{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Main stack tool entry point.

module Main where

import           Control.Exception
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.List
import           Data.Maybe
import qualified Data.Text as T
import           Development.Shake (Verbosity(..))
import           Distribution.Text (display)
import           Network.HTTP.Conduit
import           Options.Applicative.Extra
import           Options.Applicative.Simple
import           Stack.Build
import           Stack.Build.Types
import           Stack.Config
import           Stack.Package
import           Stack.Setup
import           Stack.Types
import           Stack.Types.Internal

-- | Commandline dispatcher.
main :: IO ()
main =
  do (_,run) <-
       simpleOptions
         "ver"
         "header"
         "desc"
         (pure ())
         (do addCommand "build"
                        "Build the project(s) in this directory/configuration"
                        buildCmd
                        buildOpts
             addCommand "setup"
                        "Get the appropriate ghc for your project"
                        setupCmd
                        setupOpts)
     run

type SetupOpts = String

setupCmd :: SetupOpts -> IO ()
setupCmd opts = do
  version <- parseVersionFromString opts
  -- TODO: actually load the config and use it
  runMonadLogger $ flip runReaderT (undefined :: Config) $ setup version

setupOpts :: Parser SetupOpts
setupOpts = strArgument (metavar "GHC_MAJOR_VERSION")

-- | Build the project.
buildCmd :: BuildOpts -> IO ()
buildCmd opts =
  do config <- runMonadLogger loadConfig
     catch (runCmd config (Stack.Build.build opts {boptsInDocker = configInDocker config}))
           (error . printBuildException)
  where printBuildException e =
          case e of
            MissingTool dep -> "Missing build tool: " <> display dep
            Couldn'tFindPkgId name ->
              ("After installing " <> packageNameString name <>
               ", the package id couldn't be found " <>
               "(via ghc-pkg describe " <> packageNameString name <>
               "). This shouldn't happen, " <> "please report as a bug")
            MissingDep p d range ->
              "Missing dependency for package " <>
              packageNameString (packageName p) <>
              ": " <>
              packageNameString d <>
              " " <>
              display range
            StackageDepVerMismatch name ver range ->
              ("The package '" <> packageNameString name <>
               "' in this Stackage snapshot is " <> versionString ver <>
               ", but there is an (unsatisfiable) local constraint of " <>
               display range <> ". Suggestion: " <>
               "Check your local package constraints and make them consistent with your current Stackage")
            StackageVersionMismatch name this that ->
              ("There was a mismatch between an installed package, " <>
               packageNameString name <> "==" <> versionString this <>
               " but this Stackage snapshot should be " <>
               versionString that)
            DependencyIssues es ->
              ("Dependency issues:\n" ++
               intercalate "\n"
                           (map printBuildException es))

-- | Parser for build arguments.
--
-- FIXME: Parse Snapshot version properly.
buildOpts :: Parser BuildOpts
buildOpts = BuildOpts <$> target <*> verbose <*> libProfiling <*> exeProfiling <*>
            optimize <*> finalAction <*> dryRun <*> ghcOpts <*> inDocker <*>
            ltsVer
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
        inDocker = pure False
        ltsVer = pure (LTS 2 1)

-- | Run logging monad for commands.
--
-- FIXME: Decide on logging levels based on verbosity.
runMonadLogger :: LoggingT IO a -> IO a
runMonadLogger = runStdoutLoggingT

-- | Run a command with the given config.
runCmd :: (MonadBaseControl IO m,MonadIO m)
       => Config -> ReaderT Env (ResourceT m) a -> m a
runCmd config m = withManager
                    (\manager ->
                       runReaderT m
                                  (Env config manager))
