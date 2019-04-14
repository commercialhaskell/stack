{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Utilities for running stack commands.
--
-- Instead of using Has-style classes below, the type signatures use
-- concrete environments to try and avoid accidentally rerunning
-- configuration parsing. For example, we want @withConfig $
-- withConfig $ ...@ to fail.
module Stack.Runners
    ( withBuildConfig
    , withEnvConfig
    , withDefaultEnvConfig
    , withConfig
    , withGlobalProject
    , withRunnerGlobal
    , ShouldReexec (..)
    ) where

import           Stack.Prelude
import           Distribution.Version (mkVersion')
import qualified Paths_stack
import           RIO.Process (mkDefaultProcessContext)
import           RIO.Time (addUTCTime, getCurrentTime)
import           Stack.Build.Target(NeedTargets(..))
import           Stack.Config
import           Stack.Constants
import           Stack.DefaultColorWhen (defaultColorWhen)
import qualified Stack.Docker as Docker
import qualified Stack.Nix as Nix
import           Stack.Setup
import           Stack.Storage (upgradeChecksSince, logUpgradeCheck)
import           Stack.Types.Config
import           Stack.Types.Docker (dockerEnable)
import           Stack.Types.Nix (nixEnable)
import           System.Console.ANSI (hSupportsANSIWithoutEmulation)
import           System.Terminal (getTerminalWidth)

-- | Ensure that no project settings are used when running 'withConfig'.
withGlobalProject :: RIO Runner a -> RIO Runner a
withGlobalProject inner = do
  oldSYL <- view stackYamlLocL
  case oldSYL of
    SYLDefault -> local (set stackYamlLocL SYLGlobalProject) inner
    _ -> throwString "Cannot use this command with options which override the stack.yaml location"

-- | Helper for 'withEnvConfig' which passes in some default arguments:
--
-- * No targets are requested
--
-- * Default command line build options are assumed
withDefaultEnvConfig
    :: RIO EnvConfig a
    -> RIO Config a
withDefaultEnvConfig = withEnvConfig AllowNoTargets defaultBuildOptsCLI

-- | Upgrade a 'Config' environment to a 'BuildConfig' environment by
-- performing further parsing of project-specific configuration. This
-- is intended to be run inside a call to 'withConfig'.
withBuildConfig :: RIO BuildConfig a -> RIO Config a
withBuildConfig inner = do
  bconfig <- loadBuildConfig
  runRIO bconfig inner

-- | Upgrade a 'Config' environment to an 'EnvConfig' environment by
-- performing further parsing of project-specific configuration (like
-- 'withBuildConfig') and then setting up a build environment
-- toolchain. This is intended to be run inside a call to
-- 'withConfig'.
withEnvConfig
    :: NeedTargets
    -> BuildOptsCLI
    -> RIO EnvConfig a
    -- ^ Action that uses the build config.  If Docker is enabled for builds,
    -- this will be run in a Docker container.
    -> RIO Config a
withEnvConfig needTargets boptsCLI inner =
  withBuildConfig $ do
    envConfig <- setupEnv needTargets boptsCLI Nothing
    logDebug "Starting to execute command inside EnvConfig"
    runRIO envConfig inner

-- | If the settings justify it, should we reexec inside Docker or Nix?
data ShouldReexec = YesReexec | NoReexec

-- | Load the configuration. Convenience function used
-- throughout this module.
withConfig
  :: ShouldReexec
  -> RIO Config a
  -> RIO Runner a
withConfig shouldReexec inner =
    loadConfig $ \config -> do
      -- If we have been relaunched in a Docker container, perform in-container initialization
      -- (switch UID, etc.).  We do this after first loading the configuration since it must
      -- happen ASAP but needs a configuration.
      view (globalOptsL.to globalDockerEntrypoint) >>=
        traverse_ (Docker.entrypoint config)
      runRIO config $ do
        -- Catching all exceptions here, since we don't want this
        -- check to ever cause Stack to stop working
        shouldUpgradeCheck `catchAny` \e ->
          logError ("Error when running shouldUpgradeCheck: " <> displayShow e)
        case shouldReexec of
          YesReexec -> reexec inner
          NoReexec -> inner

-- | Perform a Docker or Nix reexec, if warranted. Otherwise run the
-- inner action.
reexec :: RIO Config a -> RIO Config a
reexec inner = do
  nixEnable' <- asks $ nixEnable . configNix
  dockerEnable' <- asks $ dockerEnable . configDocker
  case (nixEnable', dockerEnable') of
    (True, True) -> throwString "Cannot use both Docker and Nix at the same time"
    (False, False) -> inner

    -- Want to use Nix
    (True, False) -> do
      whenM getInContainer $ throwString "Cannot use Nix from within a Docker container"
      inShell <- getInNixShell
      if inShell
        then do
          isReexec <- view reExecL
          if isReexec
            then inner
            else throwString "In Nix shell but reExecL is False"
        else Nix.runShellAndExit

    -- Want to use Docker
    (False, True) -> do
      whenM getInNixShell $ throwString "Cannot use Docker from within a Nix shell"
      inContainer <- getInContainer
      if inContainer
        then do
          isReexec <- view reExecL
          if isReexec
            then inner
            else throwIO Docker.OnlyOnHostException
        else Docker.runContainerAndExit

-- | Use the 'GlobalOpts' to create a 'Runner' and run the provided
-- action.
withRunnerGlobal :: GlobalOpts -> RIO Runner a -> IO a
withRunnerGlobal go inner = do
  colorWhen <-
    case getFirst $ configMonoidColorWhen $ globalConfigMonoid go of
      Nothing -> defaultColorWhen
      Just colorWhen -> pure colorWhen
  useColor <- case colorWhen of
    ColorNever -> return False
    ColorAlways -> return True
    ColorAuto -> fromMaybe True <$>
                          hSupportsANSIWithoutEmulation stderr
  termWidth <- clipWidth <$> maybe (fromMaybe defaultTerminalWidth
                                    <$> getTerminalWidth)
                                   pure (globalTermWidth go)
  menv <- mkDefaultProcessContext
  logOptions0 <- logOptionsHandle stderr False
  let logOptions
        = setLogUseColor useColor
        $ setLogUseTime (globalTimeInLog go)
        $ setLogMinLevel (globalLogLevel go)
        $ setLogVerboseFormat (globalLogLevel go <= LevelDebug)
        $ setLogTerminal (globalTerminal go)
          logOptions0
  withLogFunc logOptions $ \logFunc -> runRIO Runner
    { runnerGlobalOpts = go
    , runnerUseColor = useColor
    , runnerLogFunc = logFunc
    , runnerTermWidth = termWidth
    , runnerProcessContext = menv
    } inner
  where clipWidth w
          | w < minTerminalWidth = minTerminalWidth
          | w > maxTerminalWidth = maxTerminalWidth
          | otherwise = w

-- | Check if we should recommend upgrading Stack and, if so, recommend it.
shouldUpgradeCheck :: RIO Config ()
shouldUpgradeCheck = do
  config <- ask
  when (configRecommendUpgrade config) $ do
    now <- getCurrentTime
    let yesterday = addUTCTime (-24 * 60 * 60) now
    checks <- upgradeChecksSince yesterday
    when (checks == 0) $ do
      mversion <- getLatestHackageVersion NoRequireHackageIndex "stack" UsePreferredVersions
      case mversion of
        Just (PackageIdentifierRevision _ version _) | version > mkVersion' Paths_stack.version -> do
          logWarn "<<<<<<<<<<<<<<<<<<"
          logWarn $
            "You are currently using Stack version " <>
            fromString (versionString (mkVersion' Paths_stack.version)) <>
            ", but version " <>
            fromString (versionString version) <>
            " is available"
          logWarn "You can try to upgrade by running 'stack upgrade'"
          logWarn $
            "Tired of seeing this? Add 'recommend-stack-upgrade: false' to " <>
            fromString (toFilePath (configUserConfigPath config))
          logWarn ">>>>>>>>>>>>>>>>>>"
          logWarn ""
          logWarn ""
        _ -> pure ()
      logUpgradeCheck now
