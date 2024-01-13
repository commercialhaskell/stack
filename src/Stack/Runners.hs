{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

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

import qualified Data.ByteString.Lazy.Char8 as L8
import           RIO.Process
                   ( findExecutable, mkDefaultProcessContext, proc
                   , readProcess
                   )
import           RIO.Time ( addUTCTime, getCurrentTime )
import           Stack.Build.Target ( NeedTargets (..) )
import           Stack.Config
                   ( getInContainer, getInNixShell, loadConfig, withBuildConfig
                   , withNewLogFunc
                   )
import           Stack.Constants
                   ( defaultTerminalWidth, maxTerminalWidth, minTerminalWidth
                   , nixProgName
                   )
import           Stack.DefaultColorWhen ( defaultColorWhen )
import qualified Stack.Docker as Docker
import qualified Stack.Nix as Nix
import           Stack.Prelude
import           Stack.Setup ( setupEnv )
import           Stack.Storage.User ( logUpgradeCheck, upgradeChecksSince )
import           Stack.Types.BuildOpts
                   ( BuildOptsCLI, defaultBuildOptsCLI )
import           Stack.Types.ColorWhen ( ColorWhen (..) )
import           Stack.Types.Config ( Config (..) )
import           Stack.Types.ConfigMonoid ( ConfigMonoid (..) )
import           Stack.Types.Docker ( DockerOpts (..) )
import           Stack.Types.EnvConfig ( EnvConfig )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.Nix ( NixOpts (..) )
import           Stack.Types.Runner
                   ( Runner (..), globalOptsL, reExecL, stackYamlLocL )
import           Stack.Types.StackYamlLoc ( StackYamlLoc (..) )
import           Stack.Types.Version
                   ( minorVersion, stackMinorVersion, stackVersion )
import           System.Console.ANSI ( hNowSupportsANSI )
import           System.Terminal ( getTerminalWidth )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Runners" module.
data RunnersException
  = CommandInvalid
  | DockerAndNixInvalid
  | NixWithinDockerInvalid
  | DockerWithinNixInvalid
  deriving (Show, Typeable)

instance Exception RunnersException where
  displayException CommandInvalid =
    "Error: [S-7144]\n"
    ++ "Cannot use this command with options which override the stack.yaml \
       \location."
  displayException DockerAndNixInvalid =
    "Error: [S-8314]\n"
    ++ "Cannot use both Docker and Nix at the same time."
  displayException NixWithinDockerInvalid =
    "Error: [S-8641]\n"
    ++ "Cannot use Nix from within a Docker container."
  displayException DockerWithinNixInvalid =
    "Error: [S-5107]\n"
    ++ "Cannot use Docker from within a Nix shell."

-- | Ensure that no project settings are used when running 'withConfig'.
withGlobalProject :: RIO Runner a -> RIO Runner a
withGlobalProject inner = do
  oldSYL <- view stackYamlLocL
  case oldSYL of
    SYLDefault -> local (set stackYamlLocL SYLGlobalProject) inner
    _ -> throwIO CommandInvalid

-- | Helper for 'withEnvConfig' which passes in some default arguments:
--
-- * No targets are requested
--
-- * Default command line build options are assumed
withDefaultEnvConfig :: RIO EnvConfig a -> RIO Config a
withDefaultEnvConfig = withEnvConfig AllowNoTargets defaultBuildOptsCLI

-- | Upgrade a 'Config' environment to an 'EnvConfig' environment by
-- performing further parsing of project-specific configuration (like
-- 'withBuildConfig') and then setting up a build environment
-- toolchain. This is intended to be run inside a call to
-- 'withConfig'.
withEnvConfig ::
     NeedTargets
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
data ShouldReexec
  = YesReexec
  | NoReexec

-- | Load the configuration. Convenience function used
-- throughout this module.
withConfig :: ShouldReexec -> RIO Config a -> RIO Runner a
withConfig shouldReexec inner =
  loadConfig $ \config -> do
    -- If we have been relaunched in a Docker container, perform in-container
    -- initialization (switch UID, etc.).  We do this after first loading the
    -- configuration since it must happen ASAP but needs a configuration.
    view (globalOptsL . to (.globalDockerEntrypoint)) >>=
      traverse_ (Docker.entrypoint config)
    runRIO config $ do
      -- Catching all exceptions here, since we don't want this
      -- check to ever cause Stack to stop working
      shouldUpgradeCheck `catchAny` \e ->
        logError $
          "Error: [S-7353]\n" <>
          "Error when running shouldUpgradeCheck: " <>
          displayShow e
      case shouldReexec of
        YesReexec -> reexec inner
        NoReexec -> inner

-- | Perform a Docker or Nix reexec, if warranted. Otherwise run the inner
-- action.
reexec :: RIO Config a -> RIO Config a
reexec inner = do
  nixEnable' <- asks $ (.nix.enable)
  notifyIfNixOnPath <- asks (.notifyIfNixOnPath)
  when (not nixEnable' && notifyIfNixOnPath) $ do
    eNix <- findExecutable nixProgName
    case eNix of
      Left _ -> pure ()
      Right nix -> proc nix ["--version"] $ \pc -> do
        let nixProgName' = style Shell (fromString nixProgName)
            muteMsg = fillSep
              [ flow "To mute this message in future, set"
              , style Shell (flow "notify-if-nix-on-path: false")
              , flow "in Stack's configuration."
              ]
            reportErr errMsg = prettyWarn $
                 fillSep
                   [ nixProgName'
                   , flow "is on the PATH"
                   , parens (fillSep ["at", style File (fromString nix)])
                   , flow "but Stack encountered the following error with"
                   , nixProgName'
                   , style Shell "--version" <> ":"
                   ]
              <> blankLine
              <> errMsg
              <> blankLine
              <> muteMsg
              <> line
        res <- tryAny (readProcess pc)
        case res of
          Left e -> reportErr (ppException e)
          Right (ec, out, err) -> case ec of
            ExitFailure _ -> reportErr $ string (L8.unpack err)
            ExitSuccess -> do
              let trimFinalNewline str = case reverse str of
                    '\n' : rest -> reverse rest
                    _ -> str
              prettyWarn $ fillSep
                   [ fromString (trimFinalNewline $ L8.unpack out)
                   , flow "is on the PATH"
                   , parens (fillSep ["at", style File (fromString nix)])
                   , flow "but Stack's Nix integration is disabled."
                   , muteMsg
                   ]
                <> line
  dockerEnable' <- asks (.docker.enable)
  case (nixEnable', dockerEnable') of
    (True, True) -> throwIO DockerAndNixInvalid
    (False, False) -> inner

    -- Want to use Nix
    (True, False) -> do
      whenM getInContainer $ throwIO NixWithinDockerInvalid
      isReexec <- view reExecL
      if isReexec
      then inner
      else Nix.runShellAndExit

    -- Want to use Docker
    (False, True) -> do
      whenM getInNixShell $ throwIO DockerWithinNixInvalid
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
    maybe defaultColorWhen pure $
    getFirst go.globalConfigMonoid.configMonoidColorWhen
  useColor <- case colorWhen of
    ColorNever -> pure False
    ColorAlways -> pure True
    ColorAuto -> hNowSupportsANSI stderr
  termWidth <- clipWidth <$> maybe (fromMaybe defaultTerminalWidth
                                    <$> getTerminalWidth)
                                   pure go.globalTermWidth
  menv <- mkDefaultProcessContext
  -- MVar used to ensure the Docker entrypoint is performed exactly once.
  dockerEntrypointMVar <- newMVar False
  let update = go.globalStylesUpdate
  withNewLogFunc go useColor update $ \logFunc -> do
    runRIO Runner
      { runnerGlobalOpts = go
      , runnerUseColor = useColor
      , runnerLogFunc = logFunc
      , runnerTermWidth = termWidth
      , runnerProcessContext = menv
      , runnerDockerEntrypointMVar = dockerEntrypointMVar
      } inner
 where
  clipWidth w
    | w < minTerminalWidth = minTerminalWidth
    | w > maxTerminalWidth = maxTerminalWidth
    | otherwise = w

-- | Check if we should recommend upgrading Stack and, if so, recommend it.
shouldUpgradeCheck :: RIO Config ()
shouldUpgradeCheck = do
  config <- ask
  when config.recommendUpgrade $ do
    now <- getCurrentTime
    let yesterday = addUTCTime (-24 * 60 * 60) now
    checks <- upgradeChecksSince yesterday
    when (checks == 0) $ do
      mversion <- getLatestHackageVersion NoRequireHackageIndex "stack" UsePreferredVersions
      case mversion of
        -- Compare the minor version so we avoid patch-level, Hackage-only releases.
        -- See: https://github.com/commercialhaskell/stack/pull/4729#pullrequestreview-227176315
        Just (PackageIdentifierRevision _ version _) | minorVersion version > stackMinorVersion -> do
          prettyWarn $
               fillSep
                 [ flow "You are currently using Stack version"
                 , fromString (versionString stackVersion)
                 , flow "but version"
                 , fromString (versionString version)
                 , flow "is available."
                 ]
            <> blankLine
            <> fillSep
                 [ "You can try to upgrade by running"
                 , style Shell (flow "stack upgrade")
                 ]
            <> blankLine
            <> fillSep
                 [ flow "Tired of seeing this? Add"
                 , style Shell (flow "recommend-stack-upgrade: false")
                 , "to"
                 , pretty config.userConfigPath <> "."
                 ]
            <> blankLine
        _ -> pure ()
      logUpgradeCheck now
