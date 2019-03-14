{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Utilities for running stack commands.
module Stack.Runners
    ( withGlobalConfigAndLock
    , withConfigAndLock
    , withEnvConfigAndLock
    , withDefaultEnvConfigAndLock
    , withCleanConfig
    , withEnvConfig
    , withDefaultEnvConfig
    , withEnvConfigExt
    , withEnvConfigDot
    , withConfig
    , loadCompilerVersion
    , withUserFileLock
    , munlockFile
    , withRunnerGlobal
    ) where

import           Stack.Prelude
import           Path
import           Path.IO
import           RIO.Process (mkDefaultProcessContext)
import           Stack.Build.Target(NeedTargets(..))
import           Stack.Config
import           Stack.Constants
import           Stack.DefaultColorWhen (defaultColorWhen)
import qualified Stack.Docker as Docker
import qualified Stack.Nix as Nix
import           Stack.Setup
import           Stack.Types.Config
import           System.Console.ANSI (hSupportsANSIWithoutEmulation)
import           System.Environment (getEnvironment)
import           System.FileLock
import           System.Terminal (getTerminalWidth)
import           Stack.Dot

-- FIXME it seems wrong that we call loadBuildConfig multiple times
loadCompilerVersion :: RIO Config WantedCompiler
loadCompilerVersion = view wantedCompilerVersionL <$> loadBuildConfig

-- | Enforce mutual exclusion of every action running via this
-- function, on this path, on this users account.
--
-- A lock file is created inside the given directory.  Currently,
-- stack uses locks per-snapshot.  In the future, stack may refine
-- this to an even more fine-grain locking approach.
--
withUserFileLock :: HasRunner env
                 => Path Abs Dir
                 -> (Maybe FileLock -> RIO env a)
                 -> RIO env a
withUserFileLock dir act = withRunInIO $ \run -> do
    env <- getEnvironment
    let toLock = lookup "STACK_LOCK" env == Just "true"
    if toLock
        then do
            let lockfile = relFileLockfile
            let pth = dir </> lockfile
            ensureDir dir
            -- Just in case of asynchronous exceptions, we need to be careful
            -- when using tryLockFile here:
            bracket (tryLockFile (toFilePath pth) Exclusive)
                    munlockFile
                    (\fstTry ->
                        case fstTry of
                          Just lk -> run $ act $ Just lk
                          Nothing ->
                            do run $ logError $
                                 "Failed to grab lock (" <>
                                 displayShow pth <>
                                 "); other stack instance running.  Waiting..."
                               bracket (lockFile (toFilePath pth) Exclusive)
                                       unlockFile
                                       (\lk -> run $ do
                                            logError "Lock acquired, proceeding."
                                            act $ Just lk))
        else run $ act Nothing

withConfigAndLock
    :: GlobalOpts
    -> RIO Config ()
    -> IO ()
withConfigAndLock go@GlobalOpts{..} inner = withConfig go $ do
    stackRoot <- view stackRootL
    projectRoot <- view $ to configProjectRoot
    withUserFileLock stackRoot $ \lk ->
      Docker.reexecWithOptionalContainer
        projectRoot
        Nothing
        inner
        Nothing
        (Just $ munlockFile lk)

-- | Loads global config, ignoring any configuration which would be
-- loaded due to $PWD.
withGlobalConfigAndLock
    :: GlobalOpts
    -> RIO Config ()
    -> IO ()
withGlobalConfigAndLock go@GlobalOpts{..} inner =
    withRunnerGlobal go $
    loadConfigMaybeProject
      globalConfigMonoid
      globalResolver
      PCNoProject $ \lc ->
        withUserFileLock (view stackRootL lc) $ \_lk ->
          runRIO lc inner

-- For now the non-locking version just unlocks immediately.
-- That is, there's still a serialization point.
withDefaultEnvConfig
    :: GlobalOpts
    -> RIO EnvConfig ()
    -> IO ()
withDefaultEnvConfig go inner =
    withEnvConfigAndLock go AllowNoTargets defaultBuildOptsCLI (\lk -> do munlockFile lk
                                                                          inner)

withEnvConfig
    :: GlobalOpts
    -> NeedTargets
    -> BuildOptsCLI
    -> RIO EnvConfig ()
    -> IO ()
withEnvConfig go needTargets boptsCLI inner =
    withEnvConfigAndLock go needTargets boptsCLI (\lk -> do munlockFile lk
                                                            inner)

withDefaultEnvConfigAndLock
    :: GlobalOpts
    -> (Maybe FileLock -> RIO EnvConfig ())
    -> IO ()
withDefaultEnvConfigAndLock go inner =
    withEnvConfigExt go AllowNoTargets defaultBuildOptsCLI Nothing inner Nothing

withEnvConfigAndLock
    :: GlobalOpts
    -> NeedTargets
    -> BuildOptsCLI
    -> (Maybe FileLock -> RIO EnvConfig ())
    -> IO ()
withEnvConfigAndLock go needTargets boptsCLI inner =
    withEnvConfigExt go needTargets boptsCLI Nothing inner Nothing

-- | A runner specially built for the "stack clean" use case. For some
-- reason (hysterical raisins?), all of the functions in this module
-- which say BuildConfig actually work on an EnvConfig, while the
-- clean command legitimately only needs a BuildConfig. At some point
-- in the future, we could consider renaming everything for more
-- consistency.
--
-- /NOTE/ This command always runs outside of the Docker environment,
-- since it does not need to run any commands to get information on
-- the project. This is a change as of #4480. For previous behavior,
-- see issue #2010.
withCleanConfig :: GlobalOpts -> RIO BuildConfig a -> IO a
withCleanConfig go inner =
  withConfig go $ do
    root <- view stackRootL
    withUserFileLock root $ \_lk0 -> do
      bconfig <- loadBuildConfig
      runRIO bconfig inner

withEnvConfigExt
    :: GlobalOpts
    -> NeedTargets
    -> BuildOptsCLI
    -> Maybe (RIO Config ())
    -- ^ Action to perform before the build.  This will be run on the host
    -- OS even if Docker is enabled for builds.  The build config is not
    -- available in this action, since that would require build tools to be
    -- installed on the host OS.
    -> (Maybe FileLock -> RIO EnvConfig ())
    -- ^ Action that uses the build config.  If Docker is enabled for builds,
    -- this will be run in a Docker container.
    -> Maybe (RIO Config ())
    -- ^ Action to perform after the build.  This will be run on the host
    -- OS even if Docker is enabled for builds.  The build config is not
    -- available in this action, since that would require build tools to be
    -- installed on the host OS.
    -> IO ()
withEnvConfigExt go@GlobalOpts{..} needTargets boptsCLI mbefore inner mafter = withConfig go $ do
    config <- ask
    withUserFileLock (view stackRootL config) $ \lk0 -> do
      -- A local bit of state for communication between callbacks:
      curLk <- newIORef lk0
      let inner' lk =
            -- Locking policy:  This is only used for build commands, which
            -- only need to lock the snapshot, not the global lock.  We
            -- trade in the lock here.
            do dir <- installationRootDeps
               -- Hand-over-hand locking:
               withUserFileLock dir $ \lk2 -> do
                 liftIO $ writeIORef curLk lk2
                 liftIO $ munlockFile lk
                 logDebug "Starting to execute command inside EnvConfig"
                 inner lk2

      let inner'' lk = do
              bconfig <- runRIO config loadBuildConfig
              envConfig <- runRIO bconfig (setupEnv needTargets boptsCLI Nothing)
              runRIO envConfig (inner' lk)

      Docker.reexecWithOptionalContainer
          (configProjectRoot config)
          mbefore
          (Nix.reexecWithOptionalShell (configProjectRoot config) loadCompilerVersion (inner'' lk0))
          mafter
          (Just $ liftIO $
                do lk' <- readIORef curLk
                   munlockFile lk')

-- | Load the configuration. Convenience function used
-- throughout this module.
withConfig
  :: GlobalOpts
  -> RIO Config a
  -> IO a
withConfig go@GlobalOpts{..} inner = withRunnerGlobal go $ do
    mstackYaml <- forM globalStackYaml resolveFile'
    loadConfig globalConfigMonoid globalResolver mstackYaml $ \config -> do
      -- If we have been relaunched in a Docker container, perform in-container initialization
      -- (switch UID, etc.).  We do this after first loading the configuration since it must
      -- happen ASAP but needs a configuration.
      forM_ globalDockerEntrypoint $ Docker.entrypoint config
      runRIO config inner

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

-- | Unlock a lock file, if the value is Just
munlockFile :: MonadIO m => Maybe FileLock -> m ()
munlockFile Nothing = return ()
munlockFile (Just lk) = liftIO $ unlockFile lk

-- Plumbing for --test and --bench flags
withEnvConfigDot
    :: DotOpts
    -> GlobalOpts
    -> RIO EnvConfig ()
    -> IO ()
withEnvConfigDot opts go f = withEnvConfig go' NeedTargets boptsCLI f
  where
    boptsCLI = defaultBuildOptsCLI
        { boptsCLITargets = dotTargets opts
        , boptsCLIFlags = dotFlags opts
        }
    go' =
        (if dotTestTargets opts then set (globalOptsBuildOptsMonoidL.buildOptsMonoidTestsL) (Just True) else id) $
        (if dotBenchTargets opts then set (globalOptsBuildOptsMonoidL.buildOptsMonoidBenchmarksL) (Just True) else id)
        go
