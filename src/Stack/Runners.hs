{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Utilities for running stack commands.
module Stack.Runners
    ( withGlobalConfigAndLock
    , withConfigAndLock
    , withMiniConfigAndLock
    , withBuildConfigAndLock
    , withBuildConfigAndLockNoDocker
    , withBuildConfig
    , withBuildConfigExt
    , withBuildConfigDot
    , loadConfigWithOpts
    , loadCompilerVersion
    , withUserFileLock
    , munlockFile
    ) where

import           Stack.Prelude
import           Path
import           Path.IO
import           Stack.Config
import qualified Stack.Docker as Docker
import qualified Stack.Nix as Nix
import           Stack.Setup
import           Stack.Types.Compiler (CompilerVersion, CVType (..))
import           Stack.Types.Config
import           Stack.Types.Runner
import           System.Environment (getEnvironment)
import           System.IO
import           System.FileLock
import           Stack.Dot
import           Lens.Micro

-- FIXME it seems wrong that we call lcLoadBuildConfig multiple times
loadCompilerVersion :: GlobalOpts
                    -> LoadConfig
                    -> IO (CompilerVersion 'CVWanted)
loadCompilerVersion go lc =
    view wantedCompilerVersionL <$> lcLoadBuildConfig lc (globalCompiler go)

-- | Enforce mutual exclusion of every action running via this
-- function, on this path, on this users account.
--
-- A lock file is created inside the given directory.  Currently,
-- stack uses locks per-snapshot.  In the future, stack may refine
-- this to an even more fine-grain locking approach.
--
withUserFileLock :: MonadUnliftIO m
                 => GlobalOpts
                 -> Path Abs Dir
                 -> (Maybe FileLock -> m a)
                 -> m a
withUserFileLock go@GlobalOpts{} dir act = do
    env <- liftIO getEnvironment
    let toLock = lookup "STACK_LOCK" env == Just "true"
    if toLock
        then do
            let lockfile = $(mkRelFile "lockfile")
            let pth = dir </> lockfile
            ensureDir dir
            -- Just in case of asynchronous exceptions, we need to be careful
            -- when using tryLockFile here:
            bracket (liftIO $ tryLockFile (toFilePath pth) Exclusive)
                    (maybe (return ()) (liftIO . unlockFile))
                    (\fstTry ->
                        case fstTry of
                          Just lk -> finally (act $ Just lk) (liftIO $ unlockFile lk)
                          Nothing ->
                            do let chatter = globalLogLevel go /= LevelOther "silent"
                               when chatter $
                                 liftIO $ hPutStrLn stderr $ "Failed to grab lock ("++show pth++
                                                     "); other stack instance running.  Waiting..."
                               bracket (liftIO $ lockFile (toFilePath pth) Exclusive)
                                       (liftIO . unlockFile)
                                       (\lk -> do
                                            when chatter $
                                              liftIO $ hPutStrLn stderr "Lock acquired, proceeding."
                                            act $ Just lk))
        else act Nothing

withConfigAndLock
    :: GlobalOpts
    -> RIO Config ()
    -> IO ()
withConfigAndLock go@GlobalOpts{..} inner = loadConfigWithOpts go $ \lc -> do
    withUserFileLock go (view stackRootL lc) $ \lk ->
        runRIO (lcConfig lc) $
            Docker.reexecWithOptionalContainer
                (lcProjectRoot lc)
                Nothing
                (runRIO (lcConfig lc) inner)
                Nothing
                (Just $ munlockFile lk)

-- | Loads global config, ignoring any configuration which would be
-- loaded due to $PWD.
withGlobalConfigAndLock
    :: GlobalOpts
    -> RIO Config ()
    -> IO ()
withGlobalConfigAndLock go@GlobalOpts{..} inner = withRunnerGlobal go $ \runner -> do
    lc <- runRIO runner $
        loadConfigMaybeProject
            globalConfigMonoid
            Nothing
            LCSNoProject
    withUserFileLock go (view stackRootL lc) $ \_lk ->
        runRIO (lcConfig lc) inner

-- For now the non-locking version just unlocks immediately.
-- That is, there's still a serialization point.
withBuildConfig
    :: GlobalOpts
    -> RIO EnvConfig ()
    -> IO ()
withBuildConfig go inner =
    withBuildConfigAndLock go (\lk -> do munlockFile lk
                                         inner)

withBuildConfigAndLock
    :: GlobalOpts
    -> (Maybe FileLock -> RIO EnvConfig ())
    -> IO ()
withBuildConfigAndLock go inner =
    withBuildConfigExt False go Nothing inner Nothing

-- | See issue #2010 for why this exists. Currently just used for the
-- specific case of "stack clean --full".
withBuildConfigAndLockNoDocker
    :: GlobalOpts
    -> (Maybe FileLock -> RIO EnvConfig ())
    -> IO ()
withBuildConfigAndLockNoDocker go inner =
    withBuildConfigExt True go Nothing inner Nothing

withBuildConfigExt
    :: Bool
    -> GlobalOpts
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
withBuildConfigExt skipDocker go@GlobalOpts{..} mbefore inner mafter = loadConfigWithOpts go $ \lc -> do
    withUserFileLock go (view stackRootL lc) $ \lk0 -> do
      -- A local bit of state for communication between callbacks:
      curLk <- newIORef lk0
      let inner' lk =
            -- Locking policy:  This is only used for build commands, which
            -- only need to lock the snapshot, not the global lock.  We
            -- trade in the lock here.
            do dir <- installationRootDeps
               -- Hand-over-hand locking:
               withUserFileLock go dir $ \lk2 -> do
                 liftIO $ writeIORef curLk lk2
                 liftIO $ munlockFile lk
                 logDebug "Starting to execute command inside EnvConfig"
                 inner lk2

      let inner'' lk = do
              bconfig <- lcLoadBuildConfig lc globalCompiler
              envConfig <- runRIO bconfig (setupEnv Nothing)
              runRIO envConfig (inner' lk)

      let getCompilerVersion = loadCompilerVersion go lc
      if skipDocker
          then runRIO (lcConfig lc) $ do
              forM_ mbefore id
              Nix.reexecWithOptionalShell (lcProjectRoot lc) getCompilerVersion (inner'' lk0)
              forM_ mafter id
          else runRIO (lcConfig lc) $
              Docker.reexecWithOptionalContainer
                       (lcProjectRoot lc)
                       mbefore
                       (runRIO (lcConfig lc) $
                          Nix.reexecWithOptionalShell (lcProjectRoot lc) getCompilerVersion (inner'' lk0))
                       mafter
                       (Just $ liftIO $
                            do lk' <- readIORef curLk
                               munlockFile lk')

-- | Load the configuration. Convenience function used
-- throughout this module.
loadConfigWithOpts
  :: GlobalOpts
  -> (LoadConfig -> IO a)
  -> IO a
loadConfigWithOpts go@GlobalOpts{..} inner = withRunnerGlobal go $ \runner -> do
    mstackYaml <- forM globalStackYaml resolveFile'
    runRIO runner $ do
        lc <- loadConfig globalConfigMonoid globalResolver mstackYaml
        -- If we have been relaunched in a Docker container, perform in-container initialization
        -- (switch UID, etc.).  We do this after first loading the configuration since it must
        -- happen ASAP but needs a configuration.
        forM_ globalDockerEntrypoint $ Docker.entrypoint (lcConfig lc)
        liftIO $ inner lc

withRunnerGlobal :: GlobalOpts -> (Runner -> IO a) -> IO a
withRunnerGlobal GlobalOpts{..} = withRunner
  globalLogLevel
  globalTimeInLog
  globalTerminal
  globalColorWhen
  globalTermWidth
  (isJust globalReExecVersion)

withMiniConfigAndLock
    :: GlobalOpts
    -> RIO MiniConfig ()
    -> IO ()
withMiniConfigAndLock go@GlobalOpts{..} inner = withRunnerGlobal go $ \runner -> do
    miniConfig <-
        runRIO runner $
        loadMiniConfig . lcConfig <$>
        loadConfigMaybeProject
          globalConfigMonoid
          globalResolver
          LCSNoProject
    runRIO miniConfig inner

-- | Unlock a lock file, if the value is Just
munlockFile :: MonadIO m => Maybe FileLock -> m ()
munlockFile Nothing = return ()
munlockFile (Just lk) = liftIO $ unlockFile lk

-- Plumbing for --test and --bench flags
withBuildConfigDot :: DotOpts -> GlobalOpts -> RIO EnvConfig () -> IO ()
withBuildConfigDot opts go f = withBuildConfig go' f
  where
    go' =
        (if dotTestTargets opts then set (globalOptsBuildOptsMonoidL.buildOptsMonoidTestsL) (Just True) else id) $
        (if dotBenchTargets opts then set (globalOptsBuildOptsMonoidL.buildOptsMonoidBenchmarksL) (Just True) else id)
        go

