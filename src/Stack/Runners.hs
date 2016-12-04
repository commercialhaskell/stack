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
    , withBuildConfig
    , withBuildConfigExt
    , loadConfigWithOpts
    , loadCompilerVersion
    , withUserFileLock
    , munlockFile
    ) where

import           Control.Monad hiding (forM)
import           Control.Monad.Logger
import           Control.Exception.Lifted as EL
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.IORef
import           Data.Traversable
import           Path
import           Path.IO
import           Stack.Config
import qualified Stack.Docker as Docker
import qualified Stack.Nix as Nix
import           Stack.Setup
import           Stack.Types.Compiler (CompilerVersion)
import           Stack.Types.Config
import           Stack.Types.StackT
import           System.Environment (getEnvironment)
import           System.IO
import           System.FileLock

loadCompilerVersion :: GlobalOpts
                    -> LoadConfig (StackT () IO)
                    -> IO CompilerVersion
loadCompilerVersion go lc = do
    bconfig <- runStackTGlobal () go $
      lcLoadBuildConfig lc (globalCompiler go)
    return $ view wantedCompilerVersionL bconfig

-- | Enforce mutual exclusion of every action running via this
-- function, on this path, on this users account.
--
-- A lock file is created inside the given directory.  Currently,
-- stack uses locks per-snapshot.  In the future, stack may refine
-- this to an even more fine-grain locking approach.
--
withUserFileLock :: (MonadBaseControl IO m, MonadIO m)
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
            EL.bracket (liftIO $ tryLockFile (toFilePath pth) Exclusive)
                       (maybe (return ()) (liftIO . unlockFile))
                       (\fstTry ->
                        case fstTry of
                          Just lk -> EL.finally (act $ Just lk) (liftIO $ unlockFile lk)
                          Nothing ->
                            do let chatter = globalLogLevel go /= LevelOther "silent"
                               when chatter $
                                 liftIO $ hPutStrLn stderr $ "Failed to grab lock ("++show pth++
                                                     "); other stack instance running.  Waiting..."
                               EL.bracket (liftIO $ lockFile (toFilePath pth) Exclusive)
                                          (liftIO . unlockFile)
                                          (\lk -> do
                                            when chatter $
                                              liftIO $ hPutStrLn stderr "Lock acquired, proceeding."
                                            act $ Just lk))
        else act Nothing

withConfigAndLock
    :: GlobalOpts
    -> StackT Config IO ()
    -> IO ()
withConfigAndLock go@GlobalOpts{..} inner = do
    lc <- loadConfigWithOpts go
    withUserFileLock go (configStackRoot $ lcConfig lc) $ \lk ->
        runStackTGlobal (lcConfig lc) go $
            Docker.reexecWithOptionalContainer
                (lcProjectRoot lc)
                Nothing
                (runStackTGlobal (lcConfig lc) go inner)
                Nothing
                (Just $ munlockFile lk)

-- | Loads global config, ignoring any configuration which would be
-- loaded due to $PWD.
withGlobalConfigAndLock
    :: GlobalOpts
    -> StackT Config IO ()
    -> IO ()
withGlobalConfigAndLock go@GlobalOpts{..} inner = do
    lc <- runStackTGlobal () go $
        loadConfigMaybeProject globalConfigMonoid Nothing Nothing
    withUserFileLock go (configStackRoot $ lcConfig lc) $ \_lk ->
        runStackTGlobal (lcConfig lc) go inner

-- For now the non-locking version just unlocks immediately.
-- That is, there's still a serialization point.
withBuildConfig
    :: GlobalOpts
    -> StackT EnvConfig IO ()
    -> IO ()
withBuildConfig go inner =
    withBuildConfigAndLock go (\lk -> do munlockFile lk
                                         inner)

withBuildConfigAndLock
    :: GlobalOpts
    -> (Maybe FileLock -> StackT EnvConfig IO ())
    -> IO ()
withBuildConfigAndLock go inner =
    withBuildConfigExt go Nothing inner Nothing

withBuildConfigExt
    :: GlobalOpts
    -> Maybe (StackT Config IO ())
    -- ^ Action to perform before the build.  This will be run on the host
    -- OS even if Docker is enabled for builds.  The build config is not
    -- available in this action, since that would require build tools to be
    -- installed on the host OS.
    -> (Maybe FileLock -> StackT EnvConfig IO ())
    -- ^ Action that uses the build config.  If Docker is enabled for builds,
    -- this will be run in a Docker container.
    -> Maybe (StackT Config IO ())
    -- ^ Action to perform after the build.  This will be run on the host
    -- OS even if Docker is enabled for builds.  The build config is not
    -- available in this action, since that would require build tools to be
    -- installed on the host OS.
    -> IO ()
withBuildConfigExt go@GlobalOpts{..} mbefore inner mafter = do
    lc <- loadConfigWithOpts go

    withUserFileLock go (configStackRoot $ lcConfig lc) $ \lk0 -> do
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
                 $logDebug "Starting to execute command inside EnvConfig"
                 inner lk2

      let inner'' lk = do
              bconfig <- runStackTGlobal () go $
                  lcLoadBuildConfig lc globalCompiler
              envConfig <-
                 runStackTGlobal
                     bconfig go
                     (setupEnv Nothing)
              runStackTGlobal
                  envConfig
                  go
                  (inner' lk)

      let getCompilerVersion = loadCompilerVersion go lc
      runStackTGlobal (lcConfig lc) go $
        Docker.reexecWithOptionalContainer
                 (lcProjectRoot lc)
                 mbefore
                 (runStackTGlobal (lcConfig lc) go $
                    Nix.reexecWithOptionalShell (lcProjectRoot lc) getCompilerVersion (inner'' lk0))
                 mafter
                 (Just $ liftIO $
                      do lk' <- readIORef curLk
                         munlockFile lk')

-- | Load the configuration. Convenience function used
-- throughout this module.
loadConfigWithOpts :: GlobalOpts -> IO (LoadConfig (StackT () IO))
loadConfigWithOpts go@GlobalOpts{..} = do
    mstackYaml <- forM globalStackYaml resolveFile'
    runStackTGlobal () go $ do
        lc <- loadConfig globalConfigMonoid globalResolver mstackYaml
        -- If we have been relaunched in a Docker container, perform in-container initialization
        -- (switch UID, etc.).  We do this after first loading the configuration since it must
        -- happen ASAP but needs a configuration.
        case globalDockerEntrypoint of
            Just de -> Docker.entrypoint (lcConfig lc) de
            Nothing -> return ()
        return lc

withMiniConfigAndLock
    :: GlobalOpts
    -> StackT MiniConfig IO ()
    -> IO ()
withMiniConfigAndLock go@GlobalOpts{..} inner = do
    miniConfig <- runStackTGlobal () go $ do
        lc <- loadConfigMaybeProject globalConfigMonoid globalResolver Nothing
        loadMiniConfig (lcConfig lc)
    runStackTGlobal miniConfig go inner

-- | Unlock a lock file, if the value is Just
munlockFile :: MonadIO m => Maybe FileLock -> m ()
munlockFile Nothing = return ()
munlockFile (Just lk) = liftIO $ unlockFile lk
