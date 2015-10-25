{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

-- | Run commands in a nix-shell
module Stack.Nix
  (execWithOptionalShell
  ,reexecWithOptionalShell
  ,reExecArgName
  ) where

import           Control.Applicative
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Catch (throwM,MonadCatch,MonadMask)
import           Control.Monad.IO.Class (MonadIO,liftIO)
import           Control.Monad.Logger (MonadLogger)
import           Control.Monad.Reader (MonadReader,asks)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Char (toUpper)
import           Data.List (intersperse)
import           Data.Maybe
import           Data.Streaming.Process (ProcessExitedUnsuccessfully(..))
import           Data.Version (showVersion)
import           Network.HTTP.Client.Conduit (HasHttpManager)
import qualified Paths_stack as Meta
import           Prelude -- Fix redundant import warnings
import           Stack.Constants (stackProgName)
import           Stack.Docker (StackDockerException(OnlyOnHostException))
import           Stack.Types
import           Stack.Types.Internal
import           System.Environment (lookupEnv,getArgs,getExecutablePath)
import           System.Exit (exitSuccess, exitWith)
import           System.Process.Read
import           System.Process.Run
import           System.Process (CreateProcess(delegate_ctlc))


-- | If Nix is enabled, re-runs the currently running OS command in a Nix container.
-- Otherwise, runs the inner action.
--
-- This takes an optional release action which should be taken IFF control is
-- transfering away from the current process to the intra-container one.  The main use
-- for this is releasing a lock.  After launching reexecution, the host process becomes
-- nothing but an manager for the call into docker and thus may not hold the lock.
reexecWithOptionalShell
    :: M env m
    => IO ()
    -> m ()
reexecWithOptionalShell =
    execWithOptionalShell getCmdArgs
  where
    getCmdArgs = do
        args <-
            fmap
                (("--" ++ reExecArgName ++ "=" ++ showVersion Meta.version) :)
                (liftIO getArgs)
        exePath <- liftIO getExecutablePath
        return (exePath, args)

-- | If Nix is enabled, re-runs the OS command returned by the second argument in a
-- Nix container.  Otherwise, runs the inner action.
--
-- This takes an optional release action just like `reexecWithOptionalShell`.
execWithOptionalShell
    :: M env m
    => m (FilePath,[String])
    -> IO ()
    -> m ()
execWithOptionalShell getCmdArgs inner =
  do config <- asks getConfig
     inShell <- getInShell
     isReExec <- asks getReExec
     if | inShell && not isReExec ->
            throwM OnlyOnHostException
        | inShell ->
            liftIO (do inner
                       exitSuccess)
        | not (nixEnable (configNix config)) ->
            do liftIO inner
               liftIO exitSuccess
        | otherwise ->
            do runShellAndExit
                 getCmdArgs

runShellAndExit :: M env m
                => m (String, [String])
                -> m ()
runShellAndExit getCmdArgs = do
     config <- asks getConfig
     envOverride <- getEnvOverride (configPlatform config)
     (cmnd,args) <- getCmdArgs
     resolver <- bcResolver <$> asks getBuildConfig
     let ghcInNix = case resolver of
                     ResolverSnapshot (LTS x y) ->
                       "haskell.packages.lts-" ++ show x ++ "_" ++ show y ++ ".ghc"
                     _ -> "ghc"
         nixpkgs = [ghcInNix] ++ (map show (nixPackages (configNix config)))
         fullArgs = (concat [["--pure", "-p"]
                            ,nixpkgs
                            ,["--command"]
                            ,[(concat $ intersperse " "
                                  ("export":(inContainerEnvVar++"=1"):";":cmnd:args))]
                            ])
     liftIO $ putStrLn $ "Using a nix-shell environment with nix packages: " ++
                               (concat $ intersperse ", " nixpkgs)
     e <- try (callProcess'
                 (\cp -> cp { delegate_ctlc = False })
                 Nothing
                 envOverride
                 "nix-shell"
                 fullArgs)
     case e of
       Left (ProcessExitedUnsuccessfully _ ec) -> liftIO (exitWith ec)
       Right () -> liftIO exitSuccess

-- | 'True' if we are currently running inside a Nix.
getInShell :: (MonadIO m) => m Bool
getInShell = liftIO (isJust <$> lookupEnv inContainerEnvVar)

-- | Environment variable used to indicate stack is running in container.
inContainerEnvVar :: String
inContainerEnvVar = concat [map toUpper stackProgName,"_IN_CONTAINER"]

-- | Command-line option for @--internal-re-exec@.
reExecArgName :: String
reExecArgName = "internal-re-exec-version"

type M env m =
  (MonadIO m
  ,MonadReader env m
  ,MonadLogger m
  ,MonadBaseControl IO m
  ,MonadCatch m
  ,HasBuildConfig env
  ,HasTerminal env
  ,HasReExec env
  ,HasHttpManager env
  ,MonadMask m
  )
