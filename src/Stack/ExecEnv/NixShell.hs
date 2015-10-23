{-# LANGUAGE CPP, ConstraintKinds, DeriveDataTypeable, FlexibleContexts, MultiWayIf, NamedFieldPuns,
             OverloadedStrings, RankNTypes, RecordWildCards, ScopedTypeVariables, TemplateHaskell,
             TupleSections #-}

-- | Run commands in a nix-shell
module Stack.ExecEnv.NixShell
  (execWithShell
  ,reexecWithShell
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
import           Path
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


-- | If ExecEnv is enabled, re-runs the currently running OS command in a ExecEnv container.
-- Otherwise, runs the inner action.
-- 
-- This takes an optional release action which should be taken IFF control is
-- transfering away from the current process to the intra-container one.  The main use
-- for this is releasing a lock.  After launching reexecution, the host process becomes
-- nothing but an manager for the call into docker and thus may not hold the lock.
reexecWithShell
    :: M env m
    => Resolver  -- ^ Needed for installing ghc in the nix-shell
    -> Maybe (Path Abs Dir)
    -> Maybe (m ())
    -> IO ()
    -> Maybe (m ())
    -> Maybe (m ())
    -> m ()
reexecWithShell resolver mprojectRoot =
    execWithShell resolver mprojectRoot getCmdArgs
  where
    getCmdArgs {-envOverride imageInfo-} = do
        args <-
            fmap
                (("--" ++ reExecArgName ++ "=" ++ showVersion Meta.version) :)
                (liftIO getArgs)
        exePath <- liftIO getExecutablePath
        return (exePath, args)

-- | If ExecEnv is enabled, re-runs the OS command returned by the second argument in a
-- ExecEnv container.  Otherwise, runs the inner action.
--
-- This takes an optional release action just like `reexecWithOptionalContainer`.
execWithShell
    :: M env m
    => Resolver
    -> Maybe (Path Abs Dir)
    -> ({-EnvOverride -> Inspect ->-} m (FilePath,[String])) --,[(String,String)],[Mount]))
    -> Maybe (m ())
    -> IO ()
    -> Maybe (m ())
    -> Maybe (m ())
    -> m ()
execWithShell resolver mprojectRoot getCmdArgs mbefore inner mafter mrelease =
  do config <- asks getConfig
     inShell <- getInShell
     isReExec <- asks getReExec
     let envType = execEnvType (configExecEnv config)
     if | inShell && not isReExec && (isJust mbefore || isJust mafter) ->
            throwM OnlyOnHostException
        | inShell ->
            liftIO (do inner
                       exitSuccess)
        | isNothing envType ->
            do fromMaybeAction mbefore
               liftIO inner
               fromMaybeAction mafter
               liftIO exitSuccess
        | envType == Just NixShellExecEnv ->
            do fromMaybeAction mrelease
               runShellAndExit resolver
                 getCmdArgs
                 mprojectRoot
                 (fromMaybeAction mbefore)
                 (fromMaybeAction mafter)
  where
    fromMaybeAction Nothing = return ()
    fromMaybeAction (Just hook) = hook
    
runShellAndExit :: M env m
                => Resolver
                -> m (String, [String])
                -> t
                -> m ()
                -> m ()
                -> m ()
runShellAndExit resolver getCmdArgs mprojectRoot before after = do
     config <- asks getConfig
     envOverride <- getEnvOverride (configPlatform config)
     (cmnd,args) <- getCmdArgs
     before
     let ghcInNix = case resolver of
                     ResolverSnapshot (LTS x y) ->
                       "haskell.packages.lts-" ++ (show x) ++ "_" ++ (show y) ++ ".ghc"
                     _ -> "ghc"
         nixpkgs = [ghcInNix] ++ (map show (execEnvPackages (configExecEnv config)))
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
       Right () -> do after
                      liftIO exitSuccess


-- | 'True' if we are currently running inside a ExecEnv.
getInShell :: (MonadIO m) => m Bool
getInShell = liftIO (isJust <$> lookupEnv inContainerEnvVar)

-- | Environment variable used to indicate stack is running in container.
inContainerEnvVar :: String
inContainerEnvVar = concat [map toUpper stackProgName,"_IN_CONTAINER"]

-- | Command-line option for @--internal-re-exec@.
reExecArgName :: String
reExecArgName = "internal-re-exec-version"

-- | A shortcut
type M env m = (MonadIO m,MonadReader env m,MonadLogger m,MonadBaseControl IO m,MonadCatch m
               ,HasConfig env,HasTerminal env,HasReExec env,HasHttpManager env,MonadMask m)
