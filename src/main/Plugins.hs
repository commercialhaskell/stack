{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Dynamically look up available executables.
module Plugins
  ( Plugin
  , pluginPrefix
  , pluginName
  , pluginSummary
  , pluginProc

  , Plugins
  , findPlugins
  , listPlugins
  , lookupPlugin
  , callPlugin

  , PluginException (..)
  ) where

import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Data.Conduit
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Conduit.List as CL
import Data.Conduit.Lift (evalStateC)
import qualified Data.List as L
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Monoid
import System.Directory
import System.Process (CreateProcess, proc, readProcessWithExitCode, createProcess, waitForProcess)
import System.FilePath ((</>), getSearchPath, splitExtension)
import System.Exit (ExitCode (..))

-- | Represents a runnable plugin.
-- Plugins must be discovered via `findPlugins`.
data Plugin = Plugin
  { _pluginPrefix :: !Text
  , _pluginName :: !Text
  , _pluginSummary :: !Text
  }
  deriving (Show)

-- | The program being plugged into.
pluginPrefix :: Plugin -> Text
pluginPrefix = _pluginPrefix

-- | The name of this plugin (without the prefix).
pluginName :: Plugin -> Text
pluginName = _pluginName

-- | A summary of what this plugin does
pluginSummary :: Plugin -> Text
pluginSummary = _pluginSummary

-- | Describes how to create a process out of a plugin and arguments.
-- You may use Data.Process and Data.Conduit.Process
-- to manage the process's stdin, stdout, and stderr in various ways.
pluginProc :: Plugin -> [String] -> CreateProcess
pluginProc = proc . pluginProcessName

-- Not exported
pluginProcessName :: Plugin -> String
pluginProcessName p = unpack $ pluginPrefix p <> "-" <> pluginName p


-- | Represents the plugins available to a given program.
-- See: `findPlugins`.
data Plugins = Plugins
  { _pluginsPrefix :: !Text
  , _pluginsMap :: !(HashMap Text Plugin)
  }
  deriving (Show)


-- | Find the plugins for a given program by inspecting everything on the PATH.
-- Any program that is prefixed with the given name and responds
-- to the `--summary` flag by writing one line to stdout
-- is considered a plugin.
findPlugins :: Text -> IO Plugins
findPlugins t = fmap (Plugins t)
   $ discoverPlugins t
  $$ awaitForever (toPlugin t)
  =$ CL.fold insertPlugin HashMap.empty
  where
    insertPlugin m p = HashMap.insert (pluginName p) p m

toPlugin :: (MonadIO m) => Text -> Text -> Producer m Plugin
toPlugin prefix name = do
  let proc' = unpack $ prefix <> "-" <> name
  (exit, out, _err) <- liftIO $ readProcessWithExitCode proc' ["--summary"] ""
  case exit of
    ExitSuccess -> case T.lines (pack out) of
      [summary] -> yield $ Plugin
        { _pluginPrefix = prefix
        , _pluginName = name
        , _pluginSummary = summary
        }
      _ -> return ()
    _ -> return ()


-- | Things that can go wrong when using `callPlugin`.
-- Note: it may be wiser to catch PluginExitFailure, rather than display it.
data PluginException
  = PluginNotFound !Plugins !Text
  | PluginExitFailure !Plugin !Int
  deriving (Typeable)
instance Show PluginException where
  show (PluginNotFound plugins requestedPlugin)
     = "Plugin not found for '" ++ unpack (_pluginsPrefix plugins)
    ++ "': '" ++ unpack requestedPlugin ++ "'"
  show (PluginExitFailure plugin exitCode)
     = "Plugin '" ++ unpack (_pluginName plugin)
    ++ "' for '" ++ unpack (_pluginPrefix plugin)
    ++ "' failed with exit code: "
    ++ show exitCode
instance Exception PluginException

-- | Look up a particular plugin by name.
lookupPlugin :: Plugins -> Text -> Maybe Plugin
lookupPlugin ps t = HashMap.lookup t $ _pluginsMap ps

-- | List the available plugins.
listPlugins :: Plugins -> [Plugin]
listPlugins = HashMap.elems . _pluginsMap

-- | A convenience wrapper around lookupPlugin and pluginProc.
-- Handles stdin, stdout, and stderr are all inherited by the plugin.
-- Throws PluginException.
callPlugin :: (MonadIO m, MonadThrow m)
  => Plugins -> Text -> [String] -> m ()
callPlugin ps name args = case lookupPlugin ps name of
  Nothing -> throwM $ PluginNotFound ps name
  Just plugin -> do
    exit <- liftIO $ do
      (_, _, _, process) <- createProcess $ pluginProc plugin args
      waitForProcess process
    case exit of
      ExitFailure i -> throwM $ PluginExitFailure plugin i
      ExitSuccess -> return ()


discoverPlugins :: MonadIO m => Text -> Producer m Text
discoverPlugins t
  = getPathDirs
 $= clNub -- unique dirs on path
 $= awaitForever (executablesPrefixed $ unpack $ t <> "-")
 $= CL.map pack
 $= clNub -- unique executables

executablesPrefixed :: (MonadIO m) => FilePath -> FilePath -> Producer m FilePath
executablesPrefixed prefix dir
  = pathToContents dir
 $= CL.filter (L.isPrefixOf prefix)
 $= clFilterM (fileExistsIn dir)
 $= clFilterM (isExecutableIn dir)
 $= CL.mapMaybe (L.stripPrefix prefix . dropExeExt)

-- | Drop the .exe extension if present
dropExeExt :: FilePath -> FilePath
dropExeExt fp
    | y == ".exe" = x
    | otherwise   = fp
  where
    (x, y) = splitExtension fp

getPathDirs :: (MonadIO m) => Producer m FilePath
getPathDirs = liftIO getSearchPath >>= mapM_ yield

pathToContents :: (MonadIO m) => FilePath -> Producer m FilePath
pathToContents dir = do
  exists <- liftIO $ doesDirectoryExist dir
  when exists $ do
    contents <- liftIO $ getDirectoryContents dir
    CL.sourceList contents

fileExistsIn :: (MonadIO m) => FilePath -> FilePath -> m Bool
fileExistsIn dir file = liftIO $ doesFileExist $ dir </> file

isExecutableIn :: (MonadIO m) => FilePath -> FilePath -> m Bool
isExecutableIn dir file = liftIO $ do
  perms <- getPermissions $ dir </> file
  return (executable perms)

clFilterM :: Monad m => (a -> m Bool) -> Conduit a m a
clFilterM pred' = awaitForever $ \a -> do
  predPassed <- lift $ pred' a
  when predPassed $ yield a

clNub :: (Monad m, Eq a, Hashable a)
  => Conduit a m a
clNub = evalStateC HashSet.empty clNubState

clNubState :: (Monad m, Eq a, Hashable a)
  => Conduit a (StateT (HashSet a) m) a
clNubState = awaitForever $ \a -> do
  seen <- lift get
  unless (HashSet.member a seen) $ do
    lift $ put $ HashSet.insert a seen
    yield a
