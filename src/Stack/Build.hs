{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Build project(s).

module Stack.Build
  (build
  ,clean
  ,withLoadPackage
  ,mkBaseConfigOpts)
  where

import           Control.Monad
import           Control.Monad.Catch (MonadCatch, MonadMask)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Resource
import           Data.Function
import qualified Data.Map as Map
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Path
import           Path.IO
import           Prelude hiding (FilePath, writeFile)
import           Stack.Build.ConstructPlan
import           Stack.Build.Execute
import           Stack.Build.Haddock
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Build.Target
import           Stack.Constants
import           Stack.Fetch as Fetch
import           Stack.GhcPkg
import           Stack.Package
import           Stack.Types
import           Stack.Types.Internal
import           System.FileLock (FileLock, unlockFile)

type M env m = (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m,HasLogLevel env,HasEnvConfig env,HasTerminal env)

-- | Build.
--
--   If a buildLock is passed there is an important contract here.  That lock must
--   protect the snapshot, and it must be safe to unlock it if there are no further
--   modifications to the snapshot to be performed by this build.
build :: M env m
      => (Set (Path Abs File) -> IO ()) -- ^ callback after discovering all local files
      -> Maybe FileLock
      -> BuildOpts
      -> m ()
build setLocalFiles mbuildLk bopts = do
    menv <- getMinimalEnvOverride

    (_, mbp, locals, extraToBuild, sourceMap) <- loadSourceMap NeedTargets bopts

    -- Set local files, necessary for file watching
    stackYaml <- asks $ bcStackYaml . getBuildConfig
    liftIO $ setLocalFiles
           $ Set.insert stackYaml
           $ Set.unions
           $ map lpFiles locals

    (installedMap, locallyRegistered) <-
        getInstalled menv
                     GetInstalledOpts
                         { getInstalledProfiling = profiling
                         , getInstalledHaddock   = shouldHaddockDeps bopts }
                     sourceMap

    baseConfigOpts <- mkBaseConfigOpts bopts
    plan <- withLoadPackage menv $ \loadPackage ->
        constructPlan mbp baseConfigOpts locals extraToBuild locallyRegistered loadPackage sourceMap installedMap

    -- If our work to do is all local, let someone else have a turn with the snapshot.
    -- They won't damage what's already in there.
    case (mbuildLk, allLocal plan) of
       -- NOTE: This policy is too conservative.  In the future we should be able to
       -- schedule unlocking as an Action that happens after all non-local actions are
       -- complete.
      (Just lk,True) -> do $logDebug "All installs are local; releasing snapshot lock early."
                           liftIO $ unlockFile lk
      _ -> return ()

    when (boptsPreFetch bopts) $
        preFetch plan

    if boptsDryrun bopts
        then printPlan plan
        else executePlan menv bopts baseConfigOpts locals sourceMap plan
  where
    profiling = boptsLibProfile bopts || boptsExeProfile bopts

-- | If all the tasks are local, they don't mutate anything outside of our local directory.
allLocal :: Plan -> Bool
allLocal =
    all (== Local) .
    map taskLocation .
    Map.elems .
    planTasks

-- | Get the @BaseConfigOpts@ necessary for constructing configure options
mkBaseConfigOpts :: (MonadIO m, MonadReader env m, HasEnvConfig env, MonadThrow m)
                 => BuildOpts -> m BaseConfigOpts
mkBaseConfigOpts bopts = do
    snapDBPath <- packageDatabaseDeps
    localDBPath <- packageDatabaseLocal
    snapInstallRoot <- installationRootDeps
    localInstallRoot <- installationRootLocal
    return BaseConfigOpts
        { bcoSnapDB = snapDBPath
        , bcoLocalDB = localDBPath
        , bcoSnapInstallRoot = snapInstallRoot
        , bcoLocalInstallRoot = localInstallRoot
        , bcoBuildOpts = bopts
        }

-- | Provide a function for loading package information from the package index
withLoadPackage :: ( MonadIO m
                   , HasHttpManager env
                   , MonadReader env m
                   , MonadBaseControl IO m
                   , MonadCatch m
                   , MonadLogger m
                   , HasEnvConfig env)
                => EnvOverride
                -> ((PackageName -> Version -> Map FlagName Bool -> IO Package) -> m a)
                -> m a
withLoadPackage menv inner = do
    econfig <- asks getEnvConfig
    withCabalLoader menv $ \cabalLoader ->
        inner $ \name version flags -> do
            bs <- cabalLoader $ PackageIdentifier name version -- TODO automatically update index the first time this fails
            readPackageBS (depPackageConfig econfig flags) bs
  where
    -- | Package config to be used for dependencies
    depPackageConfig :: EnvConfig -> Map FlagName Bool -> PackageConfig
    depPackageConfig econfig flags = PackageConfig
        { packageConfigEnableTests = False
        , packageConfigEnableBenchmarks = False
        , packageConfigFlags = flags
        , packageConfigGhcVersion = envConfigGhcVersion econfig
        , packageConfigPlatform = configPlatform (getConfig econfig)
        }

-- | Reset the build (remove Shake database and .gen files).
clean :: (M env m) => m ()
clean = do
    econfig <- asks getEnvConfig
    forM_
        (Map.keys (envConfigPackages econfig))
        (distDirFromDir >=> removeTreeIfExists)
