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
import           Data.Map.Strict (Map)
import qualified Data.Map as Map
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
import           Stack.Build.Types
import           Stack.Constants
import           Stack.Fetch as Fetch
import           Stack.GhcPkg
import           Stack.Package
import           Stack.Types
import           Stack.Types.Internal

type M env m = (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m,HasLogLevel env,HasEnvConfig env,HasTerminal env)

-- | Build
build :: M env m
      => (Set (Path Abs File) -> IO ()) -- ^ callback after discovering all local files
      -> BuildOpts
      -> m ()
build setLocalFiles bopts = do
    menv <- getMinimalEnvOverride

    (mbp, locals, extraToBuild, sourceMap) <- loadSourceMap bopts

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

    when (boptsPreFetch bopts) $
        preFetch plan

    if boptsDryrun bopts
        then printPlan (boptsFinalAction bopts) plan
        else executePlan menv bopts baseConfigOpts locals sourceMap plan
  where
    profiling = boptsLibProfile bopts || boptsExeProfile bopts

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
    bconfig <- asks getBuildConfig
    forM_
        (Map.keys (bcPackages bconfig))
        (distDirFromDir >=> removeTreeIfExists)
