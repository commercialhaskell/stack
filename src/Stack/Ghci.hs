{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

-- | Run a GHCi configured with the user's project(s).

module Stack.Ghci (GhciOpts(..),GhciPkgInfo(..), ghciSetup, ghci) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Function
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Distribution.ModuleName (ModuleName)
import           Distribution.Text (display)
import           Network.HTTP.Client.Conduit
import           Path
import           Prelude
import           Stack.Build.Source
import           Stack.Build.Target
import           Stack.Constants
import           Stack.Exec
import           Stack.Package
import           Stack.Types

-- | Command-line options for GHC.
data GhciOpts = GhciOpts
    {ghciTargets            :: ![Text]
    ,ghciArgs               :: ![String]
    ,ghciGhcCommand         :: !FilePath
    ,ghciNoLoadModules      :: !Bool
    ,ghciAdditionalPackages :: ![String]
    } deriving (Show,Eq)

-- | Necessary information to load a package or its components.
data GhciPkgInfo = GhciPkgInfo
  { ghciPkgName :: PackageName
  , ghciPkgOpts :: [String]
  , ghciPkgDir :: Path Abs Dir
  , ghciPkgModules :: Set ModuleName
  , ghciPkgFiles :: Set (Path Abs File)
  }

-- | Launch a GHCi session for the given local project targets with the
-- given options and configure it with the load paths and extensions
-- of those targets.
ghci :: (HasConfig r, HasBuildConfig r, HasHttpManager r,  HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m, MonadBaseControl IO m)
    => GhciOpts
    -> m ()
ghci GhciOpts{..} = do
    pkgs <- ghciSetup ghciTargets
    bconfig <- asks getBuildConfig
    let pkgopts = concatMap ghciPkgOpts pkgs
        srcfiles
          | ghciNoLoadModules = []
          | otherwise =
              concatMap (map display . S.toList . ghciPkgModules) pkgs
        odir =
            [ "-odir=" <> toFilePath (objectInterfaceDir bconfig)
            , "-hidir=" <> toFilePath (objectInterfaceDir bconfig)]
    $logInfo
        ("Configuring GHCi with the following packages: " <>
         T.intercalate ", " (map (packageNameText . ghciPkgName) pkgs))
    exec
        defaultEnvSettings
        ghciGhcCommand
        ("--interactive" : odir <> pkgopts <> srcfiles <> ghciArgs)

-- | Create a list of infos for each target containing necessary
-- information to load that package/components.
ghciSetup
    :: (HasConfig r, HasHttpManager r, HasBuildConfig r, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m, MonadBaseControl IO m)
    => [Text] -> m [GhciPkgInfo]
ghciSetup stringTargets = do
    (_,_,targets) <-
        parseTargetsFromBuildOpts
            AllowNoTargets
            defaultBuildOpts
            { boptsTargets = stringTargets
            }
    econfig <- asks getEnvConfig
    (_,_,_,sourceMap) <- loadSourceMap AllowNoTargets defaultBuildOpts
    locals <-
        liftM catMaybes $
        forM (M.toList (envConfigPackages econfig)) $
        \(dir,validWanted) ->
             do cabalfp <- getCabalFileName dir
                name <- parsePackageNameFromFilePath cabalfp
                if validWanted
                    then case M.lookup name targets of
                             Just simpleTargets ->
                                 return (Just (name, (cabalfp, simpleTargets)))
                             Nothing -> return Nothing
                    else return Nothing
    forM locals $
        \(name,(cabalfp,components)) ->
             makeGhciPkgInfo sourceMap (map fst locals) name cabalfp components

-- | Make information necessary to load the given package in GHCi.
makeGhciPkgInfo
    :: (MonadReader r m, HasEnvConfig r, MonadLogger m, MonadIO m, MonadCatch m)
    => SourceMap
    -> [PackageName]
    -> PackageName
    -> Path Abs File
    -> SimpleTarget
    -> m GhciPkgInfo
makeGhciPkgInfo sourceMap locals name cabalfp components = do
    econfig <- asks getEnvConfig
    bconfig <- asks getBuildConfig
    let config =
            PackageConfig
            { packageConfigEnableTests = True
            , packageConfigEnableBenchmarks = True
            , packageConfigFlags = localFlags mempty bconfig name
            , packageConfigGhcVersion = envConfigGhcVersion econfig
            , packageConfigPlatform = configPlatform (getConfig bconfig)
            }
    pkg <- readPackage config cabalfp
    (componentsOpts,generalOpts) <-
        getPackageOpts
            (packageOpts pkg)
            sourceMap
            locals
            cabalfp
    componentsModules <- getPackageModules (packageModules pkg) cabalfp
    (componentModFiles,generalFiles) <-
        getPackageFiles (packageFiles pkg) cabalfp
    let filterWithinWantedComponents m =
            M.elems
                (M.filterWithKey
                     (\k _ ->
                           case components of
                               STLocalComps cs -> S.member k cs
                               _ -> True)
                     m)
    return
        GhciPkgInfo
        { ghciPkgName = packageName pkg
        , ghciPkgOpts = filter
              (not . badForGhci)
              (generalOpts <>
               concat (filterWithinWantedComponents componentsOpts))
        , ghciPkgDir = parent cabalfp
        , ghciPkgModules = mconcat
              (filterWithinWantedComponents componentsModules)
        , ghciPkgFiles = generalFiles <>
          mconcat (filterWithinWantedComponents componentModFiles)
        }
  where badForGhci :: String -> Bool
        badForGhci x =
            isPrefixOf "-O" x || elem x (words "-debug -threaded -ticky")
