{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

-- | Run a GHCi configured with the user's project(s).

module Stack.Ghci where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Distribution.ModuleName (ModuleName)
import           Distribution.Text (display)
import           Network.HTTP.Client.Conduit
import           Path
import           Path.IO
import           Stack.Build.Source
import           Stack.Constants
import           Stack.Exec
import           Stack.Package
import           Stack.Types

data GhciOpts = GhciOpts
    {ghciTargets :: [Text]
    ,ghciArgs :: [String]
    ,ghciGhcCommand :: FilePath
    ,ghciNoLoadModules :: Bool
    ,ghciAdditionalPackages :: [String]
    } deriving (Show,Eq)

-- | Launch a GHCi session for the given local project targets with the
-- given options and configure it with the load paths and extensions
-- of those targets.
ghci :: (HasConfig r, HasBuildConfig r, HasHttpManager r,  HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m, MonadBaseControl IO m)
    => GhciOpts
    -> m ()
ghci GhciOpts{..} = do
    pkgs <- ghciSetup ghciTargets
    config <- asks getBuildConfig
    let pkgopts = concatMap ghciPkgOpts pkgs
        srcfiles
          | ghciNoLoadModules = []
          | otherwise = concatMap (map display . S.toList . ghciPkgModules) pkgs
        odir = ["-odir=" <> toFilePath (objectInterfaceDir config)
               ,"-hidir=" <> toFilePath (objectInterfaceDir config)]
    $logInfo
        ("Configuring GHCi with the following packages: " <>
         T.intercalate ", " (map (packageNameText . ghciPkgName) pkgs))
    exec
        defaultEnvSettings
        ghciGhcCommand
        ("--interactive" : odir <> pkgopts <> srcfiles <> ghciArgs)

data GhciPkgInfo = GhciPkgInfo
  { ghciPkgName :: PackageName
  , ghciPkgOpts :: [String]
  , ghciPkgDir :: Path Abs Dir
  , ghciPkgModules :: Set ModuleName
  , ghciPkgFiles :: Set (Path Abs File)
  }

ghciSetup
    :: (HasConfig r, HasHttpManager r, HasBuildConfig r, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m, MonadBaseControl IO m)
    => [Text] -> m [GhciPkgInfo]
ghciSetup targets = do
    econfig <- asks getEnvConfig
    bconfig <- asks getBuildConfig
    pwd <- getWorkingDir
    (_,_,_,sourceMap) <- loadSourceMap defaultBuildOpts
    locals <-
        liftM catMaybes $
        forM (M.toList (envConfigPackages econfig)) $
        \(dir,validWanted) ->
             do cabalfp <- getCabalFileName dir
                name <- parsePackageNameFromFilePath cabalfp
                if validWanted && wanted pwd cabalfp name
                    then return (Just (name, cabalfp))
                    else return Nothing
    let findTarget x = find ((x ==) . packageNameText . fst) locals
        unmetTargets = filter (isNothing . findTarget) targets
    when (not (null unmetTargets)) $ throwM (TargetsNotFound unmetTargets)
    forM locals $
        \(name,cabalfp) ->
             do let config =
                        PackageConfig
                        { packageConfigEnableTests = True
                        , packageConfigEnableBenchmarks = True
                        , packageConfigFlags = localFlags mempty bconfig name
                        , packageConfigGhcVersion = envConfigGhcVersion econfig
                        , packageConfigPlatform = configPlatform
                              (getConfig bconfig)
                        }
                pkg <- readPackage config cabalfp
                pkgOpts <-
                    getPackageOpts (packageOpts pkg) sourceMap (map fst locals) cabalfp
                modules <- getPackageModules (packageModules pkg) cabalfp
                moduleFiles <- getPackageFiles (packageFiles pkg) Modules cabalfp
                return
                    GhciPkgInfo
                    { ghciPkgName = packageName pkg
                    , ghciPkgOpts = filter (not . badForGhci) pkgOpts
                    , ghciPkgDir = parent cabalfp
                    , ghciPkgModules = modules
                    , ghciPkgFiles = moduleFiles
                    }
  where
    wanted pwd cabalfp name = isInWantedList || targetsEmptyAndInDir
      where
        isInWantedList = elem (packageNameText name) targets
        targetsEmptyAndInDir = null targets || isParentOf (parent cabalfp) pwd
    badForGhci :: String -> Bool
    badForGhci x =
        isPrefixOf "-O" x || elem x (words "-debug -threaded -ticky")

data GhciSetupException =
    TargetsNotFound [Text]
    deriving Typeable

instance Exception GhciSetupException
instance Show GhciSetupException where
    show (TargetsNotFound targets) = unlines
        [ "Couldn't find targets: " ++ T.unpack (T.unwords targets)
        , "(expecting package names)"
        ]
