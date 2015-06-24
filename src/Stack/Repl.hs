{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Run a REPL configured with the user's project(s).

module Stack.Repl where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Path
import           Path.IO
import           Stack.Build.Source
import           Stack.Exec
import           Stack.Package
import           Stack.Types

-- | Launch a GHCi REPL for the given local project targets with the
-- given options and configure it with the load paths and extensions
-- of those targets.
repl :: (HasConfig r, HasBuildConfig r, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m)
     => [Text] -- ^ Targets.
     -> [String] -- ^ GHC options.
     -> FilePath
     -> m ()
repl targets useropts ghciPath = do
    econfig <- asks getEnvConfig
    bconfig <- asks getBuildConfig
    pwd <- getWorkingDir
    pkgs <-
        liftM catMaybes $
        forM (M.toList (bcPackages bconfig)) $
        \(dir,validWanted) ->
             do cabalfp <- getCabalFileName dir
                name <- parsePackageNameFromFilePath cabalfp
                let config =
                        PackageConfig
                        { packageConfigEnableTests = True
                        , packageConfigEnableBenchmarks = True
                        , packageConfigFlags = localFlags mempty bconfig name
                        , packageConfigGhcVersion = envConfigGhcVersion econfig
                        , packageConfigPlatform = configPlatform
                              (getConfig bconfig)
                        }
                pkg <- readPackage config cabalfp
                if validWanted && wanted pwd cabalfp pkg
                    then do
                        pkgOpts <- getPackageOpts (packageOpts pkg) cabalfp
                        srcfiles <-
                            getPackageFiles
                                (packageFiles pkg)
                                Modules
                                cabalfp
                        return (Just (packageName pkg, pkgOpts, S.toList srcfiles))
                    else return Nothing
    let pkgopts = filter (not . badForGhci) (concat (map _2 pkgs))
        srcfiles = concatMap (map toFilePath . _3) pkgs
    $logInfo
        ("Configuring GHCi with the following packages: " <>
         T.intercalate ", " (map packageNameText (map _1 pkgs)))
    exec ghciPath ("--interactive" : pkgopts <> srcfiles <> useropts)
  where
    wanted pwd cabalfp pkg = isInWantedList || targetsEmptyAndInDir
      where
        isInWantedList = elem (packageNameText (packageName pkg)) targets
        targetsEmptyAndInDir = null targets || isParentOf (parent cabalfp) pwd
    badForGhci x =
        isPrefixOf "-O" x || elem x (words "-debug -threaded -ticky")
    _1 (x,_,_) = x
    _2 (_,x,_) = x
    _3 (_,_,x) = x
