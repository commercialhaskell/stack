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
repl
    :: (HasConfig r, HasBuildConfig r, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m)
    => [Text] -- ^ Targets.
    -> [String] -- ^ GHC options.
    -> FilePath
    -> Bool
    -> m ()
repl targets useropts ghciPath noload = do
    econfig <- asks getEnvConfig
    bconfig <- asks getBuildConfig
    pwd <- getWorkingDir
    locals <-
        liftM catMaybes $
        forM (M.toList (bcPackages bconfig)) $
        \(dir,validWanted) ->
             do cabalfp <- getCabalFileName dir
                name <- parsePackageNameFromFilePath cabalfp
                if validWanted && wanted pwd cabalfp name
                    then return (Just (name, cabalfp))
                    else return Nothing
    pkgs <-
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
                pkgOpts <- getPackageOpts (packageOpts pkg) (map fst locals) cabalfp
                srcfiles <- getPackageFiles (packageFiles pkg) Modules cabalfp
                return (packageName pkg, pkgOpts, S.toList srcfiles)
    let pkgopts = filter (not . badForGhci) (concat (map _2 pkgs))
        srcfiles
          | noload = []
          | otherwise = concatMap (map toFilePath . _3) pkgs
    $logInfo
        ("Configuring GHCi with the following packages: " <>
         T.intercalate ", " (map packageNameText (map _1 pkgs)))
    exec
        defaultEnvSettings
        ghciPath
        ("--interactive" : pkgopts <> srcfiles <> useropts)
  where
    wanted pwd cabalfp name = isInWantedList || targetsEmptyAndInDir
      where
        isInWantedList = elem (packageNameText name) targets
        targetsEmptyAndInDir = null targets || isParentOf (parent cabalfp) pwd
    badForGhci x =
        isPrefixOf "-O" x || elem x (words "-debug -threaded -ticky")
    _1 (x,_,_) = x
    _2 (_,x,_) = x
    _3 (_,_,x) = x
