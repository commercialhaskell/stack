{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Run a IDE configured with the user's project(s).

module Stack.Ide
    (ide, getPackageOptsAndTargetFiles)
    where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import           Data.Text (Text)
import           Distribution.System
import           Network.HTTP.Client.Conduit
import           Path
import           Path.IO
import           Stack.Constants
import           Stack.Ghci (GhciPkgInfo(..), ghciSetup)
import           Stack.Package
import           Stack.Types
import           Stack.Types.Internal
import           System.Environment (lookupEnv)
import           System.Process.Run
import           System.FilePath (searchPathSeparator)
-- | Launch a GHCi IDE for the given local project targets with the
-- given options and configure it with the load paths and extensions
-- of those targets.
ide
    :: (HasConfig r, HasBuildConfig r, HasTerminal r, HasLogLevel r, MonadMask m, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m, MonadBaseControl IO m, HasHttpManager r)
    => [Text] -- ^ Targets.
    -> [String] -- ^ GHC options.
    -> m ()
ide targets useropts = do
    (_realTargets,_,pkgs) <- ghciSetup Nothing targets
    pwd <- getWorkingDir
    (pkgopts,_srcfiles) <-
        liftM mconcat $ forM pkgs $ getPackageOptsAndTargetFiles pwd
    localdb <- packageDatabaseLocal
    depsdb <- packageDatabaseDeps
    mpath <- liftIO $ lookupEnv "PATH"
    bindirs <- extraBinDirs `ap` return True {- include local bin -}
    let pkgdbs =
            ["--package-db=" <> toFilePath depsdb <> [searchPathSeparator] <> toFilePath localdb]
        paths =
            [ "--ide-backend-tools-path=" <>
              intercalate [searchPathSeparator] (map toFilePath bindirs) <>
              (maybe "" (searchPathSeparator :) mpath)]
        args =
            ["--verbose"] <> ["--include=" <> includeDirs pkgopts] <>
            ["--local-work-dir=" ++ toFilePath pwd] <>
            map ("--ghc-option=" ++) useropts <>
            paths <>
            pkgopts <>
            pkgdbs
    menv <- getMinimalEnvOverride
    Platform _ os <- asks getPlatform
    when
        (os == OSX)
        (catch (callProcess (Just pwd) menv "stty" ["cbreak", "-imaxbel"])
               (\(_ :: ProcessExitedUnsuccessfully) -> undefined))
    callProcess (Just pwd) menv "stack-ide" args
  where
    includeDirs pkgopts =
        intercalate
            [searchPathSeparator]
            (mapMaybe
                 (stripPrefix "--ghc-option=-i")
                 pkgopts)

-- | Get options and target files for the given package info.
getPackageOptsAndTargetFiles
    :: (MonadThrow m, MonadIO m, MonadReader env m, HasEnvConfig env)
    => Path Abs Dir -> GhciPkgInfo -> m ([FilePath], [FilePath])
getPackageOptsAndTargetFiles pwd pkg = do
    dist <- distDirFromDir (ghciPkgDir pkg)
    autogen <- return (autogenDir dist)
    paths_foo <-
        liftM
            (autogen </>)
            (parseRelFile
                 ("Paths_" ++ packageNameString (ghciPkgName pkg) ++ ".hs"))
    paths_foo_exists <- fileExists paths_foo
    return
        ( ["--dist-dir=" <> toFilePath dist] ++
          map ("--ghc-option=" ++) (ghciPkgOpts pkg)
        , mapMaybe
              (fmap toFilePath . stripDir pwd)
              (S.toList (ghciPkgCFiles pkg) <> S.toList (ghciPkgModFiles pkg) <>
               if paths_foo_exists
                   then [paths_foo]
                   else []))
