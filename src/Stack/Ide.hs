{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Run a IDE configured with the user's package(s).

module Stack.Ide
    (ide, getPackageOptsAndTargetFiles, ideGhciOpts)
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
import           Path.Extra (toFilePathNoTrailingSep)
import           Path.IO
import           Stack.Constants
import           Stack.Ghci (GhciPkgInfo(..), GhciOpts(..), ghciSetup)
import           Stack.Package
import           Stack.Types
import           Stack.Types.Internal
import           System.Environment (lookupEnv)
import           System.FilePath (searchPathSeparator)
import           System.Process.Run

-- | Launch a GHCi IDE for the given local project targets with the
-- given options and configure it with the load paths and extensions
-- of those targets.
ide
    :: (HasConfig r, HasBuildConfig r, HasTerminal r, HasLogLevel r, MonadMask m, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadBaseControl IO m, HasHttpManager r)
    => [Text] -- ^ Targets.
    -> [String] -- ^ GHC options.
    -> m ()
ide targets useropts = do
    let boptsCli = defaultBuildOptsCLI
            { boptsCLITargets = targets
            , boptsCLIBuildSubset = BSOnlyDependencies
            }
    (_realTargets,_,pkgs) <- ghciSetup (ideGhciOpts boptsCli)
    pwd <- getCurrentDir
    (pkgopts,_srcfiles) <-
        liftM mconcat $ forM pkgs $ getPackageOptsAndTargetFiles pwd
    localdb <- packageDatabaseLocal
    depsdb <- packageDatabaseDeps
    mpath <- liftIO $ lookupEnv "PATH"
    bindirs <- extraBinDirs `ap` return True {- include local bin -}
    let pkgdbs =
            ["--package-db=" <> toFilePathNoTrailingSep depsdb <> [searchPathSeparator] <> toFilePathNoTrailingSep localdb]
        paths =
            [ "--ide-backend-tools-path=" <>
              intercalate [searchPathSeparator] (map toFilePath bindirs) <>
              maybe "" (searchPathSeparator :) mpath]
        args =
            ["--verbose"] <> ["--include=" <> includeDirs pkgopts] <>
            ["--local-work-dir=" ++ toFilePathNoTrailingSep pwd] <>
            map ("--ghc-option=" ++) useropts <>
            paths <>
            pkgopts <>
            pkgdbs
    menv <- getMinimalEnvOverride
    Platform _ os <- asks getPlatform
    when
        (os == OSX)
        (catch (callProcess (Cmd (Just pwd) "stty" menv ["cbreak", "-imaxbel"]))
               (\(_ :: ProcessExitedUnsuccessfully) -> return ()))
    callProcess (Cmd (Just pwd) "stack-ide" menv args)
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
    let autogen = autogenDir dist
    paths_foo <-
        liftM
            (autogen </>)
            (parseRelFile
                 ("Paths_" ++ packageNameString (ghciPkgName pkg) ++ ".hs"))
    paths_foo_exists <- doesFileExist paths_foo
    let ghcOptions bio =
            bioOneWordOpts bio ++
            bioOpts bio ++
            bioPackageFlags bio ++
            maybe [] (\cabalMacros -> ["-optP-include", "-optP" <> toFilePath cabalMacros]) (bioCabalMacros bio)
    return
        ( ("--dist-dir=" <> toFilePathNoTrailingSep dist) :
          map ("--ghc-option=" ++) (concatMap (ghcOptions . snd) (ghciPkgOpts pkg))
        , mapMaybe
              (fmap toFilePath . stripDir pwd)
              (S.toList (ghciPkgCFiles pkg) <> S.toList (ghciPkgModFiles pkg) <>
               [paths_foo | paths_foo_exists]))

ideGhciOpts :: BuildOptsCLI -> GhciOpts
ideGhciOpts boptsCli = GhciOpts
    { ghciNoBuild = False
    , ghciArgs = []
    , ghciGhcCommand = Nothing
    , ghciNoLoadModules = False
    , ghciAdditionalPackages = []
    , ghciMainIs = Nothing
    , ghciLoadLocalDeps = False
    , ghciSkipIntermediate = False
    , ghciHidePackages = True
    , ghciBuildOptsCLI = boptsCli
    }
