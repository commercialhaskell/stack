{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Clean a project.
module Stack.Clean
    (clean
    ,CleanOpts(..)
    ,StackCleanException(..)
    ) where

import           Control.Exception (Exception)
import           Control.Monad.Catch (throwM)
import           Data.Foldable (forM_)
import           Data.List ((\\),intercalate)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Typeable (Typeable)
import           Path (Path, Abs, Dir)
import           Path.IO (ignoringAbsence, removeDirRecur)
import           Stack.Build.Source (getLocalPackageViews)
import           Stack.Build.Target (LocalPackageView(..))
import           Stack.Config (getLocalPackages)
import           Stack.Constants (distDirFromDir, workDirFromDir)
import           Stack.Types.PackageName
import           Stack.Types.Config
import           Stack.Types.StackT

-- | Deletes build artifacts in the current project.
--
-- Throws 'StackCleanException'.
clean
    :: (StackM env m, HasEnvConfig env)
    => CleanOpts
    -> m ()
clean cleanOpts = do
    dirs <- dirsToDelete cleanOpts
    forM_ dirs (ignoringAbsence . removeDirRecur)

dirsToDelete
    :: (StackM env m, HasEnvConfig env)
    => CleanOpts
    -> m [Path Abs Dir]
dirsToDelete cleanOpts = do
    packages <- getLocalPackages
    let localPkgDirs = Map.keys packages
    case cleanOpts of
        CleanShallow [] -> do
            mapM distDirFromDir localPkgDirs
        CleanShallow targets -> do
            localPkgViews <- getLocalPackageViews
            let localPkgNames = Map.keys localPkgViews
                getPkgDir pkgName = fmap (lpvRoot . fst) (Map.lookup pkgName localPkgViews)
            case targets \\ localPkgNames of
                [] -> mapM distDirFromDir (mapMaybe getPkgDir targets)
                xs -> throwM (NonLocalPackages xs)
        CleanFull -> do
            pkgWorkDirs <- mapM workDirFromDir localPkgDirs
            projectWorkDir <- getProjectWorkDir
            return (projectWorkDir : pkgWorkDirs)

-- | Options for @stack clean@.
data CleanOpts
    = CleanShallow [PackageName]
    -- ^ Delete the "dist directories" as defined in 'Stack.Constants.distRelativeDir'
    -- for the given local packages. If no packages are given, all project packages
    -- should be cleaned.
    | CleanFull
    -- ^ Delete all work directories in the project.

-- | Exceptions during cleanup.
newtype StackCleanException
    = NonLocalPackages [PackageName]
    deriving (Typeable)

instance Show StackCleanException where
    show (NonLocalPackages pkgs) =
        "The following packages are not part of this project: " ++
        intercalate ", " (map show pkgs)

instance Exception StackCleanException
