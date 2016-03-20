{-# LANGUAGE DeriveDataTypeable #-}

-- | Clean a project.
module Stack.Clean
    (clean
    ,CleanOpts(..)
    ,StackCleanException(..)
    ) where

import           Control.Exception (Exception)
import           Control.Monad.Catch (MonadCatch, MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger (MonadLogger)
import           Control.Monad.Reader (MonadReader)
import           Data.Foldable (forM_)
import           Data.List ((\\),intercalate)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Typeable (Typeable)
import           Path (Path, Abs, Dir)
import           Path.IO (ignoringAbsence, removeDirRecur)
import           Stack.Build.Source (getLocalPackageViews)
import           Stack.Build.Target (LocalPackageView(..))
import           Stack.Constants (distDirFromDir, workDirFromDir)
import           Stack.Types (HasEnvConfig, PackageName, configProjectWorkDir)

-- | Deletes build artifacts in the current project.
--
-- Throws 'StackCleanException'.
clean
    :: (MonadCatch m, MonadIO m, MonadReader env m, HasEnvConfig env, MonadLogger m)
    => CleanOpts
    -> m ()
clean cleanOpts = do
    dirs <- dirsToDelete cleanOpts
    forM_ dirs (ignoringAbsence . removeDirRecur)

dirsToDelete
    :: (MonadThrow m, MonadIO m, MonadReader env m, HasEnvConfig env, MonadLogger m)
    => CleanOpts
    -> m [Path Abs Dir]
dirsToDelete cleanOpts = do
    localPkgViews <- getLocalPackageViews
    let localPkgNames = Map.keys localPkgViews
        getPkgDir pkgName = fmap (lpvRoot . fst) (Map.lookup pkgName localPkgViews)
    case cleanOpts of
        CleanShallow targets -> do
            pkgsToClean <-
                case targets \\ localPkgNames of
                    [] -> return (if null targets then localPkgNames else targets)
                    xs -> throwM (NonLocalPackages xs)
            mapM distDirFromDir (mapMaybe getPkgDir pkgsToClean)
        CleanFull -> do
            pkgWorkDirs <- mapM workDirFromDir (mapMaybe getPkgDir localPkgNames)
            projectWorkDir <- configProjectWorkDir
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
