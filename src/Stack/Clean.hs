{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Clean a project.
module Stack.Clean
    (clean
    ,CleanOpts(..)
    ,StackCleanException(..)
    ) where

import           Stack.Prelude
import           Data.List ((\\),intercalate)
import qualified Data.Map.Strict as Map
import           Path.IO (ignoringAbsence, removeDirRecur)
import           Stack.Config (getLocalPackages)
import           Stack.Constants.Config (distDirFromDir, workDirFromDir)
import           Stack.Types.PackageName
import           Stack.Types.Config
import           System.Exit (exitFailure)

-- | Deletes build artifacts in the current project.
--
-- Throws 'StackCleanException'.
clean :: HasEnvConfig env => CleanOpts -> RIO env ()
clean cleanOpts = do
    failures <- mapM cleanDir =<< dirsToDelete cleanOpts
    when (or failures) $ liftIO exitFailure
  where
    cleanDir dir =
      liftIO (ignoringAbsence (removeDirRecur dir) >> return False) `catchAny` \ex -> do
        logError $ "Exception while recursively deleting " <> fromString (toFilePath dir) <> "\n" <> displayShow ex
        logError "Perhaps you do not have permission to delete these files or they are in use?"
        return True

dirsToDelete :: HasEnvConfig env => CleanOpts -> RIO env [Path Abs Dir]
dirsToDelete cleanOpts = do
    packages <- getLocalPackages
    case cleanOpts of
        CleanShallow [] ->
            -- Filter out packages listed as extra-deps
            mapM (distDirFromDir . lpvRoot) $ Map.elems $ lpProject packages
        CleanShallow targets -> do
            let localPkgViews = lpProject packages
                localPkgNames = Map.keys localPkgViews
                getPkgDir pkgName = fmap lpvRoot (Map.lookup pkgName localPkgViews)
            case targets \\ localPkgNames of
                [] -> mapM distDirFromDir (mapMaybe getPkgDir targets)
                xs -> throwM (NonLocalPackages xs)
        CleanFull -> do
            pkgWorkDirs <- mapM (workDirFromDir . lpvRoot) $ Map.elems $ lpProject packages
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
