{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Clean a project.
module Stack.Clean
    (clean
    ,CleanOpts(..)
    ,CleanCommand(..)
    ,StackCleanException(..)
    ) where

import           Stack.Prelude
import           Data.List ((\\),intercalate)
import qualified Data.Map.Strict as Map
import           Path.IO (ignoringAbsence, removeDirRecur)
import           Stack.Config (withBuildConfig)
import           Stack.Constants.Config (rootDistDirFromDir, workDirFromDir)
import           Stack.Types.Config
import           Stack.Types.SourceMap

-- | Deletes build artifacts in the current project.
--
-- Throws 'StackCleanException'.
clean :: CleanOpts -> RIO Config ()
clean cleanOpts = do
    toDelete <- withBuildConfig $ dirsToDelete cleanOpts
    logDebug $ "Need to delete: " <> fromString (show (map toFilePath toDelete))
    failures <- mapM cleanDir toDelete
    when (or failures) exitFailure
  where
    cleanDir dir = do
      logDebug $ "Deleting directory: " <> fromString (toFilePath dir)
      liftIO (ignoringAbsence (removeDirRecur dir) >> pure False) `catchAny` \ex -> do
        logError $ "Exception while recursively deleting " <> fromString (toFilePath dir) <> "\n" <> displayShow ex
        logError "Perhaps you do not have permission to delete these files or they are in use?"
        pure True

dirsToDelete :: CleanOpts -> RIO BuildConfig [Path Abs Dir]
dirsToDelete cleanOpts = do
    packages <- view $ buildConfigL.to (smwProject . bcSMWanted)
    case cleanOpts of
        CleanShallow [] ->
            -- Filter out packages listed as extra-deps
            mapM (rootDistDirFromDir . ppRoot) $ Map.elems packages
        CleanShallow targets -> do
            let localPkgNames = Map.keys packages
                getPkgDir pkgName' = fmap ppRoot (Map.lookup pkgName' packages)
            case targets \\ localPkgNames of
                [] -> mapM rootDistDirFromDir (mapMaybe getPkgDir targets)
                xs -> throwM (NonLocalPackages xs)
        CleanFull -> do
            pkgWorkDirs <- mapM (workDirFromDir . ppRoot) $ Map.elems packages
            projectWorkDir <- getProjectWorkDir
            pure (projectWorkDir : pkgWorkDirs)

-- | Options for @stack clean@.
data CleanOpts
    = CleanShallow [PackageName]
    -- ^ Delete the "dist directories" as defined in 'Stack.Constants.Config.distRelativeDir'
    -- for the given local packages. If no packages are given, all project packages
    -- should be cleaned.
    | CleanFull
    -- ^ Delete all work directories in the project.

-- | Clean commands
data CleanCommand
    = Clean
    | Purge

-- | Exceptions during cleanup.
newtype StackCleanException
    = NonLocalPackages [PackageName]
    deriving (Typeable)

instance Show StackCleanException where
    show (NonLocalPackages pkgs) =
        "The following packages are not part of this project: " ++
        intercalate ", " (map show pkgs)

instance Exception StackCleanException
