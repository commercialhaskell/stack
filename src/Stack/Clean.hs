{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Stack.Clean
Description : Types and functions related to Stack's @clean@ and @purge@ commands.
License     : BSD-3-Clause

Types and functions related to Stack's @clean@ and @purge@ commands.
-}

module Stack.Clean
  ( CleanOpts (..)
  , CleanCommand (..)
  , cleanCmd
  , clean
  ) where

import           Data.List ( (\\) )
import qualified Data.Map.Strict as Map
import           Path.IO ( ignoringAbsence, removeDirRecur )
import           Stack.Config ( withBuildConfig )
import           Stack.Constants.Config ( rootDistDirFromDir, workDirFromDir )
import           Stack.Prelude
import           Stack.Runners ( ShouldReexec (..), withConfig )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..), getWorkDir )
import           Stack.Types.Config ( Config )
import           Stack.Types.Runner ( Runner )
import           Stack.Types.SourceMap ( SMWanted (..), ppRoot )

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "Stack.Clean" module.
data CleanPrettyException
  = NonLocalPackages [PackageName]
  | DeletionFailures [(Path Abs Dir, SomeException)]
  deriving (Show, Typeable)

instance Pretty CleanPrettyException where
  pretty (NonLocalPackages pkgs) =
    "[S-9463]"
    <> line
    <> fillSep
         ( flow "The following are not project packages:"
         : mkNarrativeList (Just Current) False
             (map fromPackageName pkgs :: [StyleDoc])
         )
  pretty (DeletionFailures failures) =
    "[S-6321]"
    <> line
    <> flow "Exception while recursively deleting:"
    <> line
    <> mconcat (map prettyFailure failures)
    <> flow "Perhaps you do not have permission to delete these files or they \
            \are in use?"
   where
    prettyFailure (dir, e) =
         pretty dir
      <> line
      <> string (displayException e)
      <> line

instance Exception CleanPrettyException

-- | Type representing command line options for the @stack clean@ command.
data CleanOpts
  = CleanShallow [PackageName]
    -- ^ Delete the "dist directories" as defined in
    -- 'Stack.Constants.Config.distRelativeDir' for the given project packages.
    -- If no project packages are given, all project packages should be cleaned.
  | CleanFull
    -- ^ Delete all work directories in the project.

-- | Type representing Stack's cleaning commands.
data CleanCommand
  = Clean
  | Purge

-- | Function underlying the @stack clean@ command.
cleanCmd :: CleanOpts -> RIO Runner ()
cleanCmd = withConfig NoReexec . clean

-- | Deletes build artifacts in the current project.
clean :: CleanOpts -> RIO Config ()
clean cleanOpts = do
  toDelete <- withBuildConfig $ dirsToDelete cleanOpts
  logDebug $ "Need to delete: " <> fromString (show (map toFilePath toDelete))
  failures <- catMaybes <$> mapM cleanDir toDelete
  case failures of
    [] -> pure ()
    _  -> prettyThrowIO $ DeletionFailures failures

cleanDir :: Path Abs Dir -> RIO Config (Maybe (Path Abs Dir, SomeException))
cleanDir dir = do
  logDebug $ "Deleting directory: " <> fromString (toFilePath dir)
  liftIO (ignoringAbsence (removeDirRecur dir) >> pure Nothing) `catchAny` \ex ->
    pure $ Just (dir, ex)

dirsToDelete :: CleanOpts -> RIO BuildConfig [Path Abs Dir]
dirsToDelete cleanOpts = do
  packages <- view $ buildConfigL . to (.smWanted.project)
  case cleanOpts of
    CleanShallow [] ->
      -- Filter out packages listed as extra-deps
      mapM (rootDistDirFromDir . ppRoot) $ Map.elems packages
    CleanShallow targets -> do
      let localPkgNames = Map.keys packages
          getPkgDir pkgName' = fmap ppRoot (Map.lookup pkgName' packages)
      case targets \\ localPkgNames of
        [] -> mapM rootDistDirFromDir (mapMaybe getPkgDir targets)
        xs -> prettyThrowM (NonLocalPackages xs)
    CleanFull -> do
      pkgWorkDirs <- mapM (workDirFromDir . ppRoot) $ Map.elems packages
      projectWorkDir <- getWorkDir
      pure (projectWorkDir : pkgWorkDirs)
