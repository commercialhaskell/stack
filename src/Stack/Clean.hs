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
  , CleanDepth (..)
  , CleanCommand (..)
  , cleanCmd
  , clean
  ) where

import           Control.Monad.Extra ( concatMapM )
import           Data.List ( (\\) )
import qualified Data.Map.Strict as Map
import           Path ( (</>), isProperPrefixOf )
import           Path.IO ( ignoringAbsence, listDirRecur, removeDirRecur )
import           Stack.Config ( withBuildConfig )
import           Stack.Constants.Config
                   ( distRelativeDir, rootDistDirFromDir, workDirFromDir )
import           Stack.Prelude
import           Stack.Runners
                   ( ShouldReexec (..), withConfig, withDefaultEnvConfig )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..), getWorkDir )
import           Stack.Types.Config ( Config )
import           Stack.Types.EnvConfig ( EnvConfig )
import           Stack.Types.Runner ( Runner )
import           Stack.Types.SourceMap ( ProjectPackage, SMWanted (..), ppRoot )

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
data CleanOpts = CleanOpts
  { depth    :: !CleanDepth
  , omitThis :: !Bool
  }

-- | Type representing depths of cleaning for the @stack clean@ command.
data CleanDepth
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
  toDelete <- if cleanOpts.omitThis
    then
      withDefaultEnvConfig $ dirsToDeleteGivenConfig cleanOpts.depth
    else
      withBuildConfig $ dirsToDeleteSimple cleanOpts.depth
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

dirsToDeleteSimple :: CleanDepth -> RIO BuildConfig [Path Abs Dir]
dirsToDeleteSimple depth = do
  packages <- view $ buildConfigL . to (.smWanted.project)
  case depth of
    CleanShallow [] -> do
      -- Filter out packages listed as extra-deps
      let pkgNames = Map.elems packages
      mapM (rootDistDirFromDir . ppRoot) pkgNames
    CleanShallow targets -> do
      let localPkgNames = Map.keys packages
          getPkgDir pkgName' = fmap ppRoot (Map.lookup pkgName' packages)
          pkgNames = mapMaybe getPkgDir targets
      case targets \\ localPkgNames of
        [] -> mapM rootDistDirFromDir pkgNames
        xs -> prettyThrowM (NonLocalPackages xs)
    CleanFull -> allWorkDirs $ Map.elems packages

dirsToDeleteGivenConfig :: CleanDepth -> RIO EnvConfig [Path Abs Dir]
dirsToDeleteGivenConfig depth = do
  packages <- view $ buildConfigL . to (.smWanted.project)
  case depth of
    CleanShallow [] -> do
      -- Filter out packages listed as extra-deps
      let pkgNames = Map.elems packages
      concatMapM (unusedRootDistDirsFromDir . ppRoot) pkgNames
    CleanShallow targets -> do
      let localPkgNames = Map.keys packages
          getPkgDir pkgName' = fmap ppRoot (Map.lookup pkgName' packages)
          pkgNames = mapMaybe getPkgDir targets
      case targets \\ localPkgNames of
        [] -> concatMapM unusedRootDistDirsFromDir pkgNames
        xs -> prettyThrowM (NonLocalPackages xs)
    CleanFull -> allWorkDirs $ Map.elems packages

allWorkDirs :: HasBuildConfig env => [ProjectPackage] -> RIO env [Path Abs Dir]
allWorkDirs pps = do
  pkgWorkDirs <- mapM (workDirFromDir . ppRoot) pps
  projectWorkDir <- getWorkDir
  pure (projectWorkDir : pkgWorkDirs)

unusedRootDistDirsFromDir :: Path Abs Dir -> RIO EnvConfig [Path Abs Dir]
unusedRootDistDirsFromDir pkgDir = do
  rootDistDir <- rootDistDirFromDir pkgDir
  omitDir <- fmap (pkgDir </>) distRelativeDir
  allDirsOmittingDirs rootDistDir omitDir

allDirsOmittingDirs ::
     MonadIO m
  => Path Abs Dir
  -> Path Abs Dir
  -> m [Path Abs Dir]
allDirsOmittingDirs topDir subDir = do
  allDirs <- (topDir :) . fst <$> listDirRecur topDir
  let isNotInSubDir dir = not
        ( isProperPrefixOf dir subDir
        || subDir == dir
        || isProperPrefixOf subDir dir
        )
  pure $ filter isNotInSubDir allDirs
