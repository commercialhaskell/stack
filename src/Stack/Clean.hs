{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Clean a project.
module Stack.Clean
  ( clean
  , CleanOpts (..)
  , CleanCommand (..)
  ) where

import           Data.List ( (\\), intercalate )
import qualified Data.Map.Strict as Map
import           Path.IO ( ignoringAbsence, removeDirRecur )
import           Stack.Config ( withBuildConfig )
import           Stack.Constants.Config ( rootDistDirFromDir, workDirFromDir )
import           Stack.Prelude
import           Stack.Types.Config
                   ( BuildConfig (..), Config, HasBuildConfig (..)
                   , getProjectWorkDir, ppRoot
                   )
import           Stack.Types.SourceMap ( SMWanted (..) )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Clean" module.
data CleanException
  = NonLocalPackages [PackageName]
  | DeletionFailures [(Path Abs Dir, SomeException)]
  deriving (Show, Typeable)

instance Exception CleanException where
  displayException (NonLocalPackages pkgs) = concat
    [ "Error: [S-9463]\n"
    , "The following packages are not part of this project: "
    , intercalate ", " (map show pkgs)
    ]
  displayException (DeletionFailures failures) = concat
    [ "Error: [S-6321]\n"
    , "Exception while recursively deleting:\n"
    , concatMap (\(dir, e) ->
        toFilePath dir <> "\n" <> displayException e <> "\n") failures
    , "Perhaps you do not have permission to delete these files or they are in \
      \use?"
    ]

-- | Deletes build artifacts in the current project.
clean :: CleanOpts -> RIO Config ()
clean cleanOpts = do
  toDelete <- withBuildConfig $ dirsToDelete cleanOpts
  logDebug $ "Need to delete: " <> fromString (show (map toFilePath toDelete))
  failures <- catMaybes <$> mapM cleanDir toDelete
  case failures of
    [] -> pure ()
    _  -> throwIO $ DeletionFailures failures

cleanDir :: Path Abs Dir -> RIO Config (Maybe (Path Abs Dir, SomeException))
cleanDir dir = do
  logDebug $ "Deleting directory: " <> fromString (toFilePath dir)
  liftIO (ignoringAbsence (removeDirRecur dir) >> pure Nothing) `catchAny` \ex ->
    pure $ Just (dir, ex)

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
    -- ^ Delete the "dist directories" as defined in
    -- 'Stack.Constants.Config.distRelativeDir' for the given local packages. If
    -- no packages are given, all project packages should be cleaned.
  | CleanFull
    -- ^ Delete all work directories in the project.

-- | Clean commands
data CleanCommand
    = Clean
    | Purge
