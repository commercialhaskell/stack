{-# LANGUAGE ExistentialQuantification #-}

module Stack.Types.Config where

import           Control.Exception
import           Data.List.NonEmpty (NonEmpty)
import           Distribution.Version
import           Data.Text (Text)
import           Data.Yaml (ParseException)
import           Path
import           Stack.Types.BuildPlan (SnapName)
import           {-# SOURCE #-} Stack.Types.Resolver (Resolver, ResolverThat's)

data WhichSolverCmd

data ConfigException
  = ParseConfigFileException (Path Abs File) ParseException
  | ParseCustomSnapshotException Text ParseException
  | ParseResolverException Text
  | NoProjectConfigFound (Path Abs Dir) (Maybe Text)
  | UnexpectedArchiveContents [Path Abs Dir] [Path Abs File]
  | UnableToExtractArchive Text (Path Abs File)
  | BadStackVersionException VersionRange
  | NoMatchingSnapshot WhichSolverCmd (NonEmpty SnapName)
  | forall l. ResolverMismatch WhichSolverCmd (ResolverThat's l) String
  | ResolverPartial WhichSolverCmd Resolver String
  | NoSuchDirectory FilePath
  | ParseGHCVariantException String
  | BadStackRoot (Path Abs Dir)
  | Won'tCreateStackRootInDirectoryOwnedByDifferentUser (Path Abs Dir) (Path Abs Dir) -- ^ @$STACK_ROOT@, parent dir
  | UserDoesn'tOwnDirectory (Path Abs Dir)
  | FailedToCloneRepo String
  | ManualGHCVariantSettingsAreIncompatibleWithSystemGHC
  | NixRequiresSystemGhc
instance Exception ConfigException
