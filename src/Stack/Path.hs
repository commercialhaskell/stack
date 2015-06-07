{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Implementation of the "path" subcommand
module Stack.Path
  ( pathString
  , PathArg(..)
  , pathGhc
  , pathLog
  , pathPackageDb
  ) where

import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.List as List
import Data.Typeable (Typeable)
import Path
import System.FilePath

import Stack.Types

data PathArg
  = PathGhc
  | PathLog
  | PathPackageDb
  deriving (Show, Eq, Ord)

data NotYetImplemented = NotYetImplemented String
  deriving (Show, Typeable)
instance Exception NotYetImplemented

-- | Finds the selected PathArg and displays it as a String.
pathString :: (MonadReader env m, HasBuildConfig env, MonadThrow m)
  => PathArg -> m String
pathString PathGhc = liftM show pathGhc
pathString PathLog = liftM show pathLog
pathString PathPackageDb = liftM process pathPackageDb where
  -- Turns the list of package-db directories into a String
  -- suitable for consumption by GHC_PACKAGE_PATH.
  process :: [Path Abs Dir] -> String
  process = List.intercalate [searchPathSeparator] . map show

-- | The path to the ghc
-- that will be used for the current project.
pathGhc :: (MonadReader env m, HasBuildConfig env, MonadThrow m)
  => m (Path Abs File)
pathGhc = throwM $ NotYetImplemented "Stack.Path.pathGhc https://github.com/fpco/stack/issues/95"

-- (Note: it's not actually as simple as just one log dir.)
-- | The path to the log file directory
-- that will be used for the current project.
pathLog :: (MonadReader env m, HasBuildConfig env, MonadThrow m)
  => m (Path Abs Dir)
pathLog = throwM $ NotYetImplemented "Stack.Path.pathLog https://github.com/fpco/stack/issues/95"

-- | The list of package-db directories
-- that will be used for the current project.
-- The earlier databases in the list take precedence over the later ones.
pathPackageDb :: (MonadReader env m, HasBuildConfig env, MonadThrow m)
  => m [Path Abs Dir]
pathPackageDb = throwM $ NotYetImplemented "Stack.Path.pathPackageDb https://github.com/fpco/stack/issues/95"
