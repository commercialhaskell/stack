-- | Functions for the GHC package database.

module Stackage.GhcPkg where

import Stackage.GhcPkgId
import Stackage.PackageName
import Stackage.PackageVersion

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Maybe
import Prelude hiding (FilePath)
import System.IO

-- | Get all available packages.
getGlobalPackages :: (MonadIO m,MonadThrow m) => m (Map PackageName PackageVersion)
getGlobalPackages = undefined

-- | Get the package of the package database.
getUserDbPath :: IO FilePath
getUserDbPath = undefined

-- | Get the id of the package e.g. @foo-0.0.0-9c293923c0685761dcff6f8c3ad8f8ec@.
findPackageId :: PackageName -> IO (Maybe GhcPkgId)
findPackageId = undefined

-- | Get all current package ids.
getPackageIds :: [PackageName] -> IO (Map PackageName GhcPkgId)
getPackageIds = undefined
