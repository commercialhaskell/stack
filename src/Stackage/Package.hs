{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A package.

module Stackage.Package where

import Data.Data
import Data.HashMap.Strict
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Distribution.Package (Dependency)
import GHC.Generics (Generic)
import Stackage.PackageName (PackageName)
import Stackage.PackageVersion (PackageVersion,VersionRange)

-- | A package's data.
data Package =
  Package {packageName    :: !PackageName
          -- ^ Name of the package.
          ,packageVersion :: !PackageVersion
          -- ^ Version of the package
          ,packageFiles   :: !(Set FilePath)
          -- ^ Files that the package depends on.
          ,packageDeps    :: !(HashMap PackageName VersionRange)
          -- ^ Packages that the package depends on.
          ,packageTools   :: !(Vector Dependency)
          -- ^ A build tool name.
          ,packageAllDeps :: !(Set PackageName)
          -- ^ Original dependencies (not sieved).
          ,packageFlags   :: !(HashMap Text Bool)
          -- ^ Flags used on package.
          }
  deriving (Show,Typeable,Data,Generic)
