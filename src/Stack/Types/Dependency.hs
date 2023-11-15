{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.Dependency
  ( DepValue (..)
  , DepType (..)
  , cabalToStackDep
  , cabalExeToStackDep
  , cabalSetupDepsToStackDep
  , libraryDepFromVersionRange
  , isDepTypeLibrary
  ) where

import           Data.Foldable ( foldr' )
import qualified Data.Map as Map
import qualified Distribution.PackageDescription as Cabal
import           Distribution.Types.VersionRange ( VersionRange )
import           Stack.Prelude

-- | The value for a map from dependency name. This contains both the version
-- range and the type of dependency.
data DepValue = DepValue
  { dvVersionRange :: !VersionRange
  , dvType :: !DepType
  }
  deriving (Show, Typeable)

-- | Is this package being used as a library, or just as a build tool? If the
-- former, we need to ensure that a library actually exists. See
-- <https://github.com/commercialhaskell/stack/issues/2195>
data DepType
  = AsLibrary
  | AsBuildTool
  deriving (Eq, Show)

isDepTypeLibrary :: DepType -> Bool
isDepTypeLibrary AsLibrary = True
isDepTypeLibrary AsBuildTool = False

cabalToStackDep :: Cabal.Dependency -> DepValue
cabalToStackDep (Cabal.Dependency _ verRange _libNameSet) =
  DepValue{dvVersionRange = verRange, dvType = AsLibrary}

cabalExeToStackDep :: Cabal.ExeDependency -> DepValue
cabalExeToStackDep (Cabal.ExeDependency _ _name verRange) =
  DepValue{dvVersionRange = verRange, dvType = AsBuildTool}

cabalSetupDepsToStackDep :: Cabal.SetupBuildInfo -> Map PackageName DepValue
cabalSetupDepsToStackDep setupInfo =
  foldr' inserter mempty (Cabal.setupDepends setupInfo)
 where
  inserter d@(Cabal.Dependency packageName _ _) =
    Map.insert packageName (cabalToStackDep d)

libraryDepFromVersionRange :: VersionRange -> DepValue
libraryDepFromVersionRange range = DepValue
  { dvVersionRange = range
  , dvType = AsLibrary
  }
