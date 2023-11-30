{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.Dependency
  ( DepValue (..)
  , DepType (..)
  , cabalToStackDep
  , cabalExeToStackDep
  , cabalSetupDepsToStackDep
  , libraryDepFromVersionRange
  , isDepTypeLibrary
  , getDepSublib
  ) where

import           Data.Foldable ( foldr' )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Distribution.PackageDescription as Cabal
import           Distribution.Types.VersionRange ( VersionRange )
import           Stack.Prelude
import           Stack.Types.ComponentUtils
                   ( StackUnqualCompName (..), fromCabalName )

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
  = AsLibrary !DepLibrary
  | AsBuildTool
  deriving (Eq, Show)

data DepLibrary = DepLibrary
  { dlMain :: !Bool
  , dlSublib :: Set StackUnqualCompName
  }
  deriving (Eq, Show)

getDepSublib :: DepValue -> Maybe (Set StackUnqualCompName)
getDepSublib val = case dvType val of
  AsLibrary libVal -> Just $ dlSublib libVal
  _ -> Nothing

defaultDepLibrary :: DepLibrary
defaultDepLibrary = DepLibrary True mempty

isDepTypeLibrary :: DepType -> Bool
isDepTypeLibrary AsLibrary{} = True
isDepTypeLibrary AsBuildTool = False

cabalToStackDep :: Cabal.Dependency -> DepValue
cabalToStackDep (Cabal.Dependency _ verRange libNameSet) =
  DepValue{dvVersionRange = verRange, dvType = AsLibrary depLibrary}
 where
  depLibrary = DepLibrary finalHasMain filteredItems
  (finalHasMain, filteredItems) = foldr' iterator (False, mempty) libNameSet
  iterator LMainLibName (_, newLibNameSet) = (True, newLibNameSet)
  iterator (LSubLibName libName) (hasMain, newLibNameSet) =
    (hasMain, Set.insert (fromCabalName libName) newLibNameSet)

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
  , dvType = AsLibrary defaultDepLibrary
  }
