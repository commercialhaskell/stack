{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Stack.Types.Dependency
  ( DepValue (..)
  , DepType (..)
  , DepLibrary (..)
  , cabalToStackDep
  , cabalExeToStackDep
  , cabalSetupDepsToStackDep
  , libraryDepFromVersionRange
  , isDepTypeLibrary
  , getDepSublib
  , depValueToTarget
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Distribution.PackageDescription as Cabal
import           Distribution.Types.VersionRange ( VersionRange )
import           Stack.Prelude
import           Stack.Types.ComponentUtils
                   ( StackUnqualCompName (..), fromCabalName )
import Stack.Types.SourceMap (Target (TargetComps, TargetAll), PackageType (..))
import Stack.Types.NamedComponent (NamedComponent(..))

-- | The value for a map from dependency name. This contains both the version
-- range and the type of dependency.
data DepValue = DepValue
  { versionRange :: !VersionRange
  , depType :: !DepType
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
  { main :: !Bool
  , subLib :: Set StackUnqualCompName
  }
  deriving (Eq, Show)

getDepSublib :: DepValue -> Maybe (Set StackUnqualCompName)
getDepSublib val = case val.depType of
  AsLibrary libVal -> Just libVal.subLib
  _ -> Nothing

defaultDepLibrary :: DepLibrary
defaultDepLibrary = DepLibrary True mempty

isDepTypeLibrary :: DepType -> Bool
isDepTypeLibrary AsLibrary{} = True
isDepTypeLibrary AsBuildTool = False

cabalToStackDep :: Cabal.Dependency -> DepValue
cabalToStackDep (Cabal.Dependency _ verRange libNameSet) =
  DepValue { versionRange = verRange, depType = AsLibrary depLibrary }
 where
  depLibrary = DepLibrary finalHasMain filteredItems
  (finalHasMain, filteredItems) = foldr' iterator (False, mempty) libNameSet
  iterator LMainLibName (_, newLibNameSet) = (True, newLibNameSet)
  iterator (LSubLibName libName) (hasMain, newLibNameSet) =
    (hasMain, Set.insert (fromCabalName libName) newLibNameSet)

cabalExeToStackDep :: Cabal.ExeDependency -> DepValue
cabalExeToStackDep (Cabal.ExeDependency _ _name verRange) =
  DepValue { versionRange = verRange, depType = AsBuildTool }

cabalSetupDepsToStackDep :: Cabal.SetupBuildInfo -> Map PackageName DepValue
cabalSetupDepsToStackDep setupInfo =
  foldr' inserter mempty (Cabal.setupDepends setupInfo)
 where
  inserter d@(Cabal.Dependency packageName _ _) =
    Map.insert packageName (cabalToStackDep d)

libraryDepFromVersionRange :: VersionRange -> DepValue
libraryDepFromVersionRange range = DepValue
  { versionRange = range
  , depType = AsLibrary defaultDepLibrary
  }

depValueToTarget :: DepValue -> Target
depValueToTarget dv = case dv.depType of
  AsLibrary dlib -> TargetComps (completeSet dlib)
  AsBuildTool -> TargetAll PTDependency
  where
    completeSet dlib = (if dlib.main then Set.insert CLib else id) $ sublibSet dlib
    sublibSet dlib = Set.mapMonotonic (CSubLib . unqualCompToText) dlib.subLib