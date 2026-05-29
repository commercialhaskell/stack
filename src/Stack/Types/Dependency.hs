{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Stack.Types.Dependency
License     : BSD-3-Clause
-}

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
import           Stack.Types.NamedComponent ( NamedComponent(..) )
import           Stack.Types.SourceMap ( PackageType (..), Target (..) )

-- | The value for a map from dependency name. This contains both the version
-- range and the type of dependency.
data DepValue = DepValue
  { versionRange :: !VersionRange
  , depType :: !DepType
  }
  deriving Show

-- | Is this package being used as a library, or just as a build tool? If the
-- former, we need to ensure that a library actually exists. See
-- <https://github.com/commercialhaskell/stack/issues/2195>
data DepType
  = AsLibrary !DepLibrary
    -- ^ Dependency is used as a library.
  | AsBuildTool
    -- ^ Dependency is used only to provide a build tool.
  deriving (Eq, Show)

-- | Type repesenting dependency packages used as a library.
data DepLibrary = DepLibrary
  { main :: !Bool
    -- ^ Is the dependency on a main (unnamed) library component?
  , subLib :: Set StackUnqualCompName
    -- ^ A set (which may be empty) of dependencies on sub-library components.
  }
  deriving (Eq, Show)

-- | A function to yield the set (which may be empty) of dependencies on
-- sub-library components. Yields 'Nothing' if the dependency is used only to
-- provide a build tool.
getDepSublib :: DepValue -> Maybe (Set StackUnqualCompName)
getDepSublib val = case val.depType of
  AsLibrary libVal -> Just libVal.subLib
  _ -> Nothing

-- | Represents a dependency only on a main (unnamed) library component.
defaultDepLibrary :: DepLibrary
defaultDepLibrary = DepLibrary True mempty

-- | Test whether the dependency is being used as a library.
isDepTypeLibrary :: DepType -> Bool
isDepTypeLibrary AsLibrary{} = True
isDepTypeLibrary AsBuildTool = False

-- | Given a 'Cabal.Dependency', yield the Stack equivalent.
cabalToStackDep :: Cabal.Dependency -> DepValue
cabalToStackDep (Cabal.Dependency _ verRange libNameSet) =
  DepValue { versionRange = verRange, depType = AsLibrary depLibrary }
 where
  depLibrary = DepLibrary finalHasMain filteredItems

  (finalHasMain, filteredItems) = foldr' iterator (False, mempty) libNameSet
   where
    iterator LMainLibName (_, newLibNameSet) = (True, newLibNameSet)
    iterator (LSubLibName libName) (hasMain, newLibNameSet) =
      (hasMain, Set.insert (fromCabalName libName) newLibNameSet)

-- | Given an 'Cabal.ExeDependency', yield the Stack equivalent.
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
  completeSet dlib =
    (if dlib.main then Set.insert CLib else id) $ sublibSet dlib
  sublibSet dlib = Set.mapMonotonic CSubLib dlib.subLib
