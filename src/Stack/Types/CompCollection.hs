{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A module providing the type 'CompCollection' and associated helper
-- functions.
--
-- The corresponding Cabal approach uses lists. See, for example, the
-- 'Distribution.Types.PackageDescription.sublibraries',
-- 'Distribution.Types.PackageDescription.foreignLibs',
-- 'Distribution.Types.PackageDescription.executables',
-- 'Distribution.Types.PackageDescription.testSuites', and
-- 'Distribution.Types.PackageDescription.benchmarks' fields.
--
-- Cabal removes all the unbuildable components very early (at the cost of
-- slightly worse error messages).
module Stack.Types.CompCollection
  ( CompCollection
  , getBuildableSet
  , getBuildableSetText
  , getBuildableListText
  , getBuildableListAs
  , foldAndMakeCollection
  , hasBuildableComponent
  , collectionLookup
  , collectionKeyValueList
  , collectionMember
  , foldComponentToAnotherCollection
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import           Data.Foldable ( Foldable (..) )
import           Stack.Prelude
import           Stack.Types.Component
                   ( HasBuildInfo, HasName, StackBuildInfo (..)
                   , StackUnqualCompName (..)
                   )

-- | A type representing collections of components, distinguishing buildable
-- components and non-buildable components.
data CompCollection component = CompCollection
  { buildableOnes :: {-# UNPACK #-} !(InnerCollection component)
  , unbuildableOnes :: Set StackUnqualCompName
    -- ^ The field is lazy beacause it should only serve when users explicitely
    -- require unbuildable components to be built. The field allows for
    -- intelligible error messages.
  }
  deriving (Show)

instance Semigroup (CompCollection component) where
  a <> b = CompCollection
    { buildableOnes = buildableOnes a <> buildableOnes b
    , unbuildableOnes = unbuildableOnes a <> unbuildableOnes b
    }

instance Monoid (CompCollection component) where
  mempty = CompCollection
    { buildableOnes = mempty
    , unbuildableOnes = mempty
    }

instance Foldable CompCollection where
  foldMap fn collection = foldMap fn (asNameMap $ buildableOnes collection)
  foldr' fn c collection = HM.foldr' fn c (asNameMap $ buildableOnes collection)
  null = HM.null . asNameMap . buildableOnes

-- | A type representing a collection of components, including a cache of
-- the components' names.
data InnerCollection component = InnerCollection
  { asNameMap :: !(HashMap StackUnqualCompName component)
  , asNameSet :: !(Set StackUnqualCompName)
    -- ^ Strictly, this field is redundant. It is provided as a cache for speed.
    -- It is assumed, but not checked, that 'asNameSet' is always equal to
    -- @keySet . asNameMap@. It takes O(n) to compute it from 'asNameMap'.
    -- 'addComponent' should be used to add new members to the collection.
  }
  deriving (Show)

instance Semigroup (InnerCollection component) where
  a <> b = InnerCollection
    { asNameMap = asNameMap a <> asNameMap b
    , asNameSet = asNameSet a <> asNameSet b
    }

instance Monoid (InnerCollection component) where
  mempty = InnerCollection
    { asNameMap = mempty
    , asNameSet = mempty
    }

-- | A function to add a component to a collection of components. Ensures that
-- both 'asNameMap' and 'asNameSet' are updated consistently.
addComponent ::
     HasName component
  => component
     -- ^ Component to add.
  -> InnerCollection component
     -- ^ Existing collection of components.
  -> InnerCollection component
addComponent componentV collection =
  let nameV = componentV.name
  in  collection
        { asNameMap=HM.insert nameV componentV (asNameMap collection)
        , asNameSet=Set.insert nameV (asNameSet collection)
        }

-- | For the given function and foldable data structure of components of type
-- @compA@, iterates on the elements of that structure and maps each element to
-- a component of type @compB@ while building a 'CompCollection'.
foldAndMakeCollection ::
     (HasBuildInfo compB, HasName compB, Foldable sourceCollection)
  => (compA -> compB)
     -- ^ Function to apply to each element in the data struture.
  -> sourceCollection compA
     -- ^ Given foldable data structure of components of type @compA@.
  -> CompCollection compB
foldAndMakeCollection mapFn = foldl' compIterator mempty
 where
  compIterator existingCollection component =
    compCreator existingCollection (mapFn component)
  compCreator existingCollection component
    | component.buildInfo.sbiBuildable = existingCollection
        { buildableOnes =
            addComponent component (buildableOnes existingCollection)
        }
    | otherwise = existingCollection
        { unbuildableOnes =
            Set.insert component.name (unbuildableOnes existingCollection)
        }

-- | Get the names of the buildable components in the given collection, as a
-- 'Set' of 'StackUnqualCompName'.
getBuildableSet :: CompCollection component -> Set StackUnqualCompName
getBuildableSet = asNameSet . buildableOnes

-- | Get the names of the buildable components in the given collection, as a
-- 'Set' of 'Text'.
getBuildableSetText :: CompCollection component -> Set Text
getBuildableSetText = Set.mapMonotonic unqualCompToText . getBuildableSet

-- | Get the names of the buildable components in the given collection, as a
-- list of 'Text.
getBuildableListText :: CompCollection component -> [Text]
getBuildableListText = getBuildableListAs unqualCompToText

-- | Apply the given function to the names of the buildable components in the
-- given collection, yielding a list.
getBuildableListAs ::
     (StackUnqualCompName -> something)
     -- ^ Function to apply to buildable components.
  -> CompCollection component
     -- ^ Collection of components.
  -> [something]
getBuildableListAs fn = Set.foldr' (\v l -> fn v:l) [] . getBuildableSet

-- | Yields 'True' if, and only if, the given collection includes at least one
-- buildable component.
hasBuildableComponent :: CompCollection component -> Bool
hasBuildableComponent = not . null . getBuildableSet

-- | For the given name of a buildable component and the given collection of
-- components, yields 'Just' @component@ if the collection includes a buildable
-- component of that name, and 'Nothing' otherwise.
collectionLookup ::
     Text
     -- ^ Name of the buildable component.
  -> CompCollection component
     -- ^ Collection of components.
  -> Maybe component
collectionLookup needle haystack =
  HM.lookup (StackUnqualCompName needle) (asNameMap $ buildableOnes haystack)

-- | For a given collection of components, yields a list of pairs for buildable
-- components of the name of the component and the component.
collectionKeyValueList :: CompCollection component -> [(Text, component)]
collectionKeyValueList haystack =
      (\(StackUnqualCompName k, !v) -> (k, v))
  <$> HM.toList (asNameMap $ buildableOnes haystack)

-- | Yields 'True' if, and only if, the given collection of components includes
-- a buildable component with the given name.
collectionMember ::
     Text
     -- ^ Name of the buildable component.
  -> CompCollection component
     -- ^ Collection of components.
  -> Bool
collectionMember needle haystack = isJust $ collectionLookup needle haystack

-- | Reduce the buildable components of the given collection of components by
-- applying the given binary operator to all buildable components, using the
-- given starting value (typically the right-identity of the operator).
foldComponentToAnotherCollection ::
     (Monad m)
  => CompCollection component
     -- ^ Collection of components.
  -> (component -> m (t b) -> m (t b))
     -- ^ Binary operator.
  -> m (t b)
     -- ^ Starting value.
  -> m (t b)
foldComponentToAnotherCollection collection fn initialValue =
  HM.foldr' fn initialValue (asNameMap $ buildableOnes collection)
