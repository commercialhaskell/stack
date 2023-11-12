{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A package has collections of the types exported by "Stack.Types.Component".
-- In Cabal, the components are grouped as a list in a @PackageDescription@;
-- this is Stack's counterpart. See for instance the
-- [foreign libraries field](https://hackage.haskell.org/package/Cabal-syntax/docs/Distribution-Types-PackageDescription.html#t:GenericPackageDescription)
-- in Cabal. Cabal removes all the unbuildable components very early (at the
-- cost of slightly worse error messages) and it historically used a list for
-- components. For , as well as set listed for backward compatbility at least,
-- hence this structure.
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
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import           Data.Foldable ( Foldable (..) )
import           Stack.Prelude
import           Stack.Types.Component
                   ( HasBuildInfo, HasName, StackBuildInfo (..)
                   , StackUnqualCompName (..), unqualCompToText
                   )

-- | This is a collection discriminating buildable components and non-buildable
-- ones.
data CompCollection component = CompCollection
  { buildableOnes :: {-# UNPACK #-} !(InnerCollection component)
  , unbuildableOnes :: Set StackUnqualCompName
    -- ^ Note that the field is lazy beacause it should only serve when users
    -- explicitely require unbuildable components to be built (and this field
    -- ensure intelligible error messages).
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

-- | This is a naive collection which trades memory consumption for speed, the
-- name set is strictly redundant but it takes O(n) to compute it from the map's
-- keys, hence the redundancy. The assumption is 'asNameSet' is always equal to
-- @keySet . asNameMap@, but faster because cached. But this assumption is not
-- checked anywhere, it's simply built into the 'addComponent' function, which
-- should always be used rather than record's fields for inserting new elements.
data InnerCollection component = InnerCollection
  { asNameMap :: !(HashMap StackUnqualCompName component)
  , asNameSet :: !(Set StackUnqualCompName)
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

instance Foldable CompCollection where
  foldMap fn collection = foldMap fn (asNameMap $ buildableOnes collection)
  foldr' fn c collection = HM.foldr' fn c (asNameMap $ buildableOnes collection)
  null = HM.null . asNameMap . buildableOnes

-- |
--
-- >>> :set -XOverloadedStrings
-- >>> import Stack.Types.Component (StackUnqualCompName(StackUnqualCompName))
-- >>> data TestComp = TestComp { name :: StackUnqualCompName } deriving (Show)
-- >>> let ok = TestComp (StackUnqualCompName "some-comp-name")
-- >>> addComponent ok mempty
-- InnerCollection {asNameMap = fromList [(StackUnqualCompName "some-comp-name",TestComp {name = StackUnqualCompName "some-comp-name"})], asNameSet = fromList [StackUnqualCompName "some-comp-name"]}
addComponent ::
     HasName component
  => component
  -> InnerCollection component
  -> InnerCollection component
addComponent componentV collection =
  let nameV = componentV.name
  in  collection
        { asNameMap=HM.insert nameV componentV (asNameMap collection)
        , asNameSet=Set.insert nameV (asNameSet collection)
        }

-- | Iterates on a collection of @compA@ and map each element to @compB@ while
-- building the corresponding @CompCollection@ out of it.
foldAndMakeCollection ::
     (HasBuildInfo compB, HasName compB, Foldable sourceCollection)
  => (compA -> compB)
  -> sourceCollection compA
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

-- | Get the name set of buildable components from this collection.
getBuildableSet :: CompCollection component -> Set StackUnqualCompName
getBuildableSet = asNameSet . buildableOnes

getBuildableSetText :: CompCollection component -> Set Text
getBuildableSetText = Set.mapMonotonic unqualCompToText . getBuildableSet

getBuildableListText :: CompCollection component -> [] Text
getBuildableListText = getBuildableListAs unqualCompToText

getBuildableListAs ::
     (StackUnqualCompName -> something)
  -> CompCollection component
  -> [] something
getBuildableListAs fn = Set.foldr' (\v l -> fn v:l) [] . getBuildableSet

hasBuildableComponent :: CompCollection component -> Bool
hasBuildableComponent = not . null . getBuildableSet

collectionLookup :: Text -> CompCollection component -> Maybe component
collectionLookup needle haystack =
  HM.lookup (StackUnqualCompName needle) (asNameMap $ buildableOnes haystack)

collectionKeyValueList :: CompCollection component -> [(Text, component)]
collectionKeyValueList haystack =
      (\(StackUnqualCompName k, !v) -> (k, v))
  <$> HM.toList (asNameMap $ buildableOnes haystack)

collectionMember :: Text -> CompCollection component -> Bool
collectionMember needle haystack = isJust $ collectionLookup needle haystack
