{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.Dependency
  ( DepValue (..)
  , DepType (..)
  ) where

import           Distribution.Types.VersionRange ( VersionRange )
import           Stack.Prelude
import           Stack.Types.Version ( intersectVersionRanges )


-- | The value for a map from dependency name. This contains both the version
-- range and the type of dependency, and provides a semigroup instance.
data DepValue = DepValue
  { dvVersionRange :: !VersionRange
  , dvType :: !DepType
  }
  deriving (Show, Typeable)

instance Semigroup DepValue where
  DepValue a x <> DepValue b y = DepValue (intersectVersionRanges a b) (x <> y)

-- | Is this package being used as a library, or just as a build tool? If the
-- former, we need to ensure that a library actually exists. See
-- <https://github.com/commercialhaskell/stack/issues/2195>
data DepType
  = AsLibrary
  | AsBuildTool
  deriving (Eq, Show)

instance Semigroup DepType where
  AsLibrary <> _ = AsLibrary
  AsBuildTool <> x = x
