{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Types.ParentMap
Description : Module exporting the 'ParentMap' type synonym.
License     : BSD-3-Clause

Module exporting the 'ParentMap' type synonym.
-}

module Stack.Types.ParentMap
  ( ParentMap
  ) where

import           Data.Monoid.Map ( MonoidMap (..) )
import           Stack.Prelude
import           Stack.Types.Version ( VersionRange )

-- | Type synonym representing dictionaries of package names, and a list of
-- pairs of the identifier of a package depending on the package and the
-- version range specified for the dependency by that package.
type ParentMap =
  MonoidMap PackageName [(PackageIdentifier, VersionRange)]
