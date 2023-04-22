{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.ParentMap
  ( ParentMap
  ) where

import           Data.Monoid.Map ( MonoidMap (..) )
import           Stack.Prelude
import           Stack.Types.Version ( VersionRange )

type ParentMap =
  MonoidMap PackageName (First Version, [(PackageIdentifier, VersionRange)])
