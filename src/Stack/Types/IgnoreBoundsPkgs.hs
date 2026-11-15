{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Types.IgnoreBoundsPkgs
License     : BSD-3-Clause
-}

module Stack.Types.IgnoreBoundsPkgs
  ( IgnoreBoundsPkgs (..)
  ) where

import           Data.Aeson.Types ( FromJSON (..) )
import           Distribution.PackageDescription ( mkPackageName )
import           Stack.Prelude

-- | A type representing lists of packages for which Stack should ignore lower
-- and upper version bounds in its Cabal file.
newtype IgnoreBoundsPkgs
  = IgnoreBoundsPkgs [PackageName]
  deriving (Eq, Generic, Monoid, Ord, Read, Semigroup, Show)

instance FromJSON IgnoreBoundsPkgs where
  parseJSON = fmap (IgnoreBoundsPkgs . fmap mkPackageName) . parseJSON
