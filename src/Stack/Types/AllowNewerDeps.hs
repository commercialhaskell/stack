{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Types.AllowNewerDeps
License     : BSD-3-Clause
-}

module Stack.Types.AllowNewerDeps
  ( AllowNewerDeps (..)
  ) where

import           Data.Aeson.Types ( FromJSON (..) )
import           Distribution.PackageDescription ( mkPackageName )
import           Stack.Prelude

-- | A type representing lists of packages for which Stack should ignore lower
-- and upper version bounds in its Cabal file.
newtype AllowNewerDeps
  = AllowNewerDeps [PackageName]
  deriving (Eq, Generic, Monoid, Ord, Read, Semigroup, Show)

instance FromJSON AllowNewerDeps where
  parseJSON = fmap (AllowNewerDeps . fmap mkPackageName) . parseJSON
