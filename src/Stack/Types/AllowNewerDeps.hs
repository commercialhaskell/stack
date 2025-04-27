{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Types.AllowNewerDeps
License     : BSD-3-Clause
-}

module Stack.Types.AllowNewerDeps
  ( AllowNewerDeps (..)
  ) where

import           Data.Aeson.Types ( FromJSON (..) )
import qualified Distribution.PackageDescription as C
import           Generics.Deriving.Monoid ( mappenddefault, memptydefault )
import           Stack.Prelude

newtype AllowNewerDeps
  = AllowNewerDeps [PackageName]
  deriving (Eq, Generic, Ord, Read, Show)

instance Semigroup AllowNewerDeps where
  (<>) = mappenddefault

instance Monoid AllowNewerDeps where
  mappend = (<>)
  mempty = memptydefault

instance FromJSON AllowNewerDeps where
  parseJSON = fmap (AllowNewerDeps . fmap C.mkPackageName) . parseJSON
