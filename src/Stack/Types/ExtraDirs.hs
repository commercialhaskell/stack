{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.ExtraDirs
  ( ExtraDirs (..)
  ) where

import           Generics.Deriving.Monoid ( mappenddefault, memptydefault )
import           Stack.Prelude

data ExtraDirs = ExtraDirs
  { edBins :: ![Path Abs Dir]
  , edInclude :: ![Path Abs Dir]
  , edLib :: ![Path Abs Dir]
  }
  deriving (Show, Generic)

instance Semigroup ExtraDirs where
  (<>) = mappenddefault

instance Monoid ExtraDirs where
  mempty = memptydefault
  mappend = (<>)
