{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.IsMutable
  ( IsMutable (..)
  ) where

import           Stack.Prelude

data IsMutable
  = Mutable
  | Immutable
  deriving (Eq, Show)

instance Semigroup IsMutable where
  Mutable <> _ = Mutable
  _ <> Mutable = Mutable
  Immutable <> Immutable = Immutable

instance Monoid IsMutable where
  mempty = Immutable
  mappend = (<>)
