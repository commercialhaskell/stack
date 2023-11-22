{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | A module providing the types that represent different sorts of components
-- of a package (library and sub-library, foreign library, executable, test
-- suite and benchmark).
module Stack.Types.ComponentUtils
  ( StackUnqualCompName (..)
  , fromCabalName
  , toCabalName
  ) where

import           Distribution.PackageDescription (UnqualComponentName, unUnqualComponentName, mkUnqualComponentName)
import           Stack.Prelude
import RIO.Text (pack, unpack)

-- | Type representing the name of an \'unqualified\' component (that is, the
-- component can be any sort - a (unnamed) main library or sub-library,
-- an executable, etc. ).
--
-- The corresponding The Cabal-syntax type is
-- 'Distribution.Types.UnqualComponentName.UnqualComponentName'.

-- Ideally, we would use the Cabal-syntax type and not 'Text', to avoid
-- unnecessary work, but there is no 'Hashable' instance for
-- 'Distribution.Types.UnqualComponentName.UnqualComponentName' yet.
newtype StackUnqualCompName = StackUnqualCompName {unqualCompToText :: Text}
  deriving (Data, Eq, Hashable, IsString, Generic, NFData, Ord, Show, Typeable, Read)

fromCabalName :: UnqualComponentName -> StackUnqualCompName
fromCabalName unqualName =
  StackUnqualCompName $ pack . unUnqualComponentName $ unqualName

toCabalName :: StackUnqualCompName -> UnqualComponentName
toCabalName (StackUnqualCompName unqualName) =
  mkUnqualComponentName (unpack unqualName)