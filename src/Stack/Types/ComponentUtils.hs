{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | A module providing a type representing the name of an \'unqualified\'
-- component and related helper functions.
module Stack.Types.ComponentUtils
  ( StackUnqualCompName (..)
  , fromCabalName
  , toCabalName
  ) where

import           Distribution.PackageDescription
                   ( UnqualComponentName, mkUnqualComponentName
                   , unUnqualComponentName
                   )
import           RIO.Text (pack, unpack)
import           Stack.Prelude

-- | Type representing the name of an \'unqualified\' component (that is, the
-- component can be any sort - a (unnamed) main library or sub-library,
-- an executable, etc. ).
--
-- The corresponding The Cabal-syntax type is
-- 'Distribution.Types.UnqualComponentName.UnqualComponentName'.

-- Ideally, we would use the Cabal-syntax type and not 'Text', to avoid
-- unnecessary work, but there is no 'Hashable' instance for
-- 'Distribution.Types.UnqualComponentName.UnqualComponentName' yet.
newtype StackUnqualCompName = StackUnqualCompName
  { unqualCompToText :: Text
  }
  deriving (Data, Eq, Generic, Hashable, IsString, NFData, Ord, Read, Show, Typeable)

fromCabalName :: UnqualComponentName -> StackUnqualCompName
fromCabalName unqualName =
  StackUnqualCompName $ pack . unUnqualComponentName $ unqualName

toCabalName :: StackUnqualCompName -> UnqualComponentName
toCabalName (StackUnqualCompName unqualName) =
  mkUnqualComponentName (unpack unqualName)
