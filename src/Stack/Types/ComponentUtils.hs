{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DuplicateRecordFields      #-}

{-|
Module      : Stack.Types.ComponentUtils
License     : BSD-3-Clause

A module providing a type representing the name of an \'unqualified\' component
and related helper functions.
-}

module Stack.Types.ComponentUtils
  ( StackUnqualCompName (..)
  , unqualCompToText
  , unqualCompFromText
  , unqualCompToString
  , unqualCompFromString
  , emptyCompName
  , fromCabalName
  , toCabalName
  ) where

import           Data.Aeson ( FromJSON (..) )
import           Data.Hashable ( Hashable (..) )
import           Distribution.Compat.Binary ( decode, encode )
import           Distribution.PackageDescription
                   ( UnqualComponentName, mkUnqualComponentName
                   , unUnqualComponentName, unUnqualComponentNameST
                   )
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
newtype StackUnqualCompName = StackUnqualCompName UnqualComponentName
  deriving (Data, Eq, Generic, IsString, NFData, Ord, Read, Show, Typeable)

instance Hashable StackUnqualCompName where
  hashWithSalt a v = hashWithSalt a (show v)

fromCabalName :: UnqualComponentName -> StackUnqualCompName
fromCabalName = StackUnqualCompName

toCabalName :: StackUnqualCompName -> UnqualComponentName
toCabalName (StackUnqualCompName unqualName) = unqualName

unqualCompToString :: StackUnqualCompName -> String
unqualCompToString = unUnqualComponentName . toCabalName
unqualCompFromString :: String -> StackUnqualCompName
unqualCompFromString = StackUnqualCompName . mkUnqualComponentName
unqualCompToText :: StackUnqualCompName -> Text
unqualCompToText = (decode . encode) . unUnqualComponentNameST . toCabalName
unqualCompFromText :: Text -> StackUnqualCompName
unqualCompFromText = StackUnqualCompName . decode . encode

emptyCompName :: StackUnqualCompName
emptyCompName = StackUnqualCompName $ mkUnqualComponentName ""

instance FromJSON StackUnqualCompName where
  parseJSON = fmap (StackUnqualCompName . decode . encode) <$> parseJSON @Text
