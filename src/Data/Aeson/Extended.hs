-- | The purpose of this module is to provide better failure messages
-- When parsing a key of an object, this makes sure the key itself will show up
module Data.Aeson.Extended (
    module Export
  , (.:)
  , (.:?)
  ) where

import Data.Aeson as Export hiding ((.:), (.:?))
import qualified Data.Aeson as A

import Data.Aeson.Types hiding ((.:), (.:?))

import Data.Text (unpack, Text)
import Data.Monoid ((<>))

(.:) :: FromJSON a => Object -> Text -> Parser a
(.:) o p = modifyFailure (("failed to parse field " <> unpack p <> ": ") <>) (o A..: p)
{-# INLINE (.:) #-}

(.:?) :: FromJSON a => Object -> Text -> Parser (Maybe a)
(.:?) o p = modifyFailure (("failed to parse field " <> unpack p <> ": ") <>) (o A..:? p)
{-# INLINE (.:?) #-}
