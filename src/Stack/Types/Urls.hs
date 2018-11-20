{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.Urls where

import Data.Aeson.Extended
import Generics.Deriving.Monoid (memptydefault, mappenddefault)
import Stack.Prelude

data Urls = Urls
    { urlsLatestSnapshot :: !Text
    }
    deriving Show

-- TODO: Really need this instance?
instance FromJSON (WithJSONWarnings Urls) where
    parseJSON = withObjectWarnings "Urls" $ \o -> do
        Urls
            <$> o ..: "latest-snapshot"

data UrlsMonoid = UrlsMonoid
    { urlsMonoidLatestSnapshot :: !(First Text)
    }
    deriving (Show, Generic)

instance FromJSON (WithJSONWarnings UrlsMonoid) where
    parseJSON = withObjectWarnings "UrlsMonoid" $ \o -> do
        UrlsMonoid
            <$> o ..: "latest-snapshot"

instance Semigroup UrlsMonoid where
    (<>) = mappenddefault

instance Monoid UrlsMonoid where
    mempty = memptydefault
    mappend = (<>)
