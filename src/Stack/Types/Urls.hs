{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.Urls where

import Control.Applicative
import Data.Aeson.Extended
import Data.Text (Text)
import Data.Monoid
import GHC.Generics (Generic)
import Generics.Deriving.Monoid (memptydefault, mappenddefault)
import Prelude

data Urls = Urls
    { urlsLatestSnapshot :: !Text
    , urlsLtsBuildPlans :: !Text
    , urlsNightlyBuildPlans :: !Text
    }
    deriving Show

-- TODO: Really need this instance?
instance FromJSON (WithJSONWarnings Urls) where
    parseJSON = withObjectWarnings "Urls" $ \o -> do
        Urls
            <$> o ..: "latest-snapshot"
            <*> o ..: "lts-build-plans"
            <*> o ..: "nightly-build-plans"

data UrlsMonoid = UrlsMonoid
    { urlsMonoidLatestSnapshot :: !(First Text)
    , urlsMonoidLtsBuildPlans :: !(First Text)
    , urlsMonoidNightlyBuildPlans :: !(First Text)
    }
    deriving (Show, Generic)

instance FromJSON (WithJSONWarnings UrlsMonoid) where
    parseJSON = withObjectWarnings "UrlsMonoid" $ \o -> do
        UrlsMonoid
            <$> o ..: "latest-snapshot"
            <*> o ..: "lts-build-plans"
            <*> o ..: "nightly-build-plans"

instance Monoid UrlsMonoid where
    mempty = memptydefault
    mappend = mappenddefault
