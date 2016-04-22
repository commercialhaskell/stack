{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Stack.Types.Urls where

import Control.Applicative
import Data.Aeson.Extended
import Data.Text (Text)
import Data.Monoid
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
    { urlsMonoidLatestSnapshot :: !(Maybe Text)
    , urlsMonoidLtsBuildPlans :: !(Maybe Text)
    , urlsMonoidNightlyBuildPlans :: !(Maybe Text)
    }
    deriving Show

instance FromJSON (WithJSONWarnings UrlsMonoid) where
    parseJSON = withObjectWarnings "UrlsMonoid" $ \o -> do
        UrlsMonoid
            <$> o ..: "latest-snapshot"
            <*> o ..: "lts-build-plans"
            <*> o ..: "nightly-build-plans"

instance Monoid UrlsMonoid where
    mempty = UrlsMonoid Nothing Nothing Nothing
    mappend l r = UrlsMonoid
      { urlsMonoidLatestSnapshot = urlsMonoidLatestSnapshot l <|> urlsMonoidLatestSnapshot r
      , urlsMonoidLtsBuildPlans = urlsMonoidLtsBuildPlans l <|> urlsMonoidLtsBuildPlans r
      , urlsMonoidNightlyBuildPlans = urlsMonoidNightlyBuildPlans l <|> urlsMonoidNightlyBuildPlans r
      }
