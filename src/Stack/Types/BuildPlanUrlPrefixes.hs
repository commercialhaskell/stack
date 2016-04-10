{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Stack.Types.BuildPlanUrlPrefixes where

import Control.Applicative
import Data.Aeson.Extended
import Data.Text (Text)
import Data.Monoid

data BuildPlanUrlPrefixes = BuildPlanUrlPrefixes
    { buildPlanUrlPrefixesLts :: !Text
    , buildPlanUrlPrefixesNightly :: !Text
    }
    deriving Show

instance FromJSON (WithJSONWarnings BuildPlanUrlPrefixes) where
    parseJSON = withObjectWarnings "BuildPlanUrlPrefixes" $ \o -> do
        BuildPlanUrlPrefixes <$> o ..: "lts" <*> o ..: "nightly"

data BuildPlanUrlPrefixesMonoid = BuildPlanUrlPrefixesMonoid
    { buildPlanUrlPrefixesMonoidLts :: !(Maybe Text)
    , buildPlanUrlPrefixesMonoidNightly :: !(Maybe Text)
    }
    deriving Show

instance FromJSON (WithJSONWarnings BuildPlanUrlPrefixesMonoid) where
    parseJSON = withObjectWarnings "BuildPlanUrlPrefixesMonoid" $ \o -> do
        BuildPlanUrlPrefixesMonoid <$> o ..: "lts" <*> o ..: "nightly"

instance Monoid BuildPlanUrlPrefixesMonoid where
    mempty = BuildPlanUrlPrefixesMonoid Nothing Nothing
    mappend l r = BuildPlanUrlPrefixesMonoid
      { buildPlanUrlPrefixesMonoidLts = buildPlanUrlPrefixesMonoidLts l <|> buildPlanUrlPrefixesMonoidLts r
      , buildPlanUrlPrefixesMonoidNightly = buildPlanUrlPrefixesMonoidNightly l <|> buildPlanUrlPrefixesMonoidNightly r
      }
