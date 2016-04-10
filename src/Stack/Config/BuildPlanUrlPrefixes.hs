{-# LANGUAGE OverloadedStrings #-}

module Stack.Config.BuildPlanUrlPrefixes (buildPlanUrlPrefixesFromMonoid) where

import           Stack.Types
import           Data.Maybe

buildPlanUrlPrefixesFromMonoid
    :: BuildPlanUrlPrefixesMonoid -> BuildPlanUrlPrefixes
buildPlanUrlPrefixesFromMonoid monoid =
    BuildPlanUrlPrefixes
        (fromMaybe defaultLts     $ buildPlanUrlPrefixesMonoidLts     monoid)
        (fromMaybe defaultNightly $ buildPlanUrlPrefixesMonoidNightly monoid)
    where
    defaultLts =
        "https://raw.githubusercontent.com/fpco/lts-haskell/master/"
    defaultNightly =
        "https://raw.githubusercontent.com/fpco/stackage-nightly/master/"
