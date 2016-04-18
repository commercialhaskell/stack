{-# LANGUAGE OverloadedStrings #-}

module Stack.Config.Urls (urlsFromMonoid) where

import           Stack.Types
import           Data.Maybe

urlsFromMonoid :: UrlsMonoid -> Urls
urlsFromMonoid monoid =
    Urls
        (fromMaybe defaultLatestSnapshot    $ urlsMonoidLatestSnapshot    monoid)
        (fromMaybe defaultLtsBuildPlans     $ urlsMonoidLtsBuildPlans     monoid)
        (fromMaybe defaultNightlyBuildPlans $ urlsMonoidNightlyBuildPlans monoid)
    where
    defaultLatestSnapshot =
        "https://www.stackage.org/download/snapshots.json"
    defaultLtsBuildPlans =
        "https://raw.githubusercontent.com/fpco/lts-haskell/master/"
    defaultNightlyBuildPlans =
        "https://raw.githubusercontent.com/fpco/stackage-nightly/master/"
