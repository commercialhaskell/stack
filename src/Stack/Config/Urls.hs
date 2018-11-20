{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Config.Urls (urlsFromMonoid) where

import           Stack.Types.Urls
import           Stack.Prelude

urlsFromMonoid :: UrlsMonoid -> Urls
urlsFromMonoid monoid =
    Urls
        (fromFirst defaultLatestSnapshot    $ urlsMonoidLatestSnapshot    monoid)
    where
    defaultLatestSnapshot =
        "https://s3.amazonaws.com/haddock.stackage.org/snapshots.json"
