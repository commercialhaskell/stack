{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.Curator
  ( Curator (..)
  ) where

import           Data.Aeson.Types ( FromJSON (..), ToJSON (..), (.=), object )
import           Data.Aeson.WarningParser
                   ( WithJSONWarnings (..), (..:?), (..!=), withObjectWarnings )
import qualified Data.Set as Set
import           Stack.Prelude

-- | Extra configuration intended exclusively for usage by the curator tool. In
-- other words, this is /not/ part of the documented and exposed Stack API.
-- SUBJECT TO CHANGE.
data Curator = Curator
  { curatorSkipTest :: !(Set PackageName)
  , curatorExpectTestFailure :: !(Set PackageName)
  , curatorSkipBenchmark :: !(Set PackageName)
  , curatorExpectBenchmarkFailure :: !(Set PackageName)
  , curatorSkipHaddock :: !(Set PackageName)
  , curatorExpectHaddockFailure :: !(Set PackageName)
  }
  deriving Show

instance ToJSON Curator where
  toJSON c = object
    [ "skip-test" .= Set.map CabalString (curatorSkipTest c)
    , "expect-test-failure" .= Set.map CabalString (curatorExpectTestFailure c)
    , "skip-bench" .= Set.map CabalString (curatorSkipBenchmark c)
    , "expect-benchmark-failure" .=
        Set.map CabalString (curatorExpectTestFailure c)
    , "skip-haddock" .= Set.map CabalString (curatorSkipHaddock c)
    , "expect-test-failure" .=
        Set.map CabalString (curatorExpectHaddockFailure c)
    ]

instance FromJSON (WithJSONWarnings Curator) where
  parseJSON = withObjectWarnings "Curator" $ \o -> Curator
    <$> fmap (Set.map unCabalString) (o ..:? "skip-test" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "expect-test-failure" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "skip-bench" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "expect-benchmark-failure" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "skip-haddock" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "expect-haddock-failure" ..!= mempty)
