{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Module exporting the 'Curator' type, used to represent Stack's
-- project-specific @curator@ option, which supports the needs of the
-- [@curator@ tool](https://github.com/commercialhaskell/curator).
module Stack.Types.Curator
  ( Curator (..)
  ) where

import           Data.Aeson.Types ( FromJSON (..), ToJSON (..), (.=), object )
import           Data.Aeson.WarningParser
                   ( WithJSONWarnings (..), (..:?), (..!=), withObjectWarnings )
import qualified Data.Set as Set
import           Stack.Prelude

-- | Type representing configuration options which support the needs of the
-- [@curator@ tool](https://github.com/commercialhaskell/curator).
data Curator = Curator
  { curatorSkipTest :: !(Set PackageName)
    -- ^ Packages for which Stack should ignore test suites.
  , curatorExpectTestFailure :: !(Set PackageName)
    -- ^ Packages for which Stack should expect building test suites to fail.
  , curatorSkipBenchmark :: !(Set PackageName)
    -- ^ Packages for which Stack should ignore benchmarks.
  , curatorExpectBenchmarkFailure :: !(Set PackageName)
    -- ^ Packages for which Stack should expect building benchmarks to fail.
  , curatorSkipHaddock :: !(Set PackageName)
    -- ^ Packages for which Stack should ignore creating Haddock documentation.
  , curatorExpectHaddockFailure :: !(Set PackageName)
    -- ^ Packages for which Stack should expect creating Haddock documentation
    -- to fail.
  }
  deriving Show

instance ToJSON Curator where
  toJSON c = object
    [ "skip-test" .= Set.map CabalString c.curatorSkipTest
    , "expect-test-failure" .= Set.map CabalString c.curatorExpectTestFailure
    , "skip-bench" .= Set.map CabalString c.curatorSkipBenchmark
    , "expect-benchmark-failure" .=
        Set.map CabalString c.curatorExpectTestFailure
    , "skip-haddock" .= Set.map CabalString c.curatorSkipHaddock
    , "expect-haddock-failure" .=
        Set.map CabalString c.curatorExpectHaddockFailure
    ]

instance FromJSON (WithJSONWarnings Curator) where
  parseJSON = withObjectWarnings "Curator" $ \o -> Curator
    <$> fmap (Set.map unCabalString) (o ..:? "skip-test" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "expect-test-failure" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "skip-bench" ..!= mempty)
    <*> fmap
          (Set.map unCabalString)
          (o ..:? "expect-benchmark-failure" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "skip-haddock" ..!= mempty)
    <*> fmap
          (Set.map unCabalString)
          (o ..:? "expect-haddock-failure" ..!= mempty)
