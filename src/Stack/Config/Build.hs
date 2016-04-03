{-# LANGUAGE RecordWildCards #-}

-- | Build configuration
module Stack.Config.Build where

import           Data.Maybe          (fromMaybe)
import           Stack.Types

-- | Interprets BuildOptsMonoid options.
buildOptsFromMonoid :: BuildOptsMonoid -> BuildOpts
buildOptsFromMonoid BuildOptsMonoid{..} = BuildOpts
    { boptsLibProfile = fromMaybe
          (boptsLibProfile defaultBuildOpts)
          buildMonoidLibProfile
    , boptsExeProfile = fromMaybe
          (boptsExeProfile defaultBuildOpts)
          buildMonoidExeProfile
    , boptsHaddock = fromMaybe
          (boptsHaddock defaultBuildOpts)
          buildMonoidHaddock
    , boptsOpenHaddocks = fromMaybe
          (boptsOpenHaddocks defaultBuildOpts)
          buildMonoidOpenHaddocks
    , boptsHaddockDeps = buildMonoidHaddockDeps
    , boptsInstallExes = fromMaybe
          (boptsInstallExes defaultBuildOpts)
          buildMonoidInstallExes
    , boptsPreFetch = fromMaybe
          (boptsPreFetch defaultBuildOpts)
          buildMonoidPreFetch
    , boptsKeepGoing = buildMonoidKeepGoing
    , boptsForceDirty = fromMaybe
          (boptsForceDirty defaultBuildOpts)
          buildMonoidForceDirty
    , boptsTests = fromMaybe (boptsTests defaultBuildOpts) buildMonoidTests
    , boptsTestOpts = testOptsFromMonoid buildMonoidTestOpts
    , boptsBenchmarks = fromMaybe
          (boptsBenchmarks defaultBuildOpts)
          buildMonoidBenchmarks
    , boptsBenchmarkOpts = benchmarkOptsFromMonoid buildMonoidBenchmarkOpts
    , boptsReconfigure = fromMaybe
          (boptsReconfigure defaultBuildOpts)
          buildMonoidReconfigure
    , boptsCabalVerbose = fromMaybe
          (boptsCabalVerbose defaultBuildOpts)
          buildMonoidCabalVerbose
    , boptsSplitObjs = fromMaybe
          (boptsSplitObjs defaultBuildOpts)
          buildMonoidSplitObjs
    }

testOptsFromMonoid :: TestOptsMonoid -> TestOpts
testOptsFromMonoid TestOptsMonoid{..} =
    defaultTestOpts
    { toRerunTests = fromMaybe (toRerunTests defaultTestOpts) toMonoidRerunTests
    , toAdditionalArgs = toMonoidAdditionalArgs
    , toCoverage = fromMaybe (toCoverage defaultTestOpts) toMonoidCoverage
    , toDisableRun = fromMaybe (toDisableRun defaultTestOpts) toMonoidDisableRun
    }

benchmarkOptsFromMonoid :: BenchmarkOptsMonoid -> BenchmarkOpts
benchmarkOptsFromMonoid BenchmarkOptsMonoid{..} =
    defaultBenchmarkOpts
    { beoAdditionalArgs = beoMonoidAdditionalArgs
    , beoDisableRun = fromMaybe
          (beoDisableRun defaultBenchmarkOpts)
          beoMonoidDisableRun
    }
