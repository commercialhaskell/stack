{-# LANGUAGE RecordWildCards #-}

-- | Build configuration
module Stack.Config.Build where

import           Data.Monoid.Extra
import           Stack.Types.Config

-- | Interprets BuildOptsMonoid options.
buildOptsFromMonoid :: BuildOptsMonoid -> BuildOpts
buildOptsFromMonoid BuildOptsMonoid{..} = BuildOpts
    { boptsLibProfile = fromFirst
          (boptsLibProfile defaultBuildOpts)
          buildMonoidLibProfile
    , boptsExeProfile = fromFirst
          (boptsExeProfile defaultBuildOpts)
          buildMonoidExeProfile
    , boptsLibStrip = fromFirst
          (boptsLibStrip defaultBuildOpts)
          buildMonoidLibStrip
    , boptsExeStrip = fromFirst
          (boptsExeStrip defaultBuildOpts)
          buildMonoidExeStrip
    , boptsHaddock = fromFirst
          (boptsHaddock defaultBuildOpts)
          buildMonoidHaddock
    , boptsHaddockOpts = haddockOptsFromMonoid buildMonoidHaddockOpts
    , boptsOpenHaddocks = fromFirst
          (boptsOpenHaddocks defaultBuildOpts)
          buildMonoidOpenHaddocks
    , boptsHaddockDeps = getFirst buildMonoidHaddockDeps
    , boptsHaddockInternal = fromFirst
          (boptsHaddockInternal defaultBuildOpts)
          buildMonoidHaddockInternal
    , boptsInstallExes = fromFirst
          (boptsInstallExes defaultBuildOpts)
          buildMonoidInstallExes
    , boptsPreFetch = fromFirst
          (boptsPreFetch defaultBuildOpts)
          buildMonoidPreFetch
    , boptsKeepGoing = getFirst buildMonoidKeepGoing
    , boptsForceDirty = fromFirst
          (boptsForceDirty defaultBuildOpts)
          buildMonoidForceDirty
    , boptsTests = fromFirst (boptsTests defaultBuildOpts) buildMonoidTests
    , boptsTestOpts = testOptsFromMonoid buildMonoidTestOpts
    , boptsBenchmarks = fromFirst
          (boptsBenchmarks defaultBuildOpts)
          buildMonoidBenchmarks
    , boptsBenchmarkOpts = benchmarkOptsFromMonoid buildMonoidBenchmarkOpts
    , boptsReconfigure = fromFirst
          (boptsReconfigure defaultBuildOpts)
          buildMonoidReconfigure
    , boptsCabalVerbose = fromFirst
          (boptsCabalVerbose defaultBuildOpts)
          buildMonoidCabalVerbose
    , boptsSplitObjs = fromFirst
          (boptsSplitObjs defaultBuildOpts)
          buildMonoidSplitObjs
    }


haddockOptsFromMonoid :: HaddockOptsMonoid -> HaddockOpts
haddockOptsFromMonoid HaddockOptsMonoid{..} =
    defaultHaddockOpts
    {hoAdditionalArgs = hoMonoidAdditionalArgs}

testOptsFromMonoid :: TestOptsMonoid -> TestOpts
testOptsFromMonoid TestOptsMonoid{..} =
    defaultTestOpts
    { toRerunTests = fromFirst (toRerunTests defaultTestOpts) toMonoidRerunTests
    , toAdditionalArgs = toMonoidAdditionalArgs
    , toCoverage = fromFirst (toCoverage defaultTestOpts) toMonoidCoverage
    , toDisableRun = fromFirst (toDisableRun defaultTestOpts) toMonoidDisableRun
    }

benchmarkOptsFromMonoid :: BenchmarkOptsMonoid -> BenchmarkOpts
benchmarkOptsFromMonoid BenchmarkOptsMonoid{..} =
    defaultBenchmarkOpts
    { beoAdditionalArgs = getFirst beoMonoidAdditionalArgs
    , beoDisableRun = fromFirst
          (beoDisableRun defaultBenchmarkOpts)
          beoMonoidDisableRun
    }
