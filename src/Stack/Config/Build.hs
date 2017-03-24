{-# LANGUAGE RecordWildCards #-}

-- | Build configuration
module Stack.Config.Build where

import           Data.Maybe
import           Data.Monoid.Extra
import           Stack.Types.Config

-- | Interprets BuildOptsMonoid options.
buildOptsFromMonoid :: BuildOptsMonoid -> BuildOpts
buildOptsFromMonoid BuildOptsMonoid{..} = BuildOpts
    { boptsLibProfile = fromFirst
          (boptsLibProfile defaultBuildOpts)
          (buildMonoidLibProfile <>
           First (if tracing || profiling then Just True else Nothing))
    , boptsExeProfile = fromFirst
          (boptsExeProfile defaultBuildOpts)
          (buildMonoidExeProfile <>
           First (if tracing || profiling then Just True else Nothing))
    , boptsLibStrip = fromFirst
          (boptsLibStrip defaultBuildOpts)
          (buildMonoidLibStrip <>
           First (if noStripping then Just False else Nothing))
    , boptsExeStrip = fromFirst
          (boptsExeStrip defaultBuildOpts)
          (buildMonoidExeStrip <>
           First (if noStripping then Just False else Nothing))
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
    , boptsTestOpts =
          testOptsFromMonoid buildMonoidTestOpts additionalArgs
    , boptsBenchmarks = fromFirst
          (boptsBenchmarks defaultBuildOpts)
          buildMonoidBenchmarks
    , boptsBenchmarkOpts =
          benchmarkOptsFromMonoid buildMonoidBenchmarkOpts additionalArgs
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
  where
    -- These options are not directly used in bopts, instead they
    -- transform other options.
    tracing = fromFirst False buildMonoidTrace
    profiling = fromFirst False buildMonoidProfile
    noStripping = getAny buildMonoidNoStrip
    -- Additional args for tracing / profiling
    additionalArgs =
        if tracing || profiling
            then Just $ "+RTS" : catMaybes [trac, prof, Just "-RTS"]
            else Nothing
    trac =
        if tracing
            then Just "-xc"
            else Nothing
    prof =
        if profiling
            then Just "-p"
            else Nothing

haddockOptsFromMonoid :: HaddockOptsMonoid -> HaddockOpts
haddockOptsFromMonoid HaddockOptsMonoid{..} =
    defaultHaddockOpts
    {hoAdditionalArgs = hoMonoidAdditionalArgs}

testOptsFromMonoid :: TestOptsMonoid -> Maybe [String] -> TestOpts
testOptsFromMonoid TestOptsMonoid{..} madditional =
    defaultTestOpts
    { toRerunTests = fromFirst (toRerunTests defaultTestOpts) toMonoidRerunTests
    , toAdditionalArgs = fromMaybe [] madditional <> toMonoidAdditionalArgs
    , toCoverage = fromFirst (toCoverage defaultTestOpts) toMonoidCoverage
    , toDisableRun = fromFirst (toDisableRun defaultTestOpts) toMonoidDisableRun
    }

benchmarkOptsFromMonoid :: BenchmarkOptsMonoid -> Maybe [String] -> BenchmarkOpts
benchmarkOptsFromMonoid BenchmarkOptsMonoid{..} madditional =
    defaultBenchmarkOpts
    { beoAdditionalArgs =
          fmap (\args -> unwords args <> " ") madditional <>
          getFirst beoMonoidAdditionalArgs
    , beoDisableRun = fromFirst
          (beoDisableRun defaultBenchmarkOpts)
          beoMonoidDisableRun
    }
