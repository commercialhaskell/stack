{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Build configuration
module Stack.Config.Build
 ( benchmarkOptsFromMonoid
 , buildOptsFromMonoid
 , haddockOptsFromMonoid
 , testOptsFromMonoid
 ) where

import           Distribution.Verbosity ( normal )
import           Stack.Prelude
import           Stack.Types.Config
                   ( BenchmarkOpts (..), BenchmarkOptsMonoid (..)
                   , BuildOpts (..), BuildOptsMonoid (..), CabalVerbosity (..)
                   , HaddockOpts (..), HaddockOptsMonoid (..), TestOpts (..)
                   , TestOptsMonoid (..), defaultBenchmarkOpts
                   , defaultHaddockOpts, defaultTestOpts
                   )

-- | Interprets BuildOptsMonoid options.
buildOptsFromMonoid :: BuildOptsMonoid -> BuildOpts
buildOptsFromMonoid BuildOptsMonoid{..} = BuildOpts
  { boptsLibProfile = fromFirstFalse
      (buildMonoidLibProfile <>
       FirstFalse (if tracing || profiling then Just True else Nothing))
  , boptsExeProfile = fromFirstFalse
      (buildMonoidExeProfile <>
       FirstFalse (if tracing || profiling then Just True else Nothing))
  , boptsLibStrip = fromFirstTrue
      (buildMonoidLibStrip <>
       FirstTrue (if noStripping then Just False else Nothing))
  , boptsExeStrip = fromFirstTrue
      (buildMonoidExeStrip <>
       FirstTrue (if noStripping then Just False else Nothing))
  , boptsHaddock = fromFirstFalse buildMonoidHaddock
  , boptsHaddockOpts = haddockOptsFromMonoid buildMonoidHaddockOpts
  , boptsOpenHaddocks = fromFirstFalse buildMonoidOpenHaddocks
  , boptsHaddockDeps = getFirst buildMonoidHaddockDeps
  , boptsHaddockInternal = fromFirstFalse buildMonoidHaddockInternal
  , boptsHaddockHyperlinkSource =
      fromFirstTrue buildMonoidHaddockHyperlinkSource
  , boptsInstallExes = fromFirstFalse buildMonoidInstallExes
  , boptsInstallCompilerTool = fromFirstFalse buildMonoidInstallCompilerTool
  , boptsPreFetch = fromFirstFalse buildMonoidPreFetch
  , boptsKeepGoing = getFirst buildMonoidKeepGoing
  , boptsKeepTmpFiles = fromFirstFalse buildMonoidKeepTmpFiles
  , boptsForceDirty = fromFirstFalse buildMonoidForceDirty
  , boptsTests = fromFirstFalse buildMonoidTests
  , boptsTestOpts =
      testOptsFromMonoid buildMonoidTestOpts additionalArgs
  , boptsBenchmarks = fromFirstFalse buildMonoidBenchmarks
  , boptsBenchmarkOpts =
      benchmarkOptsFromMonoid buildMonoidBenchmarkOpts additionalArgs
  , boptsReconfigure = fromFirstFalse buildMonoidReconfigure
  , boptsCabalVerbose =
      fromFirst (CabalVerbosity normal) buildMonoidCabalVerbose
  , boptsSplitObjs = fromFirstFalse buildMonoidSplitObjs
  , boptsSkipComponents = buildMonoidSkipComponents
  , boptsInterleavedOutput = fromFirstTrue buildMonoidInterleavedOutput
  , boptsDdumpDir = getFirst buildMonoidDdumpDir
  }
 where
  -- These options are not directly used in bopts, instead they
  -- transform other options.
  tracing = getAny buildMonoidTrace
  profiling = getAny buildMonoidProfile
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
haddockOptsFromMonoid HaddockOptsMonoid{..} = defaultHaddockOpts
  { hoAdditionalArgs = hoMonoidAdditionalArgs }

testOptsFromMonoid :: TestOptsMonoid -> Maybe [String] -> TestOpts
testOptsFromMonoid TestOptsMonoid{..} madditional = defaultTestOpts
  { toRerunTests = fromFirstTrue toMonoidRerunTests
  , toAdditionalArgs = fromMaybe [] madditional <> toMonoidAdditionalArgs
  , toCoverage = fromFirstFalse toMonoidCoverage
  , toDisableRun = fromFirstFalse toMonoidDisableRun
  , toMaximumTimeSeconds =
      fromFirst (toMaximumTimeSeconds defaultTestOpts) toMonoidMaximumTimeSeconds
  , toAllowStdin = fromFirstTrue toMonoidAllowStdin
  }

benchmarkOptsFromMonoid ::
     BenchmarkOptsMonoid
  -> Maybe [String]
  -> BenchmarkOpts
benchmarkOptsFromMonoid BenchmarkOptsMonoid{..} madditional =
  defaultBenchmarkOpts
    { beoAdditionalArgs =
        fmap (\args -> unwords args <> " ") madditional <>
        getFirst beoMonoidAdditionalArgs
    , beoDisableRun = fromFirst
        (beoDisableRun defaultBenchmarkOpts)
        beoMonoidDisableRun
    }
