{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Build configuration
module Stack.Config.Build
 ( benchmarkOptsFromMonoid
 , buildOptsFromMonoid
 , haddockOptsFromMonoid
 , testOptsFromMonoid
 ) where

import           Distribution.Verbosity ( normal )
import           Stack.Prelude
import           Stack.Types.BuildOpts
                   ( BenchmarkOpts (..), BenchmarkOptsMonoid (..)
                   , BuildOpts (..), BuildOptsMonoid (..), CabalVerbosity (..)
                   , HaddockOpts (..), HaddockOptsMonoid (..)
                   , ProgressBarFormat (..), TestOpts (..), TestOptsMonoid (..)
                   , defaultBenchmarkOpts, defaultHaddockOpts, defaultTestOpts
                   )

-- | Interprets BuildOptsMonoid options.
buildOptsFromMonoid :: BuildOptsMonoid -> BuildOpts
buildOptsFromMonoid buildMonoid = BuildOpts
  { boptsLibProfile = fromFirstFalse
      (buildMonoid.buildMonoidLibProfile <>
       FirstFalse (if tracing || profiling then Just True else Nothing))
  , boptsExeProfile = fromFirstFalse
      (buildMonoid.buildMonoidExeProfile <>
       FirstFalse (if tracing || profiling then Just True else Nothing))
  , boptsLibStrip = fromFirstTrue
      (buildMonoid.buildMonoidLibStrip <>
       FirstTrue (if noStripping then Just False else Nothing))
  , boptsExeStrip = fromFirstTrue
      (buildMonoid.buildMonoidExeStrip <>
       FirstTrue (if noStripping then Just False else Nothing))
  , boptsHaddock = fromFirstFalse buildMonoid.buildMonoidHaddock
  , boptsHaddockOpts = haddockOptsFromMonoid buildMonoid.buildMonoidHaddockOpts
  , boptsOpenHaddocks =
         not isHaddockFromHackage
      && fromFirstFalse buildMonoid.buildMonoidOpenHaddocks
  , boptsHaddockDeps = if isHaddockFromHackage
      then Nothing
      else getFirst buildMonoid.buildMonoidHaddockDeps
  , boptsHaddockInternal =
         not isHaddockFromHackage
      && fromFirstFalse buildMonoid.buildMonoidHaddockInternal
  , boptsHaddockHyperlinkSource =
         isHaddockFromHackage
      || fromFirstTrue buildMonoid.buildMonoidHaddockHyperlinkSource
  , boptsHaddockForHackage = isHaddockFromHackage
  , boptsInstallExes = fromFirstFalse buildMonoid.buildMonoidInstallExes
  , boptsInstallCompilerTool =
      fromFirstFalse buildMonoid.buildMonoidInstallCompilerTool
  , boptsPreFetch = fromFirstFalse buildMonoid.buildMonoidPreFetch
  , boptsKeepGoing = getFirst buildMonoid.buildMonoidKeepGoing
  , boptsKeepTmpFiles = fromFirstFalse buildMonoid.buildMonoidKeepTmpFiles
  , boptsForceDirty =
      isHaddockFromHackage || fromFirstFalse buildMonoid.buildMonoidForceDirty
  , boptsTests = fromFirstFalse buildMonoid.buildMonoidTests
  , boptsTestOpts =
      testOptsFromMonoid buildMonoid.buildMonoidTestOpts additionalArgs
  , boptsBenchmarks = fromFirstFalse buildMonoid.buildMonoidBenchmarks
  , boptsBenchmarkOpts =
      benchmarkOptsFromMonoid
        buildMonoid.buildMonoidBenchmarkOpts
        additionalArgs
  , boptsReconfigure = fromFirstFalse buildMonoid.buildMonoidReconfigure
  , boptsCabalVerbose =
      fromFirst (CabalVerbosity normal) buildMonoid.buildMonoidCabalVerbose
  , boptsSplitObjs = fromFirstFalse buildMonoid.buildMonoidSplitObjs
  , boptsSkipComponents = buildMonoid.buildMonoidSkipComponents
  , boptsInterleavedOutput =
      fromFirstTrue buildMonoid.buildMonoidInterleavedOutput
  , boptsProgressBar = fromFirst CappedBar buildMonoid.buildMonoidProgressBar
  , boptsDdumpDir = getFirst buildMonoid.buildMonoidDdumpDir
  }
 where
  isHaddockFromHackage = fromFirstFalse buildMonoid.buildMonoidHaddockForHackage
  -- These options are not directly used in bopts, instead they
  -- transform other options.
  tracing = getAny buildMonoid.buildMonoidTrace
  profiling = getAny buildMonoid.buildMonoidProfile
  noStripping = getAny buildMonoid.buildMonoidNoStrip
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
haddockOptsFromMonoid hoMonoid = defaultHaddockOpts
  { hoAdditionalArgs = hoMonoid.hoMonoidAdditionalArgs }

testOptsFromMonoid :: TestOptsMonoid -> Maybe [String] -> TestOpts
testOptsFromMonoid toMonoid madditional = defaultTestOpts
  { toRerunTests = fromFirstTrue toMonoid.toMonoidRerunTests
  , toAdditionalArgs =
      fromMaybe [] madditional <> toMonoid.toMonoidAdditionalArgs
  , toCoverage = fromFirstFalse toMonoid.toMonoidCoverage
  , toDisableRun = fromFirstFalse toMonoid.toMonoidDisableRun
  , toMaximumTimeSeconds =
      fromFirst
        defaultTestOpts.toMaximumTimeSeconds
        toMonoid.toMonoidMaximumTimeSeconds
  , toAllowStdin = fromFirstTrue toMonoid.toMonoidAllowStdin
  }

benchmarkOptsFromMonoid ::
     BenchmarkOptsMonoid
  -> Maybe [String]
  -> BenchmarkOpts
benchmarkOptsFromMonoid beoMonoid madditional =
  defaultBenchmarkOpts
    { beoAdditionalArgs =
        fmap (\args -> unwords args <> " ") madditional <>
        getFirst beoMonoid.beoMonoidAdditionalArgs
    , beoDisableRun = fromFirst
        defaultBenchmarkOpts.beoDisableRun
        beoMonoid.beoMonoidDisableRun
    }
