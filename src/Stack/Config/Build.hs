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
  { libProfile = fromFirstFalse
      (buildMonoid.buildMonoidLibProfile <>
       FirstFalse (if tracing || profiling then Just True else Nothing))
  , exeProfile = fromFirstFalse
      (buildMonoid.buildMonoidExeProfile <>
       FirstFalse (if tracing || profiling then Just True else Nothing))
  , libStrip = fromFirstTrue
      (buildMonoid.buildMonoidLibStrip <>
       FirstTrue (if noStripping then Just False else Nothing))
  , exeStrip = fromFirstTrue
      (buildMonoid.buildMonoidExeStrip <>
       FirstTrue (if noStripping then Just False else Nothing))
  , haddock = fromFirstFalse buildMonoid.buildMonoidHaddock
  , haddockOpts = haddockOptsFromMonoid buildMonoid.buildMonoidHaddockOpts
  , openHaddocks =
         not isHaddockFromHackage
      && fromFirstFalse buildMonoid.buildMonoidOpenHaddocks
  , haddockDeps = if isHaddockFromHackage
      then Nothing
      else getFirst buildMonoid.buildMonoidHaddockDeps
  , haddockInternal =
         not isHaddockFromHackage
      && fromFirstFalse buildMonoid.buildMonoidHaddockInternal
  , haddockHyperlinkSource =
         isHaddockFromHackage
      || fromFirstTrue buildMonoid.buildMonoidHaddockHyperlinkSource
  , haddockForHackage = isHaddockFromHackage
  , installExes = fromFirstFalse buildMonoid.buildMonoidInstallExes
  , installCompilerTool =
      fromFirstFalse buildMonoid.buildMonoidInstallCompilerTool
  , preFetch = fromFirstFalse buildMonoid.buildMonoidPreFetch
  , keepGoing = getFirst buildMonoid.buildMonoidKeepGoing
  , keepTmpFiles = fromFirstFalse buildMonoid.buildMonoidKeepTmpFiles
  , forceDirty =
      isHaddockFromHackage || fromFirstFalse buildMonoid.buildMonoidForceDirty
  , tests = fromFirstFalse buildMonoid.buildMonoidTests
  , testOpts =
      testOptsFromMonoid buildMonoid.buildMonoidTestOpts additionalArgs
  , benchmarks = fromFirstFalse buildMonoid.buildMonoidBenchmarks
  , benchmarkOpts =
      benchmarkOptsFromMonoid
        buildMonoid.buildMonoidBenchmarkOpts
        additionalArgs
  , reconfigure = fromFirstFalse buildMonoid.buildMonoidReconfigure
  , cabalVerbose =
      fromFirst (CabalVerbosity normal) buildMonoid.buildMonoidCabalVerbose
  , splitObjs = fromFirstFalse buildMonoid.buildMonoidSplitObjs
  , skipComponents = buildMonoid.buildMonoidSkipComponents
  , interleavedOutput = fromFirstTrue buildMonoid.buildMonoidInterleavedOutput
  , progressBar = fromFirst CappedBar buildMonoid.buildMonoidProgressBar
  , ddumpDir = getFirst buildMonoid.buildMonoidDdumpDir
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
