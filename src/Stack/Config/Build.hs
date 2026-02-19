{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

{-|
Module      : Stack.Config.Build
Description : Build configuration.
License     : BSD-3-Clause

Build configuration.
-}

module Stack.Config.Build
 ( buildOptsFromMonoid
 , haddockOptsFromMonoid
 , testOptsFromMonoid
 , benchmarkOptsFromMonoid
 ) where

import           Distribution.Verbosity ( normal )
import           Stack.BuildOpts
                   ( defaultBenchmarkOpts, defaultHaddockOpts, defaultTestOpts )
import           Stack.Prelude
import           Stack.Types.BuildOpts
                   ( BenchmarkOpts (..), BuildOpts (..), HaddockOpts (..)
                   , TestOpts (..)
                   )
import qualified Stack.Types.BuildOpts as BenchmarkOpts ( BenchmarkOpts (..) )
import qualified Stack.Types.BuildOpts as HaddockOpts ( HaddockOpts (..) )
import qualified Stack.Types.BuildOpts as TestOpts ( TestOpts (..) )
import           Stack.Types.BuildOptsMonoid
                   ( BenchmarkOptsMonoid (..), BuildOptsMonoid (..)
                   , CabalVerbosity (..), HaddockOptsMonoid (..)
                   , ProgressBarFormat (..), TestOptsMonoid (..)
                   )

-- | Interprets BuildOptsMonoid options.
buildOptsFromMonoid :: BuildOptsMonoid -> BuildOpts
buildOptsFromMonoid buildMonoid = BuildOpts
  { libProfile = fromFirstFalse
      (  buildMonoid.libProfile
      <> FirstFalse (if tracing || profiling then Just True else Nothing)
      )
  , exeProfile = fromFirstFalse
      (  buildMonoid.exeProfile
      <> FirstFalse (if tracing || profiling then Just True else Nothing)
      )
  , libStrip = fromFirstTrue
      (  buildMonoid.libStrip
      <> FirstTrue (if noStripping then Just False else Nothing)
      )
  , exeStrip = fromFirstTrue
      (  buildMonoid.exeStrip
      <> FirstTrue (if noStripping then Just False else Nothing)
      )
  , buildHaddocks = fromFirstFalse buildMonoid.buildHaddocks
  , haddockOpts = haddockOptsFromMonoid buildMonoid.haddockOpts
  , openHaddocks =
         not isHaddockFromHackage
      && fromFirstFalse buildMonoid.openHaddocks
  , haddockDeps = if isHaddockFromHackage
      then Nothing
      else getFirst buildMonoid.haddockDeps
  , haddockExecutables =
         not isHaddockFromHackage
      && fromFirstFalse buildMonoid.haddockExecutables
  , haddockTests =
         not isHaddockFromHackage
      && fromFirstFalse buildMonoid.haddockTests
  , haddockBenchmarks =
         not isHaddockFromHackage
      && fromFirstFalse buildMonoid.haddockBenchmarks
  , haddockInternal =
         not isHaddockFromHackage
      && fromFirstFalse buildMonoid.haddockInternal
  , haddockHyperlinkSource =
         isHaddockFromHackage
      || fromFirstTrue buildMonoid.haddockHyperlinkSource
  , haddockForHackage = isHaddockFromHackage
  , installExes = fromFirstFalse buildMonoid.installExes
  , installCompilerTool = fromFirstFalse buildMonoid.installCompilerTool
  , preFetch = fromFirstFalse buildMonoid.preFetch
  , keepGoing = getFirst buildMonoid.keepGoing
  , keepTmpFiles = fromFirstFalse buildMonoid.keepTmpFiles
  , forceDirty = isHaddockFromHackage || fromFirstFalse buildMonoid.forceDirty
  , tests = fromFirstFalse buildMonoid.tests
  , testOpts = testOptsFromMonoid buildMonoid.testOpts additionalArgs
  , benchmarks = fromFirstFalse buildMonoid.benchmarks
  , benchmarkOpts =
      benchmarkOptsFromMonoid buildMonoid.benchmarkOpts additionalArgs
  , reconfigure = fromFirstFalse buildMonoid.reconfigure
  , cabalVerbose = fromFirst (CabalVerbosity normal) buildMonoid.cabalVerbose
  , splitObjs = fromFirstFalse buildMonoid.splitObjs
  , skipComponents = buildMonoid.skipComponents
  , interleavedOutput = fromFirstTrue buildMonoid.interleavedOutput
  , progressBar = fromFirst CappedBar buildMonoid.progressBar
  , ddumpDir = getFirst buildMonoid.ddumpDir
  , semaphore = fromFirstFalse buildMonoid.semaphore
  }
 where
  isHaddockFromHackage = fromFirstFalse buildMonoid.haddockForHackage
  -- These options are not directly used in bopts, instead they
  -- transform other options.
  tracing = getAny buildMonoid.trace
  profiling = getAny buildMonoid.profile
  noStripping = getAny buildMonoid.noStrip
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

-- | Interprets HaddockOptsMonoid options.
haddockOptsFromMonoid :: HaddockOptsMonoid -> HaddockOpts
haddockOptsFromMonoid hoMonoid = defaultHaddockOpts
  { HaddockOpts.additionalArgs = hoMonoid.additionalArgs }

-- | Interprets TestOptsMonoid options.
testOptsFromMonoid :: TestOptsMonoid -> Maybe [String] -> TestOpts
testOptsFromMonoid toMonoid madditional = defaultTestOpts
  { TestOpts.rerunTests = fromFirstTrue toMonoid.rerunTests
  , TestOpts.additionalArgs =
      fromMaybe [] madditional <> toMonoid.additionalArgs
  , TestOpts.coverage = fromFirstFalse toMonoid.coverage
  , TestOpts.runTests = fromFirstTrue toMonoid.runTests
  , TestOpts.maximumTimeSeconds =
      fromFirst
        defaultTestOpts.maximumTimeSeconds
        toMonoid.maximumTimeSeconds
  , TestOpts.allowStdin = fromFirstTrue toMonoid.allowStdin
  }

-- | Interprets BenchmarkOptsMonoid options.
benchmarkOptsFromMonoid ::
     BenchmarkOptsMonoid
  -> Maybe [String]
  -> BenchmarkOpts
benchmarkOptsFromMonoid beoMonoid madditional = defaultBenchmarkOpts
  { BenchmarkOpts.additionalArgs =
      fmap (\args -> unwords args <> " ") madditional <>
      getFirst beoMonoid.additionalArgs
  , BenchmarkOpts.runBenchmarks = fromFirstTrue beoMonoid.runBenchmarks
  }
