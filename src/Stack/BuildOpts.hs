{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

-- | Default configuration options for building.
module Stack.BuildOpts
  ( defaultBuildOpts
  , defaultTestOpts
  , defaultHaddockOpts
  , defaultBenchmarkOpts
  ) where

import           Distribution.Verbosity ( normal )
import           Stack.Prelude
import           Stack.Types.BuildOpts
                   ( BenchmarkOpts (..), BuildOpts (..), HaddockOpts (..)
                   , TestOpts (..)
                   )
import           Stack.Types.BuildOptsMonoid
                   ( BuildOptsMonoid (..), CabalVerbosity (..)
                   , ProgressBarFormat (..), TestOptsMonoid (..)
                   )

defaultBuildOpts :: BuildOpts
defaultBuildOpts = BuildOpts
  { libProfile = defaultFirstFalse buildMonoid.libProfile
  , exeProfile = defaultFirstFalse buildMonoid.exeProfile
  , libStrip = defaultFirstTrue buildMonoid.libStrip
  , exeStrip = defaultFirstTrue buildMonoid.exeStrip
  , haddock = False
  , haddockOpts = defaultHaddockOpts
  , openHaddocks = defaultFirstFalse buildMonoid.openHaddocks
  , haddockDeps = Nothing
  , haddockInternal = defaultFirstFalse buildMonoid.haddockInternal
  , haddockHyperlinkSource = defaultFirstTrue buildMonoid.haddockHyperlinkSource
  , haddockForHackage = defaultFirstFalse buildMonoid.haddockForHackage
  , installExes = defaultFirstFalse buildMonoid.installExes
  , installCompilerTool = defaultFirstFalse buildMonoid.installCompilerTool
  , preFetch = defaultFirstFalse buildMonoid.preFetch
  , keepGoing = Nothing
  , keepTmpFiles = defaultFirstFalse buildMonoid.keepTmpFiles
  , forceDirty = defaultFirstFalse buildMonoid.forceDirty
  , tests = defaultFirstFalse buildMonoid.tests
  , testOpts = defaultTestOpts
  , benchmarks = defaultFirstFalse buildMonoid.benchmarks
  , benchmarkOpts = defaultBenchmarkOpts
  , reconfigure = defaultFirstFalse buildMonoid.reconfigure
  , cabalVerbose = CabalVerbosity normal
  , splitObjs = defaultFirstFalse buildMonoid.splitObjs
  , skipComponents = []
  , interleavedOutput = defaultFirstTrue buildMonoid.interleavedOutput
  , progressBar = CappedBar
  , ddumpDir = Nothing
  }
 where
  buildMonoid = undefined :: BuildOptsMonoid

defaultTestOpts :: TestOpts
defaultTestOpts = TestOpts
  { rerunTests = defaultFirstTrue toMonoid.rerunTests
  , additionalArgs = []
  , coverage = defaultFirstFalse toMonoid.coverage
  , disableRun = defaultFirstFalse toMonoid.disableRun
  , maximumTimeSeconds = Nothing
  , allowStdin = defaultFirstTrue toMonoid.allowStdin
  }
 where
  toMonoid = undefined :: TestOptsMonoid

defaultHaddockOpts :: HaddockOpts
defaultHaddockOpts = HaddockOpts { additionalArgs = [] }

defaultBenchmarkOpts :: BenchmarkOpts
defaultBenchmarkOpts = BenchmarkOpts
  { additionalArgs = Nothing
  , disableRun = False
  }
