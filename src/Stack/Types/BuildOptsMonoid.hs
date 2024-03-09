{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Configuration options for building from the command line and/or a
-- configuration file.
module Stack.Types.BuildOptsMonoid
  ( BuildOptsMonoid (..)
  , HaddockOptsMonoid (..)
  , TestOptsMonoid (..)
  , BenchmarkOptsMonoid (..)
  , CabalVerbosity (..)
  , ProgressBarFormat (..)
  , buildOptsMonoidHaddockL
  , buildOptsMonoidTestsL
  , buildOptsMonoidBenchmarksL
  , buildOptsMonoidInstallExesL
  , toFirstCabalVerbosity
  , readProgressBarFormat
  ) where

import           Data.Aeson.Types ( FromJSON (..), withText )
import           Data.Aeson.WarningParser
                   ( WithJSONWarnings, (..:?), (..!=), jsonSubWarnings
                   , withObjectWarnings
                   )
import qualified Data.Text as T
import           Distribution.Parsec ( Parsec (..), simpleParsec )
import           Distribution.Verbosity ( Verbosity, normal, verbose )
import           Generics.Deriving.Monoid ( mappenddefault, memptydefault )
import           Stack.Prelude hiding ( trace )

-- | Build options that may be specified in the stack.yaml or from the CLI
data BuildOptsMonoid = BuildOptsMonoid
  { trace :: !Any
  , profile :: !Any
  , noStrip :: !Any
  , libProfile :: !FirstFalse
  , exeProfile :: !FirstFalse
  , libStrip :: !FirstTrue
  , exeStrip :: !FirstTrue
  , buildHaddocks :: !FirstFalse
  , haddockOpts :: !HaddockOptsMonoid
  , openHaddocks :: !FirstFalse
  , haddockDeps :: !(First Bool)
  , haddockExecutables :: !FirstFalse
  , haddockTests :: !FirstFalse
  , haddockBenchmarks :: !FirstFalse
  , haddockInternal :: !FirstFalse
  , haddockHyperlinkSource :: !FirstTrue
  , haddockForHackage :: !FirstFalse
  , installExes :: !FirstFalse
  , installCompilerTool :: !FirstFalse
  , preFetch :: !FirstFalse
  , keepGoing :: !(First Bool)
  , keepTmpFiles :: !FirstFalse
  , forceDirty :: !FirstFalse
  , tests :: !FirstFalse
  , testOpts :: !TestOptsMonoid
  , benchmarks :: !FirstFalse
  , benchmarkOpts :: !BenchmarkOptsMonoid
  , reconfigure :: !FirstFalse
  , cabalVerbose :: !(First CabalVerbosity)
  , splitObjs :: !FirstFalse
  , skipComponents :: ![Text]
  , interleavedOutput :: !FirstTrue
  , progressBar :: !(First ProgressBarFormat)
  , ddumpDir :: !(First Text)
  }
  deriving (Generic, Show)

instance FromJSON (WithJSONWarnings BuildOptsMonoid) where
  parseJSON = withObjectWarnings "BuildOptsMonoid" $ \o -> do
    let trace = Any False
        profile = Any False
        noStrip = Any False
    libProfile <- FirstFalse <$> o ..:? libProfileArgName
    exeProfile <-FirstFalse <$>  o ..:? exeProfileArgName
    libStrip <- FirstTrue <$> o ..:? libStripArgName
    exeStrip <-FirstTrue <$>  o ..:? exeStripArgName
    buildHaddocks <- FirstFalse <$> o ..:? haddockArgName
    haddockOpts <- jsonSubWarnings (o ..:? haddockOptsArgName ..!= mempty)
    openHaddocks <- FirstFalse <$> o ..:? openHaddocksArgName
    haddockDeps <- First <$> o ..:? haddockDepsArgName
    haddockExecutables <- FirstFalse <$> o ..:? haddockExecutablesArgName
    haddockTests <- FirstFalse <$> o ..:? haddockTestsArgName
    haddockBenchmarks <- FirstFalse <$> o ..:? haddockBenchmarksArgName
    haddockInternal <- FirstFalse <$> o ..:? haddockInternalArgName
    haddockHyperlinkSource <- FirstTrue <$> o ..:? haddockHyperlinkSourceArgName
    haddockForHackage <-  FirstFalse <$> o ..:? haddockForHackageArgName
    installExes <- FirstFalse <$> o ..:? installExesArgName
    installCompilerTool <- FirstFalse <$> o ..:? installCompilerToolArgName
    preFetch <- FirstFalse <$> o ..:? preFetchArgName
    keepGoing <- First <$> o ..:? keepGoingArgName
    keepTmpFiles <- FirstFalse <$> o ..:? keepTmpFilesArgName
    forceDirty <- FirstFalse <$> o ..:? forceDirtyArgName
    tests <- FirstFalse <$> o ..:? testsArgName
    testOpts <- jsonSubWarnings (o ..:? testOptsArgName ..!= mempty)
    benchmarks <- FirstFalse <$> o ..:? benchmarksArgName
    benchmarkOpts <- jsonSubWarnings (o ..:? benchmarkOptsArgName ..!= mempty)
    reconfigure <- FirstFalse <$> o ..:? reconfigureArgName
    cabalVerbosity <- First <$> o ..:? cabalVerbosityArgName
    cabalVerbose' <- FirstFalse <$> o ..:? cabalVerboseArgName
    let cabalVerbose = cabalVerbosity <> toFirstCabalVerbosity cabalVerbose'
    splitObjs <- FirstFalse <$> o ..:? splitObjsName
    skipComponents <- o ..:? skipComponentsName ..!= mempty
    interleavedOutput <- FirstTrue <$> o ..:? interleavedOutputName
    progressBar <- First <$> o ..:? progressBarName
    ddumpDir <- o ..:? ddumpDirName ..!= mempty
    pure BuildOptsMonoid
      { trace
      , profile
      , noStrip
      , libProfile
      , exeProfile
      , libStrip
      , exeStrip
      , buildHaddocks
      , haddockOpts
      , openHaddocks
      , haddockDeps
      , haddockExecutables
      , haddockTests
      , haddockBenchmarks
      , haddockInternal
      , haddockHyperlinkSource
      , haddockForHackage
      , installExes
      , installCompilerTool
      , preFetch
      , keepGoing
      , keepTmpFiles
      , forceDirty
      , tests
      , testOpts
      , benchmarks
      , benchmarkOpts
      , reconfigure
      , cabalVerbose
      , splitObjs
      , skipComponents
      , interleavedOutput
      , progressBar
      , ddumpDir
      }

libProfileArgName :: Text
libProfileArgName = "library-profiling"

exeProfileArgName :: Text
exeProfileArgName = "executable-profiling"

libStripArgName :: Text
libStripArgName = "library-stripping"

exeStripArgName :: Text
exeStripArgName = "executable-stripping"

haddockArgName :: Text
haddockArgName = "haddock"

haddockOptsArgName :: Text
haddockOptsArgName = "haddock-arguments"

openHaddocksArgName :: Text
openHaddocksArgName = "open-haddocks"

haddockDepsArgName :: Text
haddockDepsArgName = "haddock-deps"

haddockExecutablesArgName :: Text
haddockExecutablesArgName = "haddock-executables"

haddockTestsArgName :: Text
haddockTestsArgName = "haddock-tests"

haddockBenchmarksArgName :: Text
haddockBenchmarksArgName = "haddock-benchmarks"

haddockInternalArgName :: Text
haddockInternalArgName = "haddock-internal"

haddockHyperlinkSourceArgName :: Text
haddockHyperlinkSourceArgName = "haddock-hyperlink-source"

haddockForHackageArgName :: Text
haddockForHackageArgName = "haddock-for-hackage"

installExesArgName :: Text
installExesArgName = "copy-bins"

installCompilerToolArgName :: Text
installCompilerToolArgName = "copy-compiler-tool"

preFetchArgName :: Text
preFetchArgName = "prefetch"

keepGoingArgName :: Text
keepGoingArgName = "keep-going"

keepTmpFilesArgName :: Text
keepTmpFilesArgName = "keep-tmp-files"

forceDirtyArgName :: Text
forceDirtyArgName = "force-dirty"

testsArgName :: Text
testsArgName = "test"

testOptsArgName :: Text
testOptsArgName = "test-arguments"

benchmarksArgName :: Text
benchmarksArgName = "bench"

benchmarkOptsArgName :: Text
benchmarkOptsArgName = "benchmark-opts"

reconfigureArgName :: Text
reconfigureArgName = "reconfigure"

cabalVerbosityArgName :: Text
cabalVerbosityArgName = "cabal-verbosity"

cabalVerboseArgName :: Text
cabalVerboseArgName = "cabal-verbose"

splitObjsName :: Text
splitObjsName = "split-objs"

skipComponentsName :: Text
skipComponentsName = "skip-components"

interleavedOutputName :: Text
interleavedOutputName = "interleaved-output"

progressBarName :: Text
progressBarName = "progress-bar"

ddumpDirName :: Text
ddumpDirName = "ddump-dir"

instance Semigroup BuildOptsMonoid where
  (<>) = mappenddefault

instance Monoid BuildOptsMonoid where
  mempty = memptydefault
  mappend = (<>)

data TestOptsMonoid = TestOptsMonoid
  { rerunTests :: !FirstTrue
  , additionalArgs :: ![String]
  , coverage :: !FirstFalse
  , disableRun :: !FirstFalse
  , maximumTimeSeconds :: !(First (Maybe Int))
  , allowStdin :: !FirstTrue
  }
  deriving (Show, Generic)

instance FromJSON (WithJSONWarnings TestOptsMonoid) where
  parseJSON = withObjectWarnings "TestOptsMonoid" $ \o -> do
    rerunTests <- FirstTrue <$> o ..:? rerunTestsArgName
    additionalArgs <- o ..:? testAdditionalArgsName ..!= []
    coverage <- FirstFalse <$> o ..:? coverageArgName
    disableRun <- FirstFalse <$> o ..:? testDisableRunArgName
    maximumTimeSeconds <- First <$> o ..:? maximumTimeSecondsArgName
    allowStdin <- FirstTrue <$> o ..:? testsAllowStdinName
    pure TestOptsMonoid
      { rerunTests
      , additionalArgs
      , coverage
      , disableRun
      , maximumTimeSeconds
      , allowStdin
      }

rerunTestsArgName :: Text
rerunTestsArgName = "rerun-tests"

testAdditionalArgsName :: Text
testAdditionalArgsName = "additional-args"

coverageArgName :: Text
coverageArgName = "coverage"

testDisableRunArgName :: Text
testDisableRunArgName = "no-run-tests"

maximumTimeSecondsArgName :: Text
maximumTimeSecondsArgName = "test-suite-timeout"

testsAllowStdinName :: Text
testsAllowStdinName = "tests-allow-stdin"

instance Semigroup TestOptsMonoid where
  (<>) = mappenddefault

instance Monoid TestOptsMonoid where
  mempty = memptydefault
  mappend = (<>)

newtype HaddockOptsMonoid = HaddockOptsMonoid
  { additionalArgs :: [String]
  }
  deriving (Generic, Show)

instance FromJSON (WithJSONWarnings HaddockOptsMonoid) where
  parseJSON = withObjectWarnings "HaddockOptsMonoid" $ \o -> do
    additionalArgs <- o ..:? haddockAdditionalArgsName ..!= []
    pure HaddockOptsMonoid { additionalArgs }

instance Semigroup HaddockOptsMonoid where
  (<>) = mappenddefault

instance Monoid HaddockOptsMonoid where
  mempty = memptydefault
  mappend = (<>)

haddockAdditionalArgsName :: Text
haddockAdditionalArgsName = "haddock-args"

data BenchmarkOptsMonoid = BenchmarkOptsMonoid
  { additionalArgs :: !(First String)
  , disableRun :: !(First Bool)
  }
  deriving (Generic, Show)

instance FromJSON (WithJSONWarnings BenchmarkOptsMonoid) where
  parseJSON = withObjectWarnings "BenchmarkOptsMonoid" $ \o -> do
    additionalArgs <- First <$> o ..:? benchmarkAdditionalArgsName
    disableRun <- First <$> o ..:? benchmarkDisableRunArgName
    pure BenchmarkOptsMonoid
      { additionalArgs
      , disableRun
      }

benchmarkAdditionalArgsName :: Text
benchmarkAdditionalArgsName = "benchmark-arguments"

benchmarkDisableRunArgName :: Text
benchmarkDisableRunArgName = "no-run-benchmarks"

instance Semigroup BenchmarkOptsMonoid where
  (<>) = mappenddefault

instance Monoid BenchmarkOptsMonoid where
  mempty = memptydefault
  mappend :: BenchmarkOptsMonoid -> BenchmarkOptsMonoid -> BenchmarkOptsMonoid
  mappend = (<>)

newtype CabalVerbosity
  = CabalVerbosity Verbosity
  deriving (Eq, Show)

toFirstCabalVerbosity :: FirstFalse -> First CabalVerbosity
toFirstCabalVerbosity vf = First $ vf.firstFalse <&> \p ->
  if p then verboseLevel else normalLevel
 where
  verboseLevel = CabalVerbosity verbose
  normalLevel  = CabalVerbosity normal

instance FromJSON CabalVerbosity where

  parseJSON = withText "CabalVerbosity" $ \t ->
    let s = T.unpack t
        errMsg = fail $ "Unrecognised Cabal verbosity: " ++ s
    in  maybe errMsg pure (simpleParsec s)

instance Parsec CabalVerbosity where
  parsec = CabalVerbosity <$> parsec

buildOptsMonoidHaddockL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidHaddockL =
  lens (.buildHaddocks.firstFalse)
    (\buildMonoid t -> buildMonoid {buildHaddocks = FirstFalse t})

buildOptsMonoidTestsL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidTestsL =
  lens (.tests.firstFalse)
    (\buildMonoid t -> buildMonoid {tests = FirstFalse t})

buildOptsMonoidBenchmarksL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidBenchmarksL =
  lens (.benchmarks.firstFalse)
    (\buildMonoid t -> buildMonoid {benchmarks = FirstFalse t})

buildOptsMonoidInstallExesL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidInstallExesL =
  lens (.installExes.firstFalse)
    (\buildMonoid t -> buildMonoid {installExes = FirstFalse t})

-- Type representing formats of Stack's progress bar when building.
data ProgressBarFormat
  = NoBar -- No progress bar at all.
  | CountOnlyBar -- A bar that only counts packages.
  | CappedBar -- A bar capped at a length equivalent to the terminal's width.
  | FullBar -- A full progress bar.
  deriving (Eq, Show)

instance FromJSON ProgressBarFormat where
  parseJSON = withText "ProgressBarFormat" $ \t -> either
    fail
    pure
    (readProgressBarFormat $ T.unpack t)

-- | Parse ProgressBarFormat from a String.
readProgressBarFormat :: String -> Either String ProgressBarFormat
readProgressBarFormat s
  | s == "none" = pure NoBar
  | s == "count-only" = pure CountOnlyBar
  | s == "capped" = pure CappedBar
  | s == "full" = pure FullBar
  | otherwise = Left $ "Invalid progress bar format: " ++ s
