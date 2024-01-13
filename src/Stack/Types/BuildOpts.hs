{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Configuration options for building.
module Stack.Types.BuildOpts
  ( BuildOpts (..)
  , buildOptsHaddockL
  , buildOptsInstallExesL
  , BuildCommand (..)
  , defaultBuildOpts
  , defaultBuildOptsCLI
  , BuildOptsCLI (..)
  , boptsCLIAllProgOptions
  , BuildOptsMonoid (..)
  , ProgressBarFormat (..)
  , readProgressBarFormat
  , buildOptsMonoidBenchmarksL
  , buildOptsMonoidHaddockL
  , buildOptsMonoidInstallExesL
  , buildOptsMonoidTestsL
  , TestOpts (..)
  , defaultTestOpts
  , TestOptsMonoid (..)
  , HaddockOpts (..)
  , defaultHaddockOpts
  , HaddockOptsMonoid (..)
  , BenchmarkOpts (..)
  , defaultBenchmarkOpts
  , BenchmarkOptsMonoid (..)
  , FileWatchOpts (..)
  , BuildSubset (..)
  , ApplyCLIFlag (..)
  , boptsCLIFlagsByName
  , CabalVerbosity (..)
  , toFirstCabalVerbosity
  ) where

import           Data.Aeson.Types ( FromJSON (..), withText )
import           Data.Aeson.WarningParser
                   ( WithJSONWarnings, (..:?), (..!=), jsonSubWarnings
                   , withObjectWarnings
                   )
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Distribution.Parsec ( Parsec (..), simpleParsec )
import           Distribution.Verbosity ( Verbosity, normal, verbose )
import           Generics.Deriving.Monoid ( mappenddefault, memptydefault )
import           Stack.Prelude

-- | Build options that is interpreted by the build command. This is built up
-- from BuildOptsCLI and BuildOptsMonoid
data BuildOpts = BuildOpts
  { libProfile :: !Bool
  , exeProfile :: !Bool
  , libStrip :: !Bool
  , exeStrip :: !Bool
  , haddock :: !Bool
    -- ^ Build haddocks?
  , haddockOpts :: !HaddockOpts
    -- ^ Options to pass to haddock
  , openHaddocks :: !Bool
    -- ^ Open haddocks in the browser?
  , haddockDeps :: !(Maybe Bool)
    -- ^ Build haddocks for dependencies?
  , haddockInternal :: !Bool
    -- ^ Build haddocks for all symbols and packages, like
    -- @cabal haddock --internal@
  , haddockHyperlinkSource :: !Bool
    -- ^ Build hyperlinked source. Disable for no sources.
  , haddockForHackage :: !Bool
    -- ^ Build with flags to generate Haddock documentation suitable to upload
    -- to Hackage.
  , installExes :: !Bool
    -- ^ Install executables to user path after building?
  , installCompilerTool :: !Bool
    -- ^ Install executables to compiler tools path after building?
  , preFetch :: !Bool
    -- ^ Fetch all packages immediately
    -- ^ Watch files for changes and automatically rebuild
  , keepGoing :: !(Maybe Bool)
    -- ^ Keep building/running after failure
  , keepTmpFiles :: !Bool
    -- ^ Keep intermediate files and build directories
  , forceDirty :: !Bool
    -- ^ Force treating all local packages as having dirty files
  , tests :: !Bool
    -- ^ Turn on tests for local targets
  , testOpts :: !TestOpts
    -- ^ Additional test arguments
  , benchmarks :: !Bool
    -- ^ Turn on benchmarks for local targets
  , benchmarkOpts :: !BenchmarkOpts
    -- ^ Additional test arguments
    -- ^ Commands (with arguments) to run after a successful build
    -- ^ Only perform the configure step when building
  , reconfigure :: !Bool
    -- ^ Perform the configure step even if already configured
  , cabalVerbose :: !CabalVerbosity
    -- ^ Ask Cabal to be verbose in its builds
  , splitObjs :: !Bool
    -- ^ Whether to enable split-objs.
  , skipComponents :: ![Text]
    -- ^ Which components to skip when building
  , interleavedOutput :: !Bool
    -- ^ Should we use the interleaved GHC output when building
    -- multiple packages?
  , progressBar :: !ProgressBarFormat
    -- ^ Format of the progress bar
  , ddumpDir :: !(Maybe Text)
  }
  deriving Show

defaultBuildOpts :: BuildOpts
defaultBuildOpts = BuildOpts
  { libProfile = defaultFirstFalse buildMonoid.buildMonoidLibProfile
  , exeProfile = defaultFirstFalse buildMonoid.buildMonoidExeProfile
  , libStrip = defaultFirstTrue buildMonoid.buildMonoidLibStrip
  , exeStrip = defaultFirstTrue buildMonoid.buildMonoidExeStrip
  , haddock = False
  , haddockOpts = defaultHaddockOpts
  , openHaddocks = defaultFirstFalse buildMonoid.buildMonoidOpenHaddocks
  , haddockDeps = Nothing
  , haddockInternal =
      defaultFirstFalse buildMonoid.buildMonoidHaddockInternal
  , haddockHyperlinkSource =
      defaultFirstTrue buildMonoid.buildMonoidHaddockHyperlinkSource
  , haddockForHackage =
      defaultFirstFalse buildMonoid.buildMonoidHaddockForHackage
  , installExes = defaultFirstFalse buildMonoid.buildMonoidInstallExes
  , installCompilerTool =
      defaultFirstFalse buildMonoid.buildMonoidInstallCompilerTool
  , preFetch = defaultFirstFalse buildMonoid.buildMonoidPreFetch
  , keepGoing = Nothing
  , keepTmpFiles = defaultFirstFalse buildMonoid.buildMonoidKeepTmpFiles
  , forceDirty = defaultFirstFalse buildMonoid.buildMonoidForceDirty
  , tests = defaultFirstFalse buildMonoid.buildMonoidTests
  , testOpts = defaultTestOpts
  , benchmarks = defaultFirstFalse buildMonoid.buildMonoidBenchmarks
  , benchmarkOpts = defaultBenchmarkOpts
  , reconfigure = defaultFirstFalse buildMonoid.buildMonoidReconfigure
  , cabalVerbose = CabalVerbosity normal
  , splitObjs = defaultFirstFalse buildMonoid.buildMonoidSplitObjs
  , skipComponents = []
  , interleavedOutput =
      defaultFirstTrue buildMonoid.buildMonoidInterleavedOutput
  , progressBar = CappedBar
  , ddumpDir = Nothing
  }
 where
  buildMonoid = undefined :: BuildOptsMonoid

defaultBuildOptsCLI ::BuildOptsCLI
defaultBuildOptsCLI = BuildOptsCLI
  { boptsCLITargets = []
  , boptsCLIDryrun = False
  , boptsCLIFlags = Map.empty
  , boptsCLIGhcOptions = []
  , boptsCLIProgsOptions = []
  , boptsCLIBuildSubset = BSAll
  , boptsCLIFileWatch = NoFileWatch
  , boptsCLIWatchAll = False
  , boptsCLIExec = []
  , boptsCLIOnlyConfigure = False
  , boptsCLICommand = Build
  , boptsCLIInitialBuildSteps = False
  }

-- | How to apply a CLI flag
data ApplyCLIFlag
  = ACFAllProjectPackages
    -- ^ Apply to all project packages which have such a flag name available.
  | ACFByName !PackageName
    -- ^ Apply to the specified package only.
  deriving (Eq, Ord, Show)

-- | Only flags set via 'ACFByName'
boptsCLIFlagsByName :: BuildOptsCLI -> Map PackageName (Map FlagName Bool)
boptsCLIFlagsByName =
  Map.fromList .
  mapMaybe go .
  Map.toList .
  (.boptsCLIFlags)
 where
  go (ACFAllProjectPackages, _) = Nothing
  go (ACFByName name, flags) = Just (name, flags)

-- | Build options that may only be specified from the CLI
data BuildOptsCLI = BuildOptsCLI
  { boptsCLITargets :: ![Text]
  , boptsCLIDryrun :: !Bool
  , boptsCLIGhcOptions :: ![Text]
  , boptsCLIProgsOptions :: ![(Text, [Text])]
  , boptsCLIFlags :: !(Map ApplyCLIFlag (Map FlagName Bool))
  , boptsCLIBuildSubset :: !BuildSubset
  , boptsCLIFileWatch :: !FileWatchOpts
  , boptsCLIWatchAll :: !Bool
  , boptsCLIExec :: ![(String, [String])]
  , boptsCLIOnlyConfigure :: !Bool
  , boptsCLICommand :: !BuildCommand
  , boptsCLIInitialBuildSteps :: !Bool
  }
  deriving Show

-- | Generate a list of --PROG-option="<argument>" arguments for all PROGs.
boptsCLIAllProgOptions :: BuildOptsCLI -> [Text]
boptsCLIAllProgOptions boptsCLI =
  concatMap progOptionArgs boptsCLI.boptsCLIProgsOptions
 where
  -- Generate a list of --PROG-option="<argument>" arguments for a PROG.
  progOptionArgs :: (Text, [Text]) -> [Text]
  progOptionArgs (prog, opts) = map progOptionArg opts
   where
    -- Generate a --PROG-option="<argument>" argument for a PROG and option.
    progOptionArg :: Text -> Text
    progOptionArg opt = T.concat
      [ "--"
      , prog
      , "-option=\""
      , opt
      , "\""
      ]

-- | Command sum type for conditional arguments.
data BuildCommand
  = Build
  | Test
  | Haddock
  | Bench
  | Install
  deriving (Eq, Show)

-- | Build options that may be specified in the stack.yaml or from the CLI
data BuildOptsMonoid = BuildOptsMonoid
  { buildMonoidTrace :: !Any
  , buildMonoidProfile :: !Any
  , buildMonoidNoStrip :: !Any
  , buildMonoidLibProfile :: !FirstFalse
  , buildMonoidExeProfile :: !FirstFalse
  , buildMonoidLibStrip :: !FirstTrue
  , buildMonoidExeStrip :: !FirstTrue
  , buildMonoidHaddock :: !FirstFalse
  , buildMonoidHaddockOpts :: !HaddockOptsMonoid
  , buildMonoidOpenHaddocks :: !FirstFalse
  , buildMonoidHaddockDeps :: !(First Bool)
  , buildMonoidHaddockInternal :: !FirstFalse
  , buildMonoidHaddockHyperlinkSource :: !FirstTrue
  , buildMonoidHaddockForHackage :: !FirstFalse
  , buildMonoidInstallExes :: !FirstFalse
  , buildMonoidInstallCompilerTool :: !FirstFalse
  , buildMonoidPreFetch :: !FirstFalse
  , buildMonoidKeepGoing :: !(First Bool)
  , buildMonoidKeepTmpFiles :: !FirstFalse
  , buildMonoidForceDirty :: !FirstFalse
  , buildMonoidTests :: !FirstFalse
  , buildMonoidTestOpts :: !TestOptsMonoid
  , buildMonoidBenchmarks :: !FirstFalse
  , buildMonoidBenchmarkOpts :: !BenchmarkOptsMonoid
  , buildMonoidReconfigure :: !FirstFalse
  , buildMonoidCabalVerbose :: !(First CabalVerbosity)
  , buildMonoidSplitObjs :: !FirstFalse
  , buildMonoidSkipComponents :: ![Text]
  , buildMonoidInterleavedOutput :: !FirstTrue
  , buildMonoidProgressBar :: !(First ProgressBarFormat)
  , buildMonoidDdumpDir :: !(First Text)
  }
  deriving (Generic, Show)

instance FromJSON (WithJSONWarnings BuildOptsMonoid) where
  parseJSON = withObjectWarnings "BuildOptsMonoid" $ \o -> do
    let buildMonoidTrace = Any False
        buildMonoidProfile = Any False
        buildMonoidNoStrip = Any False
    buildMonoidLibProfile <- FirstFalse <$> o ..:? buildMonoidLibProfileArgName
    buildMonoidExeProfile <-FirstFalse <$>  o ..:? buildMonoidExeProfileArgName
    buildMonoidLibStrip <- FirstTrue <$> o ..:? buildMonoidLibStripArgName
    buildMonoidExeStrip <-FirstTrue <$>  o ..:? buildMonoidExeStripArgName
    buildMonoidHaddock <- FirstFalse <$> o ..:? buildMonoidHaddockArgName
    buildMonoidHaddockOpts <-
      jsonSubWarnings (o ..:? buildMonoidHaddockOptsArgName ..!= mempty)
    buildMonoidOpenHaddocks <-
      FirstFalse <$> o ..:? buildMonoidOpenHaddocksArgName
    buildMonoidHaddockDeps <- First <$> o ..:? buildMonoidHaddockDepsArgName
    buildMonoidHaddockInternal <-
      FirstFalse <$> o ..:? buildMonoidHaddockInternalArgName
    buildMonoidHaddockHyperlinkSource <-
      FirstTrue <$> o ..:? buildMonoidHaddockHyperlinkSourceArgName
    buildMonoidHaddockForHackage <-
      FirstFalse <$> o ..:? buildMonoidHaddockForHackageArgName
    buildMonoidInstallExes <-
      FirstFalse <$> o ..:? buildMonoidInstallExesArgName
    buildMonoidInstallCompilerTool <-
      FirstFalse <$> o ..:? buildMonoidInstallCompilerToolArgName
    buildMonoidPreFetch <- FirstFalse <$> o ..:? buildMonoidPreFetchArgName
    buildMonoidKeepGoing <- First <$> o ..:? buildMonoidKeepGoingArgName
    buildMonoidKeepTmpFiles <-
      FirstFalse <$> o ..:? buildMonoidKeepTmpFilesArgName
    buildMonoidForceDirty <- FirstFalse <$> o ..:? buildMonoidForceDirtyArgName
    buildMonoidTests <- FirstFalse <$> o ..:? buildMonoidTestsArgName
    buildMonoidTestOpts <-
      jsonSubWarnings (o ..:? buildMonoidTestOptsArgName ..!= mempty)
    buildMonoidBenchmarks <- FirstFalse <$> o ..:? buildMonoidBenchmarksArgName
    buildMonoidBenchmarkOpts <-
      jsonSubWarnings (o ..:? buildMonoidBenchmarkOptsArgName ..!= mempty)
    buildMonoidReconfigure <-
      FirstFalse <$> o ..:? buildMonoidReconfigureArgName
    cabalVerbosity <- First <$> o ..:? buildMonoidCabalVerbosityArgName
    cabalVerbose <- FirstFalse <$> o ..:? buildMonoidCabalVerboseArgName
    let buildMonoidCabalVerbose =
          cabalVerbosity <> toFirstCabalVerbosity cabalVerbose
    buildMonoidSplitObjs <- FirstFalse <$> o ..:? buildMonoidSplitObjsName
    buildMonoidSkipComponents <-
      o ..:? buildMonoidSkipComponentsName ..!= mempty
    buildMonoidInterleavedOutput <-
      FirstTrue <$> o ..:? buildMonoidInterleavedOutputName
    buildMonoidProgressBar <-
      First <$> o ..:? buildMonoidProgressBarName
    buildMonoidDdumpDir <- o ..:? buildMonoidDdumpDirName ..!= mempty
    pure $ BuildOptsMonoid
      { buildMonoidTrace
      , buildMonoidProfile
      , buildMonoidNoStrip
      , buildMonoidLibProfile
      , buildMonoidExeProfile
      , buildMonoidLibStrip
      , buildMonoidExeStrip
      , buildMonoidHaddock
      , buildMonoidHaddockOpts
      , buildMonoidOpenHaddocks
      , buildMonoidHaddockDeps
      , buildMonoidHaddockInternal
      , buildMonoidHaddockHyperlinkSource
      , buildMonoidHaddockForHackage
      , buildMonoidInstallExes
      , buildMonoidInstallCompilerTool
      , buildMonoidPreFetch
      , buildMonoidKeepGoing
      , buildMonoidKeepTmpFiles
      , buildMonoidForceDirty
      , buildMonoidTests
      , buildMonoidTestOpts
      , buildMonoidBenchmarks
      , buildMonoidBenchmarkOpts
      , buildMonoidReconfigure
      , buildMonoidCabalVerbose
      , buildMonoidSplitObjs
      , buildMonoidSkipComponents
      , buildMonoidInterleavedOutput
      , buildMonoidProgressBar
      , buildMonoidDdumpDir
      }

buildMonoidLibProfileArgName :: Text
buildMonoidLibProfileArgName = "library-profiling"

buildMonoidExeProfileArgName :: Text
buildMonoidExeProfileArgName = "executable-profiling"

buildMonoidLibStripArgName :: Text
buildMonoidLibStripArgName = "library-stripping"

buildMonoidExeStripArgName :: Text
buildMonoidExeStripArgName = "executable-stripping"

buildMonoidHaddockArgName :: Text
buildMonoidHaddockArgName = "haddock"

buildMonoidHaddockOptsArgName :: Text
buildMonoidHaddockOptsArgName = "haddock-arguments"

buildMonoidOpenHaddocksArgName :: Text
buildMonoidOpenHaddocksArgName = "open-haddocks"

buildMonoidHaddockDepsArgName :: Text
buildMonoidHaddockDepsArgName = "haddock-deps"

buildMonoidHaddockInternalArgName :: Text
buildMonoidHaddockInternalArgName = "haddock-internal"

buildMonoidHaddockHyperlinkSourceArgName :: Text
buildMonoidHaddockHyperlinkSourceArgName = "haddock-hyperlink-source"

buildMonoidHaddockForHackageArgName :: Text
buildMonoidHaddockForHackageArgName = "haddock-for-hackage"

buildMonoidInstallExesArgName :: Text
buildMonoidInstallExesArgName = "copy-bins"

buildMonoidInstallCompilerToolArgName :: Text
buildMonoidInstallCompilerToolArgName = "copy-compiler-tool"

buildMonoidPreFetchArgName :: Text
buildMonoidPreFetchArgName = "prefetch"

buildMonoidKeepGoingArgName :: Text
buildMonoidKeepGoingArgName = "keep-going"

buildMonoidKeepTmpFilesArgName :: Text
buildMonoidKeepTmpFilesArgName = "keep-tmp-files"

buildMonoidForceDirtyArgName :: Text
buildMonoidForceDirtyArgName = "force-dirty"

buildMonoidTestsArgName :: Text
buildMonoidTestsArgName = "test"

buildMonoidTestOptsArgName :: Text
buildMonoidTestOptsArgName = "test-arguments"

buildMonoidBenchmarksArgName :: Text
buildMonoidBenchmarksArgName = "bench"

buildMonoidBenchmarkOptsArgName :: Text
buildMonoidBenchmarkOptsArgName = "benchmark-opts"

buildMonoidReconfigureArgName :: Text
buildMonoidReconfigureArgName = "reconfigure"

buildMonoidCabalVerbosityArgName :: Text
buildMonoidCabalVerbosityArgName = "cabal-verbosity"

buildMonoidCabalVerboseArgName :: Text
buildMonoidCabalVerboseArgName = "cabal-verbose"

buildMonoidSplitObjsName :: Text
buildMonoidSplitObjsName = "split-objs"

buildMonoidSkipComponentsName :: Text
buildMonoidSkipComponentsName = "skip-components"

buildMonoidInterleavedOutputName :: Text
buildMonoidInterleavedOutputName = "interleaved-output"

buildMonoidProgressBarName :: Text
buildMonoidProgressBarName = "progress-bar"

buildMonoidDdumpDirName :: Text
buildMonoidDdumpDirName = "ddump-dir"

instance Semigroup BuildOptsMonoid where
  (<>) = mappenddefault

instance Monoid BuildOptsMonoid where
  mempty = memptydefault
  mappend = (<>)

-- | Which subset of packages to build
data BuildSubset
  = BSAll
  | BSOnlySnapshot
    -- ^ Only install packages in the snapshot database, skipping
    -- packages intended for the local database.
  | BSOnlyDependencies
  | BSOnlyLocals
    -- ^ Refuse to build anything in the snapshot database, see
    -- https://github.com/commercialhaskell/stack/issues/5272
  deriving (Show, Eq)

-- | Options for the 'FinalAction' 'DoTests'
data TestOpts = TestOpts
  { toRerunTests :: !Bool -- ^ Whether successful tests will be run gain
  , toAdditionalArgs :: ![String] -- ^ Arguments passed to the test program
  , toCoverage :: !Bool -- ^ Generate a code coverage report
  , toDisableRun :: !Bool -- ^ Disable running of tests
  , toMaximumTimeSeconds :: !(Maybe Int) -- ^ test suite timeout in seconds
  , toAllowStdin :: !Bool -- ^ Whether to allow standard input
  }
  deriving (Eq, Show)

defaultTestOpts :: TestOpts
defaultTestOpts = TestOpts
  { toRerunTests = defaultFirstTrue toMonoid.toMonoidRerunTests
  , toAdditionalArgs = []
  , toCoverage = defaultFirstFalse toMonoid.toMonoidCoverage
  , toDisableRun = defaultFirstFalse toMonoid.toMonoidDisableRun
  , toMaximumTimeSeconds = Nothing
  , toAllowStdin = defaultFirstTrue toMonoid.toMonoidAllowStdin
  }
 where
  toMonoid = undefined :: TestOptsMonoid

data TestOptsMonoid = TestOptsMonoid
  { toMonoidRerunTests :: !FirstTrue
  , toMonoidAdditionalArgs :: ![String]
  , toMonoidCoverage :: !FirstFalse
  , toMonoidDisableRun :: !FirstFalse
  , toMonoidMaximumTimeSeconds :: !(First (Maybe Int))
  , toMonoidAllowStdin :: !FirstTrue
  }
  deriving (Show, Generic)

instance FromJSON (WithJSONWarnings TestOptsMonoid) where
  parseJSON = withObjectWarnings "TestOptsMonoid" $ \o -> do
    toMonoidRerunTests <- FirstTrue <$> o ..:? toMonoidRerunTestsArgName
    toMonoidAdditionalArgs <- o ..:? toMonoidAdditionalArgsName ..!= []
    toMonoidCoverage <- FirstFalse <$> o ..:? toMonoidCoverageArgName
    toMonoidDisableRun <- FirstFalse <$> o ..:? toMonoidDisableRunArgName
    toMonoidMaximumTimeSeconds <-
      First <$> o ..:? toMonoidMaximumTimeSecondsArgName
    toMonoidAllowStdin <- FirstTrue <$> o ..:? toMonoidTestsAllowStdinName
    pure $ TestOptsMonoid
      { toMonoidRerunTests
      , toMonoidAdditionalArgs
      , toMonoidCoverage
      , toMonoidDisableRun
      , toMonoidMaximumTimeSeconds
      , toMonoidAllowStdin
      }

toMonoidRerunTestsArgName :: Text
toMonoidRerunTestsArgName = "rerun-tests"

toMonoidAdditionalArgsName :: Text
toMonoidAdditionalArgsName = "additional-args"

toMonoidCoverageArgName :: Text
toMonoidCoverageArgName = "coverage"

toMonoidDisableRunArgName :: Text
toMonoidDisableRunArgName = "no-run-tests"

toMonoidMaximumTimeSecondsArgName :: Text
toMonoidMaximumTimeSecondsArgName = "test-suite-timeout"

toMonoidTestsAllowStdinName :: Text
toMonoidTestsAllowStdinName = "tests-allow-stdin"

instance Semigroup TestOptsMonoid where
  (<>) = mappenddefault

instance Monoid TestOptsMonoid where
  mempty = memptydefault
  mappend = (<>)

-- | Haddock Options
newtype HaddockOpts = HaddockOpts
  { hoAdditionalArgs :: [String] -- ^ Arguments passed to haddock program
  }
  deriving (Eq, Show)

newtype HaddockOptsMonoid = HaddockOptsMonoid
  { hoMonoidAdditionalArgs :: [String]
  }
  deriving (Generic, Show)

defaultHaddockOpts :: HaddockOpts
defaultHaddockOpts = HaddockOpts {hoAdditionalArgs = []}

instance FromJSON (WithJSONWarnings HaddockOptsMonoid) where
  parseJSON = withObjectWarnings "HaddockOptsMonoid" $ \o -> do
    hoMonoidAdditionalArgs <- o ..:? hoMonoidAdditionalArgsName ..!= []
    pure $ HaddockOptsMonoid { hoMonoidAdditionalArgs }

instance Semigroup HaddockOptsMonoid where
  (<>) = mappenddefault

instance Monoid HaddockOptsMonoid where
  mempty = memptydefault
  mappend = (<>)

hoMonoidAdditionalArgsName :: Text
hoMonoidAdditionalArgsName = "haddock-args"

-- | Options for the 'FinalAction' 'DoBenchmarks'
data BenchmarkOpts = BenchmarkOpts
  { beoAdditionalArgs :: !(Maybe String)
    -- ^ Arguments passed to the benchmark program
  , beoDisableRun :: !Bool
    -- ^ Disable running of benchmarks
  }
  deriving (Eq, Show)

defaultBenchmarkOpts :: BenchmarkOpts
defaultBenchmarkOpts = BenchmarkOpts
  { beoAdditionalArgs = Nothing
  , beoDisableRun = False
  }

data BenchmarkOptsMonoid = BenchmarkOptsMonoid
  { beoMonoidAdditionalArgs :: !(First String)
  , beoMonoidDisableRun :: !(First Bool)
  }
  deriving (Generic, Show)

instance FromJSON (WithJSONWarnings BenchmarkOptsMonoid) where
  parseJSON = withObjectWarnings "BenchmarkOptsMonoid" $ \o -> do
    beoMonoidAdditionalArgs <- First <$> o ..:? beoMonoidAdditionalArgsArgName
    beoMonoidDisableRun <- First <$> o ..:? beoMonoidDisableRunArgName
    pure $ BenchmarkOptsMonoid
      { beoMonoidAdditionalArgs
      , beoMonoidDisableRun
      }

beoMonoidAdditionalArgsArgName :: Text
beoMonoidAdditionalArgsArgName = "benchmark-arguments"

beoMonoidDisableRunArgName :: Text
beoMonoidDisableRunArgName = "no-run-benchmarks"

instance Semigroup BenchmarkOptsMonoid where
  (<>) = mappenddefault

instance Monoid BenchmarkOptsMonoid where
  mempty = memptydefault
  mappend = (<>)

data FileWatchOpts
  = NoFileWatch
  | FileWatch
  | FileWatchPoll
  deriving (Show, Eq)

newtype CabalVerbosity
  = CabalVerbosity Verbosity
  deriving (Eq, Show)

toFirstCabalVerbosity :: FirstFalse -> First CabalVerbosity
toFirstCabalVerbosity vf = First $ vf.getFirstFalse <&> \p ->
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
  lens (.buildMonoidHaddock.getFirstFalse)
    (\buildMonoid t -> buildMonoid {buildMonoidHaddock = FirstFalse t})

buildOptsMonoidTestsL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidTestsL =
  lens (.buildMonoidTests.getFirstFalse)
    (\buildMonoid t -> buildMonoid {buildMonoidTests = FirstFalse t})

buildOptsMonoidBenchmarksL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidBenchmarksL =
  lens (.buildMonoidBenchmarks.getFirstFalse)
    (\buildMonoid t -> buildMonoid {buildMonoidBenchmarks = FirstFalse t})

buildOptsMonoidInstallExesL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidInstallExesL =
  lens (.buildMonoidInstallExes.getFirstFalse)
    (\buildMonoid t -> buildMonoid {buildMonoidInstallExes = FirstFalse t})

buildOptsInstallExesL :: Lens' BuildOpts Bool
buildOptsInstallExesL = lens
  (.installExes)
  (\bopts t -> bopts {installExes = t})

buildOptsHaddockL :: Lens' BuildOpts Bool
buildOptsHaddockL = lens (.haddock) (\bopts t -> bopts {haddock = t})

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
