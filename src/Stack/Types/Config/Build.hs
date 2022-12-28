{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Configuration options for building.

module Stack.Types.Config.Build
    (
      BuildOpts (..)
    , BuildCommand (..)
    , defaultBuildOpts
    , defaultBuildOptsCLI
    , BuildOptsCLI (..)
    , BuildOptsMonoid (..)
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
    )
    where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Distribution.Parsec ( Parsec (..), simpleParsec )
import           Distribution.Verbosity ( Verbosity, normal, verbose )
import           Generics.Deriving.Monoid ( memptydefault, mappenddefault )
import           Pantry.Internal.AesonExtended
import           Stack.Prelude

-- | Build options that is interpreted by the build command.
--   This is built up from BuildOptsCLI and BuildOptsMonoid
data BuildOpts =
  BuildOpts {boptsLibProfile :: !Bool
            ,boptsExeProfile :: !Bool
            ,boptsLibStrip :: !Bool
            ,boptsExeStrip :: !Bool
            ,boptsHaddock :: !Bool
            -- ^ Build haddocks?
            ,boptsHaddockOpts :: !HaddockOpts
            -- ^ Options to pass to haddock
            ,boptsOpenHaddocks :: !Bool
            -- ^ Open haddocks in the browser?
            ,boptsHaddockDeps :: !(Maybe Bool)
            -- ^ Build haddocks for dependencies?
            ,boptsHaddockInternal :: !Bool
            -- ^ Build haddocks for all symbols and packages, like @cabal haddock --internal@
            ,boptsHaddockHyperlinkSource :: !Bool
            -- ^ Build hyperlinked source if possible. Fallback to
            -- @hscolour@. Disable for no sources.
            ,boptsInstallExes :: !Bool
            -- ^ Install executables to user path after building?
            ,boptsInstallCompilerTool :: !Bool
            -- ^ Install executables to compiler tools path after building?
            ,boptsPreFetch :: !Bool
            -- ^ Fetch all packages immediately
            -- ^ Watch files for changes and automatically rebuild
            ,boptsKeepGoing :: !(Maybe Bool)
            -- ^ Keep building/running after failure
            ,boptsKeepTmpFiles :: !Bool
            -- ^ Keep intermediate files and build directories
            ,boptsForceDirty :: !Bool
            -- ^ Force treating all local packages as having dirty files

            ,boptsTests :: !Bool
            -- ^ Turn on tests for local targets
            ,boptsTestOpts :: !TestOpts
            -- ^ Additional test arguments

            ,boptsBenchmarks :: !Bool
            -- ^ Turn on benchmarks for local targets
            ,boptsBenchmarkOpts :: !BenchmarkOpts
            -- ^ Additional test arguments
            -- ^ Commands (with arguments) to run after a successful build
            -- ^ Only perform the configure step when building
            ,boptsReconfigure :: !Bool
            -- ^ Perform the configure step even if already configured
            ,boptsCabalVerbose :: !CabalVerbosity
            -- ^ Ask Cabal to be verbose in its builds
            ,boptsSplitObjs :: !Bool
            -- ^ Whether to enable split-objs.
            ,boptsSkipComponents :: ![Text]
            -- ^ Which components to skip when building
            ,boptsInterleavedOutput :: !Bool
            -- ^ Should we use the interleaved GHC output when building
            -- multiple packages?
            ,boptsDdumpDir :: !(Maybe Text)
            }
  deriving Show

defaultBuildOpts :: BuildOpts
defaultBuildOpts = BuildOpts
    { boptsLibProfile = defaultFirstFalse buildMonoidLibProfile
    , boptsExeProfile = defaultFirstFalse buildMonoidExeProfile
    , boptsLibStrip = defaultFirstTrue buildMonoidLibStrip
    , boptsExeStrip = defaultFirstTrue buildMonoidExeStrip
    , boptsHaddock = False
    , boptsHaddockOpts = defaultHaddockOpts
    , boptsOpenHaddocks = defaultFirstFalse buildMonoidOpenHaddocks
    , boptsHaddockDeps = Nothing
    , boptsHaddockInternal = defaultFirstFalse buildMonoidHaddockInternal
    , boptsHaddockHyperlinkSource = defaultFirstTrue buildMonoidHaddockHyperlinkSource
    , boptsInstallExes = defaultFirstFalse buildMonoidInstallExes
    , boptsInstallCompilerTool = defaultFirstFalse buildMonoidInstallCompilerTool
    , boptsPreFetch = defaultFirstFalse buildMonoidPreFetch
    , boptsKeepGoing = Nothing
    , boptsKeepTmpFiles = defaultFirstFalse buildMonoidKeepTmpFiles
    , boptsForceDirty = defaultFirstFalse buildMonoidForceDirty
    , boptsTests = defaultFirstFalse buildMonoidTests
    , boptsTestOpts = defaultTestOpts
    , boptsBenchmarks = defaultFirstFalse buildMonoidBenchmarks
    , boptsBenchmarkOpts = defaultBenchmarkOpts
    , boptsReconfigure = defaultFirstFalse buildMonoidReconfigure
    , boptsCabalVerbose = CabalVerbosity normal
    , boptsSplitObjs = defaultFirstFalse buildMonoidSplitObjs
    , boptsSkipComponents = []
    , boptsInterleavedOutput = defaultFirstTrue buildMonoidInterleavedOutput
    , boptsDdumpDir = Nothing
    }

defaultBuildOptsCLI ::BuildOptsCLI
defaultBuildOptsCLI = BuildOptsCLI
    { boptsCLITargets = []
    , boptsCLIDryrun = False
    , boptsCLIFlags = Map.empty
    , boptsCLIGhcOptions = []
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
  deriving (Show, Eq, Ord)

-- | Only flags set via 'ACFByName'
boptsCLIFlagsByName :: BuildOptsCLI -> Map PackageName (Map FlagName Bool)
boptsCLIFlagsByName =
  Map.fromList .
  mapMaybe go .
  Map.toList .
  boptsCLIFlags
  where
    go (ACFAllProjectPackages, _) = Nothing
    go (ACFByName name, flags) = Just (name, flags)

-- | Build options that may only be specified from the CLI
data BuildOptsCLI = BuildOptsCLI
    { boptsCLITargets :: ![Text]
    , boptsCLIDryrun :: !Bool
    , boptsCLIGhcOptions :: ![Text]
    , boptsCLIFlags :: !(Map ApplyCLIFlag (Map FlagName Bool))
    , boptsCLIBuildSubset :: !BuildSubset
    , boptsCLIFileWatch :: !FileWatchOpts
    , boptsCLIWatchAll :: !Bool
    , boptsCLIExec :: ![(String, [String])]
    , boptsCLIOnlyConfigure :: !Bool
    , boptsCLICommand :: !BuildCommand
    , boptsCLIInitialBuildSteps :: !Bool
    } deriving Show

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
    , buildMonoidDdumpDir :: !(First Text)
    } deriving (Show, Generic)

instance FromJSON (WithJSONWarnings BuildOptsMonoid) where
  parseJSON = withObjectWarnings "BuildOptsMonoid"
    (\o -> do let buildMonoidTrace = Any False
                  buildMonoidProfile = Any False
                  buildMonoidNoStrip = Any False
              buildMonoidLibProfile <- FirstFalse <$> o ..:? buildMonoidLibProfileArgName
              buildMonoidExeProfile <-FirstFalse <$>  o ..:? buildMonoidExeProfileArgName
              buildMonoidLibStrip <- FirstTrue <$> o ..:? buildMonoidLibStripArgName
              buildMonoidExeStrip <-FirstTrue <$>  o ..:? buildMonoidExeStripArgName
              buildMonoidHaddock <- FirstFalse <$> o ..:? buildMonoidHaddockArgName
              buildMonoidHaddockOpts <- jsonSubWarnings (o ..:? buildMonoidHaddockOptsArgName ..!= mempty)
              buildMonoidOpenHaddocks <- FirstFalse <$> o ..:? buildMonoidOpenHaddocksArgName
              buildMonoidHaddockDeps <- First <$> o ..:? buildMonoidHaddockDepsArgName
              buildMonoidHaddockInternal <- FirstFalse <$> o ..:? buildMonoidHaddockInternalArgName
              buildMonoidHaddockHyperlinkSource <- FirstTrue <$> o ..:? buildMonoidHaddockHyperlinkSourceArgName
              buildMonoidInstallExes <- FirstFalse <$> o ..:? buildMonoidInstallExesArgName
              buildMonoidInstallCompilerTool <- FirstFalse <$> o ..:? buildMonoidInstallCompilerToolArgName
              buildMonoidPreFetch <- FirstFalse <$> o ..:? buildMonoidPreFetchArgName
              buildMonoidKeepGoing <- First <$> o ..:? buildMonoidKeepGoingArgName
              buildMonoidKeepTmpFiles <- FirstFalse <$> o ..:? buildMonoidKeepTmpFilesArgName
              buildMonoidForceDirty <- FirstFalse <$> o ..:? buildMonoidForceDirtyArgName
              buildMonoidTests <- FirstFalse <$> o ..:? buildMonoidTestsArgName
              buildMonoidTestOpts <- jsonSubWarnings (o ..:? buildMonoidTestOptsArgName ..!= mempty)
              buildMonoidBenchmarks <- FirstFalse <$> o ..:? buildMonoidBenchmarksArgName
              buildMonoidBenchmarkOpts <- jsonSubWarnings (o ..:? buildMonoidBenchmarkOptsArgName ..!= mempty)
              buildMonoidReconfigure <- FirstFalse <$> o ..:? buildMonoidReconfigureArgName
              cabalVerbosity <- First <$> o ..:? buildMonoidCabalVerbosityArgName
              cabalVerbose <- FirstFalse <$> o ..:? buildMonoidCabalVerboseArgName
              let buildMonoidCabalVerbose = cabalVerbosity <> toFirstCabalVerbosity cabalVerbose
              buildMonoidSplitObjs <- FirstFalse <$> o ..:? buildMonoidSplitObjsName
              buildMonoidSkipComponents <- o ..:? buildMonoidSkipComponentsName ..!= mempty
              buildMonoidInterleavedOutput <- FirstTrue <$> o ..:? buildMonoidInterleavedOutputName
              buildMonoidDdumpDir <- o ..:? buildMonoidDdumpDirName ..!= mempty
              pure BuildOptsMonoid{..})

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
data TestOpts =
  TestOpts {toRerunTests :: !Bool -- ^ Whether successful tests will be run gain
           ,toAdditionalArgs :: ![String] -- ^ Arguments passed to the test program
           ,toCoverage :: !Bool -- ^ Generate a code coverage report
           ,toDisableRun :: !Bool -- ^ Disable running of tests
           ,toMaximumTimeSeconds :: !(Maybe Int) -- ^ test suite timeout in seconds
           ,toAllowStdin :: !Bool -- ^ Whether to allow standard input
           } deriving (Eq,Show)

defaultTestOpts :: TestOpts
defaultTestOpts = TestOpts
    { toRerunTests = defaultFirstTrue toMonoidRerunTests
    , toAdditionalArgs = []
    , toCoverage = defaultFirstFalse toMonoidCoverage
    , toDisableRun = defaultFirstFalse toMonoidDisableRun
    , toMaximumTimeSeconds = Nothing
    , toAllowStdin = defaultFirstTrue toMonoidAllowStdin
    }

data TestOptsMonoid =
  TestOptsMonoid
    { toMonoidRerunTests :: !FirstTrue
    , toMonoidAdditionalArgs :: ![String]
    , toMonoidCoverage :: !FirstFalse
    , toMonoidDisableRun :: !FirstFalse
    , toMonoidMaximumTimeSeconds :: !(First (Maybe Int))
    , toMonoidAllowStdin :: !FirstTrue
    } deriving (Show, Generic)

instance FromJSON (WithJSONWarnings TestOptsMonoid) where
  parseJSON = withObjectWarnings "TestOptsMonoid"
    (\o -> do toMonoidRerunTests <- FirstTrue <$> o ..:? toMonoidRerunTestsArgName
              toMonoidAdditionalArgs <- o ..:? toMonoidAdditionalArgsName ..!= []
              toMonoidCoverage <- FirstFalse <$> o ..:? toMonoidCoverageArgName
              toMonoidDisableRun <- FirstFalse <$> o ..:? toMonoidDisableRunArgName
              toMonoidMaximumTimeSeconds <- First <$> o ..:? toMonoidMaximumTimeSecondsArgName
              toMonoidAllowStdin <- FirstTrue <$> o ..:? toMonoidTestsAllowStdinName
              pure TestOptsMonoid{..})

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
newtype HaddockOpts =
  HaddockOpts { hoAdditionalArgs :: [String] -- ^ Arguments passed to haddock program
              } deriving (Eq,Show)

newtype HaddockOptsMonoid =
  HaddockOptsMonoid {hoMonoidAdditionalArgs :: [String]
                    } deriving (Show, Generic)

defaultHaddockOpts :: HaddockOpts
defaultHaddockOpts = HaddockOpts {hoAdditionalArgs = []}

instance FromJSON (WithJSONWarnings HaddockOptsMonoid) where
  parseJSON = withObjectWarnings "HaddockOptsMonoid"
    (\o -> do hoMonoidAdditionalArgs <- o ..:? hoMonoidAdditionalArgsName ..!= []
              pure HaddockOptsMonoid{..})

instance Semigroup HaddockOptsMonoid where
  (<>) = mappenddefault

instance Monoid HaddockOptsMonoid where
  mempty = memptydefault
  mappend = (<>)

hoMonoidAdditionalArgsName :: Text
hoMonoidAdditionalArgsName = "haddock-args"


-- | Options for the 'FinalAction' 'DoBenchmarks'
data BenchmarkOpts =
  BenchmarkOpts
    { beoAdditionalArgs :: !(Maybe String) -- ^ Arguments passed to the benchmark program
    , beoDisableRun :: !Bool -- ^ Disable running of benchmarks
    } deriving (Eq,Show)

defaultBenchmarkOpts :: BenchmarkOpts
defaultBenchmarkOpts = BenchmarkOpts
    { beoAdditionalArgs = Nothing
    , beoDisableRun = False
    }

data BenchmarkOptsMonoid =
  BenchmarkOptsMonoid
     { beoMonoidAdditionalArgs :: !(First String)
     , beoMonoidDisableRun :: !(First Bool)
     } deriving (Show, Generic)

instance FromJSON (WithJSONWarnings BenchmarkOptsMonoid) where
  parseJSON = withObjectWarnings "BenchmarkOptsMonoid"
    (\o -> do beoMonoidAdditionalArgs <- First <$> o ..:? beoMonoidAdditionalArgsArgName
              beoMonoidDisableRun <- First <$> o ..:? beoMonoidDisableRunArgName
              pure BenchmarkOptsMonoid{..})

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
  deriving (Show,Eq)

newtype CabalVerbosity = CabalVerbosity Verbosity
  deriving (Eq, Show)

toFirstCabalVerbosity :: FirstFalse -> First CabalVerbosity
toFirstCabalVerbosity vf = First $ getFirstFalse vf <&> \p ->
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
