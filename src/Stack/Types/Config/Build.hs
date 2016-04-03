{-# LANGUAGE FlexibleInstances, RecordWildCards, OverloadedStrings #-}

-- | Configuration options for building.

module Stack.Types.Config.Build
    (
      BuildOpts(..)
    , BuildCommand(..)
    , defaultBuildOpts
    , defaultBuildOptsCLI
    , BuildOptsCLI(..)
    , BuildOptsMonoid(..)
    , TestOpts(..)
    , defaultTestOpts
    , TestOptsMonoid(..)
    , BenchmarkOpts(..)
    , defaultBenchmarkOpts
    , BenchmarkOptsMonoid(..)
    , FileWatchOpts(..)
    , BuildSubset(..)
    )
    where

import           Data.Aeson.Extended
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Text (Text)
import           Stack.Types.FlagName
import           Stack.Types.PackageName
import           Control.Applicative

-- | Build options that is interpreted by the build command.
--   This is built up from BuildOptsCLI and BuildOptsMonoid
data BuildOpts =
  BuildOpts {boptsLibProfile :: !Bool
            ,boptsExeProfile :: !Bool
            ,boptsHaddock :: !Bool
            -- ^ Build haddocks?
            ,boptsOpenHaddocks :: !Bool
            -- ^ Open haddocks in the browser?
            ,boptsHaddockDeps :: !(Maybe Bool)
            -- ^ Build haddocks for dependencies?
            ,boptsInstallExes :: !Bool
            -- ^ Install executables to user path after building?
            ,boptsPreFetch :: !Bool
            -- ^ Fetch all packages immediately
            -- ^ Watch files for changes and automatically rebuild
            ,boptsKeepGoing :: !(Maybe Bool)
            -- ^ Keep building/running after failure
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
            ,boptsCabalVerbose :: !Bool
            -- ^ Ask Cabal to be verbose in its builds
            ,boptsSplitObjs :: !Bool
            -- ^ Whether to enable split-objs.
            }
  deriving (Show)

defaultBuildOpts :: BuildOpts
defaultBuildOpts = BuildOpts
    { boptsLibProfile = False
    , boptsExeProfile = False
    , boptsHaddock = False
    , boptsOpenHaddocks = False
    , boptsHaddockDeps = Nothing
    , boptsInstallExes = False
    , boptsPreFetch = False
    , boptsKeepGoing = Nothing
    , boptsForceDirty = False
    , boptsTests = False
    , boptsTestOpts = defaultTestOpts
    , boptsBenchmarks = False
    , boptsBenchmarkOpts = defaultBenchmarkOpts
    , boptsReconfigure = False
    , boptsCabalVerbose = False
    , boptsSplitObjs = False
    }

defaultBuildOptsCLI ::BuildOptsCLI
defaultBuildOptsCLI = BuildOptsCLI
    { boptsCLITargets = []
    , boptsCLIDryrun = False
    , boptsCLIFlags = Map.empty
    , boptsCLIGhcOptions = []
    , boptsCLIBuildSubset = BSAll
    , boptsCLIFileWatch = NoFileWatch
    , boptsCLIExec = []
    , boptsCLIOnlyConfigure = False
    , boptsCLICommand = Build
    }

-- | Build options that may only be specified from the CLI
data BuildOptsCLI = BuildOptsCLI
    { boptsCLITargets :: ![Text]
    , boptsCLIDryrun :: !Bool
    , boptsCLIGhcOptions :: ![Text]
    , boptsCLIFlags :: !(Map (Maybe PackageName) (Map FlagName Bool))
    , boptsCLIBuildSubset :: !BuildSubset
    , boptsCLIFileWatch :: !FileWatchOpts
    , boptsCLIExec :: ![(String, [String])]
    , boptsCLIOnlyConfigure :: !Bool
    , boptsCLICommand :: !BuildCommand
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
    { buildMonoidLibProfile :: !(Maybe Bool)
    , buildMonoidExeProfile :: !(Maybe Bool)
    , buildMonoidHaddock :: !(Maybe Bool)
    , buildMonoidOpenHaddocks :: !(Maybe Bool)
    , buildMonoidHaddockDeps :: !(Maybe Bool)
    , buildMonoidInstallExes :: !(Maybe Bool)
    , buildMonoidPreFetch :: !(Maybe Bool)
    , buildMonoidKeepGoing :: !(Maybe Bool)
    , buildMonoidForceDirty :: !(Maybe Bool)
    , buildMonoidTests :: !(Maybe Bool)
    , buildMonoidTestOpts :: !TestOptsMonoid
    , buildMonoidBenchmarks :: !(Maybe Bool)
    , buildMonoidBenchmarkOpts :: !BenchmarkOptsMonoid
    , buildMonoidReconfigure :: !(Maybe Bool)
    , buildMonoidCabalVerbose :: !(Maybe Bool)
    , buildMonoidSplitObjs :: !(Maybe Bool)
    } deriving (Show)

instance FromJSON (WithJSONWarnings BuildOptsMonoid) where
  parseJSON = withObjectWarnings "BuildOptsMonoid"
    (\o -> do buildMonoidLibProfile <- o ..:? buildMonoidLibProfileArgName
              buildMonoidExeProfile <- o ..:? buildMonoidExeProfileArgName
              buildMonoidHaddock <- o ..:? buildMonoidHaddockArgName
              buildMonoidOpenHaddocks <- o ..:? buildMonoidOpenHaddocksArgName
              buildMonoidHaddockDeps <- o ..:? buildMonoidHaddockDepsArgName
              buildMonoidInstallExes <- o ..:? buildMonoidInstallExesArgName
              buildMonoidPreFetch <- o ..:? buildMonoidPreFetchArgName
              buildMonoidKeepGoing <- o ..:? buildMonoidKeepGoingArgName
              buildMonoidForceDirty <- o ..:? buildMonoidForceDirtyArgName
              buildMonoidTests <- o ..:? buildMonoidTestsArgName
              buildMonoidTestOpts <- jsonSubWarnings (o ..:? buildMonoidTestOptsArgName ..!= mempty)
              buildMonoidBenchmarks <- o ..:? buildMonoidBenchmarksArgName
              buildMonoidBenchmarkOpts <- jsonSubWarnings (o ..:? buildMonoidBenchmarkOptsArgName ..!= mempty)
              buildMonoidReconfigure <- o ..:? buildMonoidReconfigureArgName
              buildMonoidCabalVerbose <- o ..:? buildMonoidCabalVerboseArgName
              buildMonoidSplitObjs <- o ..:? buildMonoidSplitObjsName
              return BuildOptsMonoid{..})

buildMonoidLibProfileArgName :: Text
buildMonoidLibProfileArgName = "library-profiling"

buildMonoidExeProfileArgName :: Text
buildMonoidExeProfileArgName = "executable-profiling"

buildMonoidHaddockArgName :: Text
buildMonoidHaddockArgName = "haddock"

buildMonoidOpenHaddocksArgName :: Text
buildMonoidOpenHaddocksArgName = "open-haddocks"

buildMonoidHaddockDepsArgName :: Text
buildMonoidHaddockDepsArgName = "haddock-deps"

buildMonoidInstallExesArgName :: Text
buildMonoidInstallExesArgName = "copy-bins"

buildMonoidPreFetchArgName :: Text
buildMonoidPreFetchArgName = "prefetch"

buildMonoidKeepGoingArgName :: Text
buildMonoidKeepGoingArgName = "keep-going"

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

buildMonoidCabalVerboseArgName :: Text
buildMonoidCabalVerboseArgName = "cabal-verbose"

buildMonoidSplitObjsName :: Text
buildMonoidSplitObjsName = "split-objs"

instance Monoid BuildOptsMonoid where
  mempty = BuildOptsMonoid
    {buildMonoidLibProfile = Nothing
    ,buildMonoidExeProfile = Nothing
    ,buildMonoidHaddock = Nothing
    ,buildMonoidOpenHaddocks = Nothing
    ,buildMonoidHaddockDeps = Nothing
    ,buildMonoidInstallExes = Nothing
    ,buildMonoidPreFetch = Nothing
    ,buildMonoidKeepGoing = Nothing
    ,buildMonoidForceDirty = Nothing
    ,buildMonoidTests = Nothing
    ,buildMonoidTestOpts = mempty
    ,buildMonoidBenchmarks = Nothing
    ,buildMonoidBenchmarkOpts = mempty
    ,buildMonoidReconfigure = Nothing
    ,buildMonoidCabalVerbose = Nothing
    ,buildMonoidSplitObjs = Nothing
    }

  mappend l r = BuildOptsMonoid
    {buildMonoidLibProfile = buildMonoidLibProfile l <|> buildMonoidLibProfile r
    ,buildMonoidExeProfile = buildMonoidExeProfile l <|> buildMonoidExeProfile r
    ,buildMonoidHaddock = buildMonoidHaddock l <|> buildMonoidHaddock r
    ,buildMonoidOpenHaddocks = buildMonoidOpenHaddocks l <|> buildMonoidOpenHaddocks r
    ,buildMonoidHaddockDeps = buildMonoidHaddockDeps l <|> buildMonoidHaddockDeps r
    ,buildMonoidInstallExes = buildMonoidInstallExes l <|> buildMonoidInstallExes r
    ,buildMonoidPreFetch = buildMonoidPreFetch l <|> buildMonoidPreFetch r
    ,buildMonoidKeepGoing = buildMonoidKeepGoing l <|> buildMonoidKeepGoing r
    ,buildMonoidForceDirty = buildMonoidForceDirty l <|> buildMonoidForceDirty r
    ,buildMonoidTests = buildMonoidTests l <|> buildMonoidTests r
    ,buildMonoidTestOpts = buildMonoidTestOpts l <> buildMonoidTestOpts r
    ,buildMonoidBenchmarks = buildMonoidBenchmarks l <|> buildMonoidBenchmarks r
    ,buildMonoidBenchmarkOpts = buildMonoidBenchmarkOpts l <> buildMonoidBenchmarkOpts r
    ,buildMonoidReconfigure = buildMonoidReconfigure l <|> buildMonoidReconfigure r
    ,buildMonoidCabalVerbose = buildMonoidCabalVerbose l <|> buildMonoidCabalVerbose r
    ,buildMonoidSplitObjs = buildMonoidSplitObjs l <|> buildMonoidSplitObjs r
    }

-- | Which subset of packages to build
data BuildSubset
    = BSAll
    | BSOnlySnapshot
    -- ^ Only install packages in the snapshot database, skipping
    -- packages intended for the local database.
    | BSOnlyDependencies
    deriving (Show, Eq)

-- | Options for the 'FinalAction' 'DoTests'
data TestOpts =
  TestOpts {toRerunTests :: !Bool -- ^ Whether successful tests will be run gain
           ,toAdditionalArgs :: ![String] -- ^ Arguments passed to the test program
           ,toCoverage :: !Bool -- ^ Generate a code coverage report
           ,toDisableRun :: !Bool -- ^ Disable running of tests
           } deriving (Eq,Show)

defaultTestOpts :: TestOpts
defaultTestOpts = TestOpts
    { toRerunTests = True
    , toAdditionalArgs = []
    , toCoverage = False
    , toDisableRun = False
    }

data TestOptsMonoid =
  TestOptsMonoid
    { toMonoidRerunTests :: !(Maybe Bool)
    , toMonoidAdditionalArgs :: ![String]
    , toMonoidCoverage :: !(Maybe Bool)
    , toMonoidDisableRun :: !(Maybe Bool)
    } deriving (Show)

instance FromJSON (WithJSONWarnings TestOptsMonoid) where
  parseJSON = withObjectWarnings "TestOptsMonoid"
    (\o -> do toMonoidRerunTests <- o ..:? toMonoidRerunTestsArgName
              toMonoidAdditionalArgs <- o ..:? toMonoidAdditionalArgsName ..!= []
              toMonoidCoverage <- o ..:? toMonoidCoverageArgName
              toMonoidDisableRun <- o ..:? toMonoidDisableRunArgName
              return TestOptsMonoid{..})

toMonoidRerunTestsArgName :: Text
toMonoidRerunTestsArgName = "rerun-tests"

toMonoidAdditionalArgsName :: Text
toMonoidAdditionalArgsName = "additional-args"

toMonoidCoverageArgName :: Text
toMonoidCoverageArgName = "coverage"

toMonoidDisableRunArgName :: Text
toMonoidDisableRunArgName = "no-run-tests"

instance Monoid TestOptsMonoid where
  mempty = TestOptsMonoid
    { toMonoidRerunTests = Nothing
    , toMonoidAdditionalArgs = []
    , toMonoidCoverage = Nothing
    , toMonoidDisableRun = Nothing
    }
  mappend l r = TestOptsMonoid
    { toMonoidRerunTests = toMonoidRerunTests l <|> toMonoidRerunTests r
    , toMonoidAdditionalArgs = toMonoidAdditionalArgs l <> toMonoidAdditionalArgs r
    , toMonoidCoverage = toMonoidCoverage l <|> toMonoidCoverage r
    , toMonoidDisableRun = toMonoidDisableRun l <|> toMonoidDisableRun r
    }

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
     { beoMonoidAdditionalArgs :: !(Maybe String)
     , beoMonoidDisableRun :: !(Maybe Bool)
     } deriving (Show)

instance FromJSON (WithJSONWarnings BenchmarkOptsMonoid) where
  parseJSON = withObjectWarnings "BenchmarkOptsMonoid"
    (\o -> do beoMonoidAdditionalArgs <- o ..:? beoMonoidAdditionalArgsArgName
              beoMonoidDisableRun <- o ..:? beoMonoidDisableRunArgName
              return BenchmarkOptsMonoid{..})

beoMonoidAdditionalArgsArgName :: Text
beoMonoidAdditionalArgsArgName = "benchmark-arguments"

beoMonoidDisableRunArgName :: Text
beoMonoidDisableRunArgName = "no-run-benchmarks"

instance Monoid BenchmarkOptsMonoid where
  mempty = BenchmarkOptsMonoid
    { beoMonoidAdditionalArgs = Nothing
    , beoMonoidDisableRun = Nothing}
  mappend l r = BenchmarkOptsMonoid
    { beoMonoidAdditionalArgs = beoMonoidAdditionalArgs l <|> beoMonoidAdditionalArgs r
    , beoMonoidDisableRun = beoMonoidDisableRun l <|> beoMonoidDisableRun r}

data FileWatchOpts
  = NoFileWatch
  | FileWatch
  | FileWatchPoll
  deriving (Show,Eq)
