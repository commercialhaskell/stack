{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
    , HaddockOpts(..)
    , defaultHaddockOpts
    , HaddockOptsMonoid(..)
    , BenchmarkOpts(..)
    , defaultBenchmarkOpts
    , BenchmarkOptsMonoid(..)
    , FileWatchOpts(..)
    , BuildSubset(..)
    )
    where

import           Control.Applicative
import           Data.Aeson.Extended
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Generics.Deriving.Monoid (memptydefault, mappenddefault)
import           Prelude -- Fix AMP warning
import           Stack.Types.FlagName
import           Stack.Types.PackageName

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
    , boptsLibStrip = True
    , boptsExeStrip = True
    , boptsHaddock = False
    , boptsHaddockOpts = defaultHaddockOpts
    , boptsOpenHaddocks = False
    , boptsHaddockDeps = Nothing
    , boptsHaddockInternal = False
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
    , boptsCLIInitialBuildSteps = False
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
    { buildMonoidLibProfile :: !(First Bool)
    , buildMonoidExeProfile :: !(First Bool)
    , buildMonoidLibStrip :: !(First Bool)
    , buildMonoidExeStrip :: !(First Bool)
    , buildMonoidHaddock :: !(First Bool)
    , buildMonoidHaddockOpts :: !HaddockOptsMonoid
    , buildMonoidOpenHaddocks :: !(First Bool)
    , buildMonoidHaddockDeps :: !(First Bool)
    , buildMonoidHaddockInternal :: !(First Bool)
    , buildMonoidInstallExes :: !(First Bool)
    , buildMonoidPreFetch :: !(First Bool)
    , buildMonoidKeepGoing :: !(First Bool)
    , buildMonoidForceDirty :: !(First Bool)
    , buildMonoidTests :: !(First Bool)
    , buildMonoidTestOpts :: !TestOptsMonoid
    , buildMonoidBenchmarks :: !(First Bool)
    , buildMonoidBenchmarkOpts :: !BenchmarkOptsMonoid
    , buildMonoidReconfigure :: !(First Bool)
    , buildMonoidCabalVerbose :: !(First Bool)
    , buildMonoidSplitObjs :: !(First Bool)
    } deriving (Show, Generic)

instance FromJSON (WithJSONWarnings BuildOptsMonoid) where
  parseJSON = withObjectWarnings "BuildOptsMonoid"
    (\o -> do buildMonoidLibProfile <- First <$> o ..:? buildMonoidLibProfileArgName
              buildMonoidExeProfile <-First <$>  o ..:? buildMonoidExeProfileArgName
              buildMonoidLibStrip <- First <$> o ..:? buildMonoidLibStripArgName
              buildMonoidExeStrip <-First <$>  o ..:? buildMonoidExeStripArgName
              buildMonoidHaddock <- First <$> o ..:? buildMonoidHaddockArgName
              buildMonoidHaddockOpts <- jsonSubWarnings (o ..:? buildMonoidHaddockOptsArgName ..!= mempty)
              buildMonoidOpenHaddocks <- First <$> o ..:? buildMonoidOpenHaddocksArgName
              buildMonoidHaddockDeps <- First <$> o ..:? buildMonoidHaddockDepsArgName
              buildMonoidHaddockInternal <- First <$> o ..:? buildMonoidHaddockInternalArgName
              buildMonoidInstallExes <- First <$> o ..:? buildMonoidInstallExesArgName
              buildMonoidPreFetch <- First <$> o ..:? buildMonoidPreFetchArgName
              buildMonoidKeepGoing <- First <$> o ..:? buildMonoidKeepGoingArgName
              buildMonoidForceDirty <- First <$> o ..:? buildMonoidForceDirtyArgName
              buildMonoidTests <- First <$> o ..:? buildMonoidTestsArgName
              buildMonoidTestOpts <- jsonSubWarnings (o ..:? buildMonoidTestOptsArgName ..!= mempty)
              buildMonoidBenchmarks <- First <$> o ..:? buildMonoidBenchmarksArgName
              buildMonoidBenchmarkOpts <- jsonSubWarnings (o ..:? buildMonoidBenchmarkOptsArgName ..!= mempty)
              buildMonoidReconfigure <- First <$> o ..:? buildMonoidReconfigureArgName
              buildMonoidCabalVerbose <- First <$> o ..:? buildMonoidCabalVerboseArgName
              buildMonoidSplitObjs <- First <$> o ..:? buildMonoidSplitObjsName
              return BuildOptsMonoid{..})

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
    mempty = memptydefault
    mappend = mappenddefault

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
    { toMonoidRerunTests :: !(First Bool)
    , toMonoidAdditionalArgs :: ![String]
    , toMonoidCoverage :: !(First Bool)
    , toMonoidDisableRun :: !(First Bool)
    } deriving (Show, Generic)

instance FromJSON (WithJSONWarnings TestOptsMonoid) where
  parseJSON = withObjectWarnings "TestOptsMonoid"
    (\o -> do toMonoidRerunTests <- First <$> o ..:? toMonoidRerunTestsArgName
              toMonoidAdditionalArgs <- o ..:? toMonoidAdditionalArgsName ..!= []
              toMonoidCoverage <- First <$> o ..:? toMonoidCoverageArgName
              toMonoidDisableRun <- First <$> o ..:? toMonoidDisableRunArgName
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
  mempty = memptydefault
  mappend = mappenddefault



-- | Haddock Options
data HaddockOpts =
  HaddockOpts { hoAdditionalArgs :: ![String] -- ^ Arguments passed to haddock program
              } deriving (Eq,Show)

data HaddockOptsMonoid =
  HaddockOptsMonoid {hoMonoidAdditionalArgs :: ![String]
                    } deriving (Show, Generic)

defaultHaddockOpts :: HaddockOpts
defaultHaddockOpts = HaddockOpts {hoAdditionalArgs = []}

instance FromJSON (WithJSONWarnings HaddockOptsMonoid) where
  parseJSON = withObjectWarnings "HaddockOptsMonoid"
    (\o -> do hoMonoidAdditionalArgs <- o ..:? hoMonoidAdditionalArgsName ..!= []
              return HaddockOptsMonoid{..})

instance Monoid HaddockOptsMonoid where
  mempty = memptydefault
  mappend = mappenddefault

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
              return BenchmarkOptsMonoid{..})

beoMonoidAdditionalArgsArgName :: Text
beoMonoidAdditionalArgsArgName = "benchmark-arguments"

beoMonoidDisableRunArgName :: Text
beoMonoidDisableRunArgName = "no-run-benchmarks"

instance Monoid BenchmarkOptsMonoid where
  mempty = memptydefault
  mappend = mappenddefault

data FileWatchOpts
  = NoFileWatch
  | FileWatch
  | FileWatchPoll
  deriving (Show,Eq)
