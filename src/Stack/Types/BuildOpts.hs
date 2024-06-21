{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

-- | Configuration options for building.
module Stack.Types.BuildOpts
  ( BuildOpts (..)
  , HaddockOpts (..)
  , TestOpts (..)
  , BenchmarkOpts (..)
  , buildOptsHaddockL
  , buildOptsInstallExesL
  ) where

import           Stack.Prelude
import           Stack.Types.BuildOptsMonoid
                   ( CabalVerbosity (..), ProgressBarFormat (..) )
import           Stack.Types.Component ( StackUnqualCompName )

-- | Build options that is interpreted by the build command. This is built up
-- from BuildOptsCLI and BuildOptsMonoid
data BuildOpts = BuildOpts
  { libProfile :: !Bool
  , exeProfile :: !Bool
  , libStrip :: !Bool
  , exeStrip :: !Bool
  , buildHaddocks :: !Bool
    -- ^ Build Haddock documentation?
  , haddockOpts :: !HaddockOpts
    -- ^ Options to pass to haddock
  , openHaddocks :: !Bool
    -- ^ Open haddocks in the browser?
  , haddockDeps :: !(Maybe Bool)
    -- ^ Build haddocks for dependencies?
  , haddockExecutables :: !Bool
    -- ^ Also build Haddock documentation for all executable components, like
    -- @runghc Setup.hs haddock --executables@.
  , haddockTests :: !Bool
    -- ^ Also build Haddock documentation for all test suite components, like
    -- @runghc Setup.hs haddock --tests@.
  , haddockBenchmarks :: !Bool
    -- ^ Also build Haddock documentation for all benchmark components, like
    -- @runghc Setup.hs haddock --benchmarks@.
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
    -- ^ Force treating all project packages and local extra-deps as having
    -- dirty files.
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
  , skipComponents :: ![StackUnqualCompName]
    -- ^ Which components to skip when building
  , interleavedOutput :: !Bool
    -- ^ Should we use the interleaved GHC output when building
    -- multiple packages?
  , progressBar :: !ProgressBarFormat
    -- ^ Format of the progress bar
  , ddumpDir :: !(Maybe Text)
  }
  deriving Show

-- | Haddock Options
newtype HaddockOpts = HaddockOpts
  { additionalArgs :: [String] -- ^ Arguments passed to haddock program
  }
  deriving (Eq, Show)

-- | Options for the 'FinalAction' 'DoTests'
data TestOpts = TestOpts
  { rerunTests :: !Bool -- ^ Whether successful tests will be run gain
  , additionalArgs :: ![String] -- ^ Arguments passed to the test program
  , coverage :: !Bool -- ^ Generate a code coverage report
  , disableRun :: !Bool -- ^ Disable running of tests
  , maximumTimeSeconds :: !(Maybe Int) -- ^ test suite timeout in seconds
  , allowStdin :: !Bool -- ^ Whether to allow standard input
  }
  deriving (Eq, Show)

-- | Options for the 'FinalAction' 'DoBenchmarks'
data BenchmarkOpts = BenchmarkOpts
  { additionalArgs :: !(Maybe String)
    -- ^ Arguments passed to the benchmark program
  , disableRun :: !Bool
    -- ^ Disable running of benchmarks
  }
  deriving (Eq, Show)

buildOptsInstallExesL :: Lens' BuildOpts Bool
buildOptsInstallExesL =
  lens (.installExes) (\bopts t -> bopts {installExes = t})

buildOptsHaddockL :: Lens' BuildOpts Bool
buildOptsHaddockL =
  lens (.buildHaddocks) (\bopts t -> bopts {buildHaddocks = t})
