{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

-- | All utility functions for Components in Stack (library, internal library,
-- foreign library, executable, tests, benchmarks). In particular, this module
-- gathers all the Cabal-to-Stack component translations, which previously
-- occurred in the "Stack.Package" module. See "Stack.Types.Component" for more
-- details about the design choices.

module Stack.Component
  ( isComponentBuildable
  , stackLibraryFromCabal
  , stackExecutableFromCabal
  , stackForeignLibraryFromCabal
  , stackBenchmarkFromCabal
  , stackTestFromCabal
  , foldOnNameAndBuildInfo
  , componentDependencyMap
  , fromCabalName
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text ( pack )
import           Distribution.PackageDescription
                   ( Benchmark (..), Executable, ForeignLib, Library (..)
                   , TestSuite (..)
                   )
import           Distribution.Types.BuildInfo ( BuildInfo )
import           Distribution.Package ( mkPackageName )
import qualified Distribution.PackageDescription as Cabal
import           Distribution.Utils.Path (interpretSymbolicPathCWD)
import           GHC.Records ( HasField )
import           Stack.Prelude
import           Stack.Types.Component
                   ( HasBuildInfo, StackBenchmark (..), StackBuildInfo (..)
                   , StackExecutable (..), StackForeignLibrary (..)
                   , StackLibrary (..), StackTestSuite (..)
                   , StackUnqualCompName (..)
                   )
import           Stack.Types.ComponentUtils ( fromCabalName )
import           Stack.Types.Dependency ( cabalExeToStackDep, cabalToStackDep )

foldOnNameAndBuildInfo ::
     ( HasField "buildInfo" a StackBuildInfo
     , HasField "name" a StackUnqualCompName
     , Foldable c
     )
  => c a
  -> (StackUnqualCompName -> StackBuildInfo -> t -> t)
  -> t
  -> t
foldOnNameAndBuildInfo initialCollection accumulator input =
  foldr' iterator input initialCollection
 where
  iterator comp = accumulator comp.name comp.buildInfo

stackLibraryFromCabal :: Library -> StackLibrary
stackLibraryFromCabal cabalLib = StackLibrary
  { name = case cabalLib.libName of
      LMainLibName -> StackUnqualCompName mempty
      LSubLibName v -> fromCabalName v
  , buildInfo = stackBuildInfoFromCabal cabalLib.libBuildInfo
  , exposedModules = cabalLib.exposedModules
  }

stackExecutableFromCabal :: Executable -> StackExecutable
stackExecutableFromCabal cabalExecutable = StackExecutable
  { name = fromCabalName cabalExecutable.exeName
  , buildInfo = stackBuildInfoFromCabal cabalExecutable.buildInfo
  , modulePath = interpretSymbolicPathCWD cabalExecutable.modulePath
  }

stackForeignLibraryFromCabal :: ForeignLib -> StackForeignLibrary
stackForeignLibraryFromCabal cabalForeignLib = StackForeignLibrary
  { name = fromCabalName cabalForeignLib.foreignLibName
  , buildInfo=stackBuildInfoFromCabal cabalForeignLib.foreignLibBuildInfo
  }

stackBenchmarkFromCabal :: Benchmark -> StackBenchmark
stackBenchmarkFromCabal cabalBenchmark = StackBenchmark
  { name = fromCabalName cabalBenchmark.benchmarkName
  , interface = cabalBenchmark.benchmarkInterface
  , buildInfo = stackBuildInfoFromCabal cabalBenchmark.benchmarkBuildInfo
  }

stackTestFromCabal :: TestSuite -> StackTestSuite
stackTestFromCabal cabalTest = StackTestSuite
  { name = fromCabalName cabalTest.testName
  , interface = cabalTest.testInterface
  , buildInfo = stackBuildInfoFromCabal cabalTest.testBuildInfo
  }

isComponentBuildable :: HasBuildInfo component => component -> Bool
isComponentBuildable componentRec = componentRec.buildInfo.buildable

stackBuildInfoFromCabal :: BuildInfo -> StackBuildInfo
stackBuildInfoFromCabal buildInfoV = gatherComponentToolsAndDepsFromCabal
  buildInfoV.buildTools
  buildInfoV.buildToolDepends
  buildInfoV.targetBuildDepends
  StackBuildInfo
    { buildable = buildInfoV.buildable
    , otherModules = buildInfoV.otherModules
    , jsSources = map interpretSymbolicPathCWD buildInfoV.jsSources
    , hsSourceDirs = buildInfoV.hsSourceDirs
    , cSources = map interpretSymbolicPathCWD buildInfoV.cSources
    , dependency = mempty
    , unknownTools = mempty
    , cppOptions = buildInfoV.cppOptions
    , targetBuildDepends = buildInfoV.targetBuildDepends
    , options = buildInfoV.options
    , allLanguages = Cabal.allLanguages buildInfoV
    , usedExtensions = Cabal.usedExtensions buildInfoV
    , includeDirs = map interpretSymbolicPathCWD buildInfoV.includeDirs
    , extraLibs = buildInfoV.extraLibs
    , extraLibDirs = map interpretSymbolicPathCWD buildInfoV.extraLibDirs
    , frameworks = map interpretSymbolicPathCWD buildInfoV.frameworks
    }

-- | Iterate on all three dependency list given, and transform and sort them
-- between 'sbiUnknownTools' and legitimate 'DepValue' sbiDependency. Bear in
-- mind that this only gathers the component level dependencies.
gatherComponentToolsAndDepsFromCabal
  :: [Cabal.LegacyExeDependency]
     -- ^ Legacy build tools dependency from
     -- 'Distribution.Types.BuildInfo.buildTools'.
  -> [Cabal.ExeDependency]
     -- ^ Build tools dependency from
     -- `Distribution.Types.BuildInfo.buildToolDepends'.
  -> [Cabal.Dependency]
     -- ^ Cabal-syntax defines
     -- 'Distribution.Types.BuildInfo.targetBuildDepends'. These are the
     -- simplest dependencies for a component extracted from the Cabal file such
     -- as:
     -- @
     --  build-depends:
     --      foo ^>= 1.2.3.4,
     --      bar ^>= 1
     -- @
  -> StackBuildInfo
  -> StackBuildInfo
gatherComponentToolsAndDepsFromCabal legacyBuildTools buildTools targetDeps =
  gatherTargetDependency . gatherToolsDependency . gatherUnknownTools
 where
  gatherUnknownTools sbi = foldl' processLegacyExeDepency sbi legacyBuildTools
  gatherToolsDependency sbi = foldl' processExeDependency sbi buildTools
  gatherTargetDependency sbi = foldl' processDependency sbi targetDeps
  -- This is similar to Cabal's
  -- 'Distribution.Simple.BuildToolDepends.desugarBuildTool', however it uses
  -- our own hard-coded map which drops tools shipped with GHC (like hsc2hs),
  -- and includes some tools from Stackage.
  processLegacyExeDepency sbi (Cabal.LegacyExeDependency exeName range) =
    case isKnownLegacyExe exeName of
      Just pName ->
        processExeDependency
          sbi
          (Cabal.ExeDependency pName (Cabal.mkUnqualComponentName exeName) range)
      Nothing -> sbi
        { unknownTools = Set.insert (pack exeName) sbi.unknownTools }
  processExeDependency sbi exeDep@(Cabal.ExeDependency pName _ _)
    | isPreInstalledPackages pName = sbi
    | otherwise = sbi
        { dependency =
            Map.insert pName (cabalExeToStackDep exeDep) sbi.dependency
        }
  processDependency sbi dep@(Cabal.Dependency pName _ _) = sbi
    { dependency = Map.insert pName (cabalToStackDep dep) sbi.dependency }

componentDependencyMap ::
     (HasField "buildInfo" r1 r2, HasField "dependency" r2 a)
  => r1
  -> a
componentDependencyMap component = component.buildInfo.dependency

-- | A hard-coded map for tool dependencies. If a dependency is within this map
-- it's considered "known" (the exe will be found at the execution stage). The
-- corresponding Cabal function is
-- 'Distribution.Simple.BuildToolDepends.desugarBuildTool'.
isKnownLegacyExe :: String -> Maybe PackageName
isKnownLegacyExe input = case input of
  "alex" -> justPck "alex"
  "happy" -> justPck "happy"
  "cpphs" -> justPck "cpphs"
  "greencard" -> justPck "greencard"
  "c2hs" -> justPck "c2hs"
  "hscolour" -> justPck "hscolour"
  "hspec-discover" -> justPck "hspec-discover"
  "hsx2hs" -> justPck "hsx2hs"
  "gtk2hsC2hs" -> justPck "gtk2hs-buildtools"
  "gtk2hsHookGenerator" -> justPck "gtk2hs-buildtools"
  "gtk2hsTypeGen" -> justPck "gtk2hs-buildtools"
  _ -> Nothing
 where
  justPck = Just . mkPackageName

-- | Executable-only packages which come pre-installed with GHC and do not need
-- to be built. Without this exception, we would either end up unnecessarily
-- rebuilding these packages, or failing because the packages do not appear in
-- the Stackage snapshot.
isPreInstalledPackages :: PackageName -> Bool
isPreInstalledPackages input = case input of
  "hsc2hs" -> True
  "haddock" -> True
  _ -> False
