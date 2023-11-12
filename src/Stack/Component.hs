{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | All utility functions for Components (library, internal library, foreign library, executable, tests, benchmarks) in stack.
-- In particular, this module gathers all the Cabal-to-Stack component translations, which previlously occured in the "Stack.Package" module.
-- See "Stack.Types.Component" for more details about the design choices.

module Stack.Component
  ( isComponentBuildable
  , stackLibraryFromCabal
  , stackExecutableFromCabal
  , stackForeignLibraryFromCabal
  , stackBenchmarkFromCabal
  , stackTestFromCabal
  )
  where
import           Stack.Prelude
import           Stack.Types.Component
import           Stack.Types.Dependency ( cabalExeToStackDep, cabalToStackDep )
import           Distribution.PackageDescription (Library (libName), ForeignLib, TestSuite (testName, testBuildInfo, testInterface), Benchmark (benchmarkBuildInfo, benchmarkName), Executable)
import           Distribution.Types.BuildInfo (BuildInfo)
import qualified Distribution.Types.BuildInfo as BI
import           Distribution.Types.LibraryName (LibraryName(LMainLibName, LSubLibName))
import qualified Distribution.Types.Library
import qualified Distribution.Types.ForeignLib
import qualified Distribution.Types.Executable
import           Data.Text (pack)
import           Distribution.Types.UnqualComponentName (UnqualComponentName)
import qualified Distribution.PackageDescription as Cabal
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Distribution.Package (mkPackageName)

fromCabalName :: UnqualComponentName -> StackUnqualCompName
fromCabalName unqualName = StackUnqualCompName $ pack . Cabal.unUnqualComponentName $ unqualName

stackLibraryFromCabal :: Library -> StackLibrary
stackLibraryFromCabal cabalLib = StackLibrary {
  name=case cabalLib.libName of
      LMainLibName -> StackUnqualCompName mempty
      LSubLibName v -> fromCabalName v,
  buildInfo=stackBuildInfoFromCabal cabalLib.libBuildInfo,
  exposedModules=cabalLib.exposedModules
  }
stackExecutableFromCabal :: Executable -> StackExecutable
stackExecutableFromCabal cabalExecutable = StackExecutable {
  name=fromCabalName cabalExecutable.exeName,
  buildInfo=stackBuildInfoFromCabal cabalExecutable.buildInfo,
  modulePath=cabalExecutable.modulePath
  }
stackForeignLibraryFromCabal :: ForeignLib -> StackForeignLibrary
stackForeignLibraryFromCabal cabalForeignLib = StackForeignLibrary {
  name=fromCabalName cabalForeignLib.foreignLibName,
  buildInfo=stackBuildInfoFromCabal cabalForeignLib.foreignLibBuildInfo
  }
stackBenchmarkFromCabal :: Benchmark -> StackBenchmark
stackBenchmarkFromCabal cabalBenchmark = StackBenchmark {
  name=fromCabalName cabalBenchmark.benchmarkName,
  interface=cabalBenchmark.benchmarkInterface,
  buildInfo=stackBuildInfoFromCabal cabalBenchmark.benchmarkBuildInfo
  }
stackTestFromCabal :: TestSuite -> StackTest
stackTestFromCabal cabalTest = StackTest {
  name=fromCabalName cabalTest.testName,
  interface=cabalTest.testInterface,
  buildInfo=stackBuildInfoFromCabal cabalTest.testBuildInfo
  }

isComponentBuildable :: HasBuildInfo component => component -> Bool
isComponentBuildable componentRec = componentRec.buildInfo.sbiBuildable

stackBuildInfoFromCabal :: BuildInfo -> StackBuildInfo
stackBuildInfoFromCabal buildInfoV = gatherComponentToolsAndDepsFromCabal
  buildInfoV.buildTools
  buildInfoV.buildToolDepends
  buildInfoV.targetBuildDepends
  StackBuildInfo {
    sbiBuildable = buildInfoV.buildable,
    sbiOtherModules = buildInfoV.otherModules,
    sbiJsSources = buildInfoV.jsSources,
    hsSourceDirs = buildInfoV.hsSourceDirs,
    cSource = buildInfoV.cSources,
    sbiDependency = mempty,
    sbiUnknownTools = mempty
    }

-- | Iterate on all three dependency list given, and transform and sort them between sbiUnknownTools
-- and legitimate 'DepValue' sbiDependency.
-- Bear in mind that this only gathers the component level dependencies.
gatherComponentToolsAndDepsFromCabal
  :: [Cabal.LegacyExeDependency]
  -- ^ Legacy build tools dependency from [buildTools](https://hackage.haskell.org/package/Cabal-syntax/docs/Distribution-Types-BuildInfo.html#t:buildTools).
  -> [Cabal.ExeDependency]
  -- ^ Build tools dependency [buildToolDepends](https://hackage.haskell.org/package/Cabal-syntax/docs/Distribution-Types-BuildInfo.html#t:buildToolDepends)
  -> [Cabal.Dependency]
  -- ^ The Cabal defined [targetBuildDepends](https://hackage.haskell.org/package/Cabal-syntax/docs/Distribution-Types-BuildInfo.html#t:targetBuildDepends),
  -- these are the simplest dependencies for a component extracted from the cabal file such as :
  -- @
  --  build-depends:
  --      foo ^>= 1.2.3.4,
  --      bar ^>= 1
  -- @
  -> StackBuildInfo
  -> StackBuildInfo
gatherComponentToolsAndDepsFromCabal legacyBuildTools buildTools targetDeps = gatherTargetDependency . gatherToolsDependency . gatherUnknownTools
  where
    gatherUnknownTools sbi = foldl' processLegacyExeDepency sbi legacyBuildTools
    gatherToolsDependency sbi = foldl' processExeDependency sbi buildTools
    gatherTargetDependency sbi = foldl' processDependency sbi targetDeps
    -- This is similar to [desugarBuildTool](https://hackage.haskell.org/package/Cabal/docs/src/Distribution.Simple.BuildToolDepends.html#desugarBuildTool)
    -- from Cabal, however it uses our own hard-coded map which drops tools shipped with
    -- GHC (like hsc2hs), and includes some tools from Stackage.
    processLegacyExeDepency sbi (Cabal.LegacyExeDependency exeName range) = case isKnownLegacyExe exeName of
      Just pName -> processExeDependency sbi (Cabal.ExeDependency pName (Cabal.mkUnqualComponentName exeName) range)
      Nothing -> sbi{sbiUnknownTools = Set.insert (pack exeName) $ sbiUnknownTools sbi}
    processExeDependency sbi exeDep@(Cabal.ExeDependency pName _ _)
      | isPreInstalledPackages pName = sbi
      | otherwise = sbi{sbiDependency = Map.insert pName (cabalExeToStackDep exeDep) $ sbiDependency sbi}
    processDependency sbi dep@(Cabal.Dependency pName _ _) = sbi{
      sbiDependency = Map.insert pName (cabalToStackDep dep) $ sbiDependency sbi
      }

-- | A hard-coded map for tool dependencies.
-- If a dependency is within this map it's considered "known" (the exe will be found at the execution stage).
-- [It also exists in Cabal](https://hackage.haskell.org/package/Cabal/docs/src/Distribution.Simple.BuildToolDepends.html#local-6989586621679259154)
isKnownLegacyExe :: String -> Maybe PackageName
isKnownLegacyExe input = case input of
  "alex" -> justPck "alex"
  "happy" -> justPck "happy"
  "cpphs" -> justPck "cpphs"
  "greencard" -> justPck "greencard"
  "c2hs" -> justPck "c2hs"
  "hscolour" -> justPck "hscolour"
  "hspec-iscover" -> justPck "hspec-discover"
  "hsx2hs" -> justPck "hsx2hs"
  "gtk2hsC2hs" -> justPck "gtk2hs-buildtools"
  "gtk2hsHookGenerator" -> justPck "gtk2hs-buildtools"
  "gtk2hsTypeGen" -> justPck "gtk2hs-buildtools"
  _ -> Nothing
  where justPck = Just . mkPackageName

-- | Executable-only packages which come pre-installed with GHC and do
-- not need to be built. Without this exception, we would either end
-- up unnecessarily rebuilding these packages, or failing because the
-- packages do not appear in the Stackage snapshot.
isPreInstalledPackages :: PackageName -> Bool
isPreInstalledPackages input = case input of
  "hsc2hs" -> True
  "haddock" -> True
  _ -> False
