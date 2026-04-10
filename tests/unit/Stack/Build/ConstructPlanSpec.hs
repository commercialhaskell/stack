{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Stack.Build.ConstructPlanSpec
  ( main
  , spec
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Distribution.CabalSpecVersion ( CabalSpecVersion (..) )
import           Distribution.Compiler ( PerCompilerFlavor (..) )
import           Distribution.License ( License (BSD3) )
import qualified Distribution.ModuleName as Cabal
import           Distribution.PackageDescription
                   ( BenchmarkInterface (..), BenchmarkType (..)
                   , TestSuiteInterface (..), TestType (..)
                   )
import           Distribution.Types.BuildType
                   ( BuildType (Configure, Custom, Simple) )
import           Distribution.Types.IncludeRenaming
                   ( IncludeRenaming (..), defaultIncludeRenaming )
import           Distribution.Types.Mixin ( Mixin (..) )
import           Distribution.Types.ModuleRenaming
                   ( ModuleRenaming (..) )
import           Distribution.Types.PackageName ( mkPackageName )
import           Distribution.Types.Version ( mkVersion )
import           Distribution.Types.VersionRange ( anyVersion )
import           Distribution.ModuleName ( ModuleName )
import           Stack.Build.Backpack ( addInstantiationTasks )
import           Stack.Build.ConstructPlan ( shouldSplitComponents )
import           Stack.Build.ExecutePackage
                   ( findGhcPkgId, mkInstantiateWithOpts )
import           Stack.Package ( hasIntraPackageDeps, packageIsIndefinite )
import           Stack.Prelude
import           Stack.Types.Build.ConstructPlan ( AddDepRes (..) )
import           Database.Persist ( PersistField (..), PersistValue (..) )
import           Stack.Types.Cache ( CachePkgSrc (..), ConfigCacheType (..) )
import           Stack.Types.CompCollection
                   ( foldAndMakeCollection, getBuildableSet )
import           Stack.Types.Component
                   ( StackBenchmark (..), StackBuildInfo (..)
                   , StackExecutable (..), StackForeignLibrary (..)
                   , StackLibrary (..), StackTestSuite (..)
                   )
import           Stack.Types.ComponentUtils
                   ( emptyCompName, unqualCompFromString )
import           Stack.Types.Dependency
                   ( DepLibrary (..), DepType (..), DepValue (..) )
import           Stack.Types.Installed
                   ( InstallLocation (..), Installed (..)
                   , installedLibraryInfoFromGhcPkgId
                   )
import           Stack.Types.GhcPkgId ( GhcPkgId, parseGhcPkgId )
import           Stack.Types.IsMutable ( IsMutable (..) )
import           Stack.Types.NamedComponent ( NamedComponent (..) )
import           Stack.Types.Package ( Package (..), packageIdentifier )
import           Stack.Types.PackageFile ( StackPackageFile (..) )
import           Stack.Types.Plan
                   ( ComponentKey (..), Task (..), TaskConfigOpts (..)
                   , TaskType (..)
                   )
import           Test.Hspec
                   ( Spec, describe, expectationFailure, hspec, it
                   , shouldBe, shouldSatisfy
                   )

main :: IO ()
main = hspec spec

-- | A minimal StackBuildInfo suitable for testing. All lazy fields are empty
-- lists/sets; buildable defaults to True, no dependencies.
testBuildInfo :: StackBuildInfo
testBuildInfo = StackBuildInfo
  { buildable = True
  , dependency = Map.empty
  , unknownTools = Set.empty
  , otherModules = []
  , jsSources = []
  , hsSourceDirs = []
  , cSources = []
  , cppOptions = []
  , targetBuildDepends = []
  , options = PerCompilerFlavor [] []
  , allLanguages = []
  , usedExtensions = []
  , includeDirs = []
  , extraLibs = []
  , extraLibDirs = []
  , frameworks = []
  , mixins = []
  }

-- | A build info that declares a dependency on the package itself (Backpack).
selfDepBuildInfo :: PackageName -> StackBuildInfo
selfDepBuildInfo pn = testBuildInfo
  { dependency = Map.singleton pn depVal }
 where
  depVal = DepValue
    { versionRange = anyVersion
    , depType = AsLibrary DepLibrary { main = True, subLib = Set.empty }
    }

-- | Create a minimal test Package.
testPackage :: PackageName -> BuildType -> Package
testPackage pn bt = Package
  { name = pn
  , version = mkVersion [1, 0, 0]
  , license = Right BSD3
  , ghcOptions = []
  , cabalConfigOpts = []
  , flags = Map.empty
  , defaultFlags = Map.empty
  , library = Nothing
  , subLibraries = mempty
  , foreignLibraries = mempty
  , testSuites = mempty
  , benchmarks = mempty
  , executables = mempty
  , buildType = bt
  , setupDeps = Nothing
  , cabalSpec = CabalSpecV2_0
  , file = StackPackageFile
      { extraSrcFiles = [], dataDir = "", dataFiles = [] }
  , testEnabled = False
  , benchmarkEnabled = False
  }

-- | Add a buildable main library to a package.
withLibrary :: Package -> Package
withLibrary pkg = pkg
  { library = Just StackLibrary
      { name = emptyCompName
      , buildInfo = testBuildInfo
      , exposedModules = []
      , signatures = []
      }
  }

-- | Add a buildable main library that depends on itself (Backpack pattern:
-- main lib using internal sub-libs).
withSelfDepLibrary :: Package -> Package
withSelfDepLibrary pkg = pkg
  { library = Just StackLibrary
      { name = emptyCompName
      , buildInfo = selfDepBuildInfo pkg.name
      , exposedModules = []
      , signatures = []
      }
  }

-- | Add a buildable sub-library that depends on the same package (Backpack
-- pattern: sub-library chains).
withSelfDepSubLib :: Package -> Package
withSelfDepSubLib pkg = pkg
  { subLibraries = foldAndMakeCollection id
      [ StackLibrary
          { name = unqualCompFromString "internal"
          , buildInfo = selfDepBuildInfo pkg.name
          , exposedModules = []
          , signatures = []
          }
      ]
  }

-- | Add a buildable main library with Backpack signatures (makes it indefinite).
withIndefiniteLibrary :: [ModuleName] -> Package -> Package
withIndefiniteLibrary sigs pkg = pkg
  { library = Just StackLibrary
      { name = emptyCompName
      , buildInfo = testBuildInfo
      , exposedModules = []
      , signatures = sigs
      }
  }

-- | Add buildable executables to a package.
withExes :: [String] -> Package -> Package
withExes names pkg = pkg
  { executables = foldAndMakeCollection id
      [ StackExecutable
          { name = unqualCompFromString n
          , buildInfo = testBuildInfo
          , modulePath = "Main.hs"
          }
      | n <- names
      ]
  }

-- | Add buildable executables that depend on the package's own library.
-- This is the common Cabal pattern: @build-depends: my-package@.
withSelfDepExes :: [String] -> Package -> Package
withSelfDepExes names pkg = pkg
  { executables = foldAndMakeCollection id
      [ StackExecutable
          { name = unqualCompFromString n
          , buildInfo = selfDepBuildInfo pkg.name
          , modulePath = "Main.hs"
          }
      | n <- names
      ]
  }

-- | Add buildable test suites that depend on the package's own library.
-- This is the common Cabal pattern: @build-depends: my-package@.
withSelfDepTests :: [String] -> Package -> Package
withSelfDepTests names pkg = pkg
  { testSuites = foldAndMakeCollection id
      [ StackTestSuite
          { name = unqualCompFromString n
          , buildInfo = selfDepBuildInfo pkg.name
          , interface = TestSuiteUnsupported (TestTypeUnknown "" (mkVersion [0]))
          }
      | n <- names
      ]
  }

-- | Add buildable benchmarks that depend on the package's own library.
withSelfDepBenchmarks :: [String] -> Package -> Package
withSelfDepBenchmarks names pkg = pkg
  { benchmarks = foldAndMakeCollection id
      [ StackBenchmark
          { name = unqualCompFromString n
          , buildInfo = selfDepBuildInfo pkg.name
          , interface = BenchmarkUnsupported (BenchmarkTypeUnknown "" (mkVersion [0]))
          }
      | n <- names
      ]
  }

-- | Add buildable foreign libraries that depend on the package itself.
withSelfDepForeignLibs :: [String] -> Package -> Package
withSelfDepForeignLibs names pkg = pkg
  { foreignLibraries = foldAndMakeCollection id
      [ StackForeignLibrary
          { name = unqualCompFromString n
          , buildInfo = selfDepBuildInfo pkg.name
          }
      | n <- names
      ]
  }

-- | Add buildable sub-libraries to a package.
withSubLibs :: [String] -> Package -> Package
withSubLibs names pkg = pkg
  { subLibraries = foldAndMakeCollection id
      [ StackLibrary
          { name = unqualCompFromString n
          , buildInfo = testBuildInfo
          , exposedModules = []
          , signatures = []
          }
      | n <- names
      ]
  }

-- | Add buildable foreign libraries to a package.
withForeignLibs :: [String] -> Package -> Package
withForeignLibs names pkg = pkg
  { foreignLibraries = foldAndMakeCollection id
      [ StackForeignLibrary
          { name = unqualCompFromString n
          , buildInfo = testBuildInfo
          }
      | n <- names
      ]
  }

-- | Add buildable test suites to a package.
withTests :: [String] -> Package -> Package
withTests names pkg = pkg
  { testSuites = foldAndMakeCollection id
      [ StackTestSuite
          { name = unqualCompFromString n
          , buildInfo = testBuildInfo
          , interface = TestSuiteUnsupported (TestTypeUnknown "" (mkVersion [0]))
          }
      | n <- names
      ]
  }

-- | Add buildable benchmarks to a package.
withBenchmarks :: [String] -> Package -> Package
withBenchmarks names pkg = pkg
  { benchmarks = foldAndMakeCollection id
      [ StackBenchmark
          { name = unqualCompFromString n
          , buildInfo = testBuildInfo
          , interface = BenchmarkUnsupported (BenchmarkTypeUnknown "" (mkVersion [0]))
          }
      | n <- names
      ]
  }

spec :: Spec
spec = do
  describe "shouldSplitComponents" $ do
    it "returns True for Simple package without intra-package deps" $ do
      let pkg = testPackage (mkPackageName "pkg") Simple
      shouldSplitComponents pkg `shouldBe` True

    it "returns False for Custom build type" $ do
      let pkg = testPackage (mkPackageName "pkg") Custom
      shouldSplitComponents pkg `shouldBe` False

    it "returns False for Configure build type" $ do
      let pkg = testPackage (mkPackageName "pkg") Configure
      shouldSplitComponents pkg `shouldBe` False

    it "returns False when main lib depends on own sub-libs (Backpack)" $ do
      let pkg = withSelfDepLibrary $ testPackage (mkPackageName "pkg") Simple
      shouldSplitComponents pkg `shouldBe` False

    it "returns False when sub-lib has self-dep (Backpack)" $ do
      let pkg = withSelfDepSubLib $ testPackage (mkPackageName "pkg") Simple
      shouldSplitComponents pkg `shouldBe` False

    it "returns True when only exes depend on own lib (normal pattern)" $ do
      let pkg = withSelfDepExes ["my-exe"]
              $ withLibrary
              $ testPackage (mkPackageName "pkg") Simple
      shouldSplitComponents pkg `shouldBe` True

    it "returns True for Simple package with library but no self-dep" $ do
      let pkg = withLibrary $ testPackage (mkPackageName "pkg") Simple
      shouldSplitComponents pkg `shouldBe` True

    it "returns True for Simple package with exes and lib" $ do
      let pkg = withExes ["exe1", "exe2"]
              $ withLibrary
              $ testPackage (mkPackageName "pkg") Simple
      shouldSplitComponents pkg `shouldBe` True

    it "returns True when tests depend on own lib (normal pattern)" $ do
      let pkg = withSelfDepTests ["my-tests"]
              $ withLibrary
              $ testPackage (mkPackageName "pkg") Simple
      shouldSplitComponents pkg `shouldBe` True

    it "returns True when benchmarks depend on own lib (normal pattern)" $ do
      let pkg = withSelfDepBenchmarks ["my-bench"]
              $ withLibrary
              $ testPackage (mkPackageName "pkg") Simple
      shouldSplitComponents pkg `shouldBe` True

    it "returns True when foreign lib has self-dep (not Backpack)" $ do
      let pkg = withSelfDepForeignLibs ["cbits"]
              $ withLibrary
              $ testPackage (mkPackageName "pkg") Simple
      shouldSplitComponents pkg `shouldBe` True

    it "returns True for all non-library self-deps combined" $ do
      let pkg = withSelfDepExes ["app"]
              $ withSelfDepTests ["tests"]
              $ withSelfDepBenchmarks ["bench"]
              $ withSelfDepForeignLibs ["ffi"]
              $ withLibrary
              $ testPackage (mkPackageName "pkg") Simple
      shouldSplitComponents pkg `shouldBe` True

    it "returns False for indefinite package (has signatures)" $ do
      let pkg = withIndefiniteLibrary ["Str"]
              $ testPackage (mkPackageName "sig-pkg") Simple
      shouldSplitComponents pkg `shouldBe` False

  describe "packageIsIndefinite" $ do
    it "returns False for package without signatures" $ do
      let pkg = withLibrary $ testPackage (mkPackageName "pkg") Simple
      packageIsIndefinite pkg `shouldBe` False

    it "returns True for package with signatures" $ do
      let pkg = withIndefiniteLibrary ["Str"]
              $ testPackage (mkPackageName "sig-pkg") Simple
      packageIsIndefinite pkg `shouldBe` True

    it "returns True for package with multiple signatures" $ do
      let pkg = withIndefiniteLibrary ["Str", "Num"]
              $ testPackage (mkPackageName "sig-pkg") Simple
      packageIsIndefinite pkg `shouldBe` True

    it "returns True for sub-library with signatures" $ do
      let pkg = (withLibrary $ testPackage (mkPackageName "pkg") Simple)
            { subLibraries = foldAndMakeCollection id
                [ StackLibrary
                    { name = unqualCompFromString "indef-sub"
                    , buildInfo = testBuildInfo
                    , exposedModules = []
                    , signatures = ["Str"]
                    }
                ]
            }
      packageIsIndefinite pkg `shouldBe` True

    it "returns False for package with no library" $ do
      let pkg = withExes ["app"] $ testPackage (mkPackageName "pkg") Simple
      packageIsIndefinite pkg `shouldBe` False

  describe "hasIntraPackageDeps" $ do
    it "returns False for plain library" $ do
      let pkg = withLibrary $ testPackage (mkPackageName "pkg") Simple
      hasIntraPackageDeps pkg `shouldBe` False

    it "returns True when main lib depends on own sub-libs" $ do
      let pkg = withSelfDepLibrary $ testPackage (mkPackageName "pkg") Simple
      hasIntraPackageDeps pkg `shouldBe` True

    it "returns True when sub-lib has self-dep" $ do
      let pkg = withSelfDepSubLib $ testPackage (mkPackageName "pkg") Simple
      hasIntraPackageDeps pkg `shouldBe` True

    it "returns True for indefinite package (has signatures)" $ do
      let pkg = withIndefiniteLibrary ["Str"]
              $ testPackage (mkPackageName "sig-pkg") Simple
      hasIntraPackageDeps pkg `shouldBe` True

    it "returns False when only exes have self-dep" $ do
      let pkg = withSelfDepExes ["app"]
              $ withLibrary
              $ testPackage (mkPackageName "pkg") Simple
      hasIntraPackageDeps pkg `shouldBe` False

    it "returns False when only tests have self-dep" $ do
      let pkg = withSelfDepTests ["tests"]
              $ withLibrary
              $ testPackage (mkPackageName "pkg") Simple
      hasIntraPackageDeps pkg `shouldBe` False

  describe "per-component task expansion" $ do
    describe "component enumeration" $ do
      it "lib-only package produces [CLib]" $ do
        let pkg = withLibrary $ testPackage (mkPackageName "pkg") Simple
            comps = packageBuildableComponents pkg
        comps `shouldBe` [CLib]

      it "lib+exe package produces [CLib, CExe]" $ do
        let pkg = withExes ["my-exe"]
                $ withLibrary
                $ testPackage (mkPackageName "pkg") Simple
            comps = packageBuildableComponents pkg
        comps `shouldSatisfy` (CLib `elem`)
        comps `shouldSatisfy`
          (CExe (unqualCompFromString "my-exe") `elem`)
        length comps `shouldBe` 2

      it "exe-only package (no lib) produces [CExe]" $ do
        let pkg = withExes ["app"]
                $ testPackage (mkPackageName "pkg") Simple
            comps = packageBuildableComponents pkg
        comps `shouldBe` [CExe (unqualCompFromString "app")]

      it "package with sub-libraries produces CLib + CSubLib entries" $ do
        let pkg = withSubLibs ["internal"]
                $ withLibrary
                $ testPackage (mkPackageName "pkg") Simple
            comps = packageBuildableComponents pkg
        length comps `shouldBe` 2
        comps `shouldSatisfy` (CLib `elem`)
        comps `shouldSatisfy`
          (CSubLib (unqualCompFromString "internal") `elem`)

      it "package with foreign libraries produces CFlib entries" $ do
        let pkg = withForeignLibs ["cbits"]
                $ withLibrary
                $ testPackage (mkPackageName "pkg") Simple
            comps = packageBuildableComponents pkg
        length comps `shouldBe` 2
        comps `shouldSatisfy` (CFlib (unqualCompFromString "cbits") `elem`)

      it "package with multiple exes produces one CExe per exe" $ do
        let pkg = withExes ["exe-a", "exe-b", "exe-c"]
                $ testPackage (mkPackageName "pkg") Simple
            comps = packageBuildableComponents pkg
        length comps `shouldBe` 3

      it "full package produces CLib + CSubLib + CFlib + CExe" $ do
        let pkg = withExes ["app"]
                $ withSubLibs ["sub"]
                $ withForeignLibs ["ffi"]
                $ withLibrary
                $ testPackage (mkPackageName "pkg") Simple
            comps = packageBuildableComponents pkg
        length comps `shouldBe` 4

      it "empty package (no components) produces empty list" $ do
        let pkg = testPackage (mkPackageName "pkg") Simple
            comps = packageBuildableComponents pkg
        -- No library, no exes, no sub-libs, no foreign libs
        -- expandToComponentKeys would fall back to CLib for this case
        comps `shouldBe` []

    describe "ComponentKey expansion" $ do
      it "lib+exe package produces distinct ComponentKeys" $ do
        let pn = mkPackageName "pkg"
            pkg = withExes ["my-exe"]
                $ withLibrary
                $ testPackage pn Simple
            keys = packageToComponentKeys pn pkg
        Set.size (Set.fromList keys) `shouldBe` 2
        keys `shouldSatisfy` (ComponentKey pn CLib `elem`)
        keys `shouldSatisfy`
          (ComponentKey pn (CExe (unqualCompFromString "my-exe")) `elem`)

      it "non-splittable package always maps to single CLib key" $ do
        let pn = mkPackageName "pkg"
            pkg = withExes ["my-exe"]
                $ withLibrary
                $ testPackage pn Custom
            keys = packageToComponentKeys pn pkg
        keys `shouldBe` [ComponentKey pn CLib]

      it "Backpack package (sub-lib self-dep) maps to single CLib key" $ do
        let pn = mkPackageName "pkg"
            pkg = withExes ["my-exe"]
                $ withSelfDepSubLib
                $ testPackage pn Simple
            keys = packageToComponentKeys pn pkg
        keys `shouldBe` [ComponentKey pn CLib]

      it "empty splittable package falls back to CLib" $ do
        let pn = mkPackageName "pkg"
            pkg = testPackage pn Simple
            keys = packageToComponentKeys pn pkg
        keys `shouldBe` [ComponentKey pn CLib]

      it "tests and benchmarks are NOT in build component keys" $ do
        let pn = mkPackageName "pkg"
            pkg = withTests ["test-suite"]
                $ withBenchmarks ["bench"]
                $ withLibrary
                $ testPackage pn Simple
            comps = packageBuildableComponents pkg
        -- Tests and benchmarks go into finals, not build tasks
        comps `shouldBe` [CLib]

  describe "finals splitting" $ do
    it "test components produce CTest keys" $ do
      let pn = mkPackageName "pkg"
          pkg = withTests ["suite-a", "suite-b"]
              $ withLibrary
              $ testPackage pn Simple
          finalKeys = packageToFinalKeys pn pkg (Set.fromList
            [ CTest (unqualCompFromString "suite-a")
            , CTest (unqualCompFromString "suite-b")
            ])
      length finalKeys `shouldBe` 2
      finalKeys `shouldSatisfy`
        (ComponentKey pn (CTest (unqualCompFromString "suite-a")) `elem`)
      finalKeys `shouldSatisfy`
        (ComponentKey pn (CTest (unqualCompFromString "suite-b")) `elem`)

    it "bench components produce CBench keys" $ do
      let pn = mkPackageName "pkg"
          pkg = withBenchmarks ["bench-a"]
              $ withLibrary
              $ testPackage pn Simple
          finalKeys = packageToFinalKeys pn pkg (Set.fromList
            [ CBench (unqualCompFromString "bench-a")
            ])
      finalKeys `shouldBe`
        [ComponentKey pn (CBench (unqualCompFromString "bench-a"))]

    it "non-splittable package uses single CLib key for finals" $ do
      let pn = mkPackageName "pkg"
          pkg = withTests ["tests"]
              $ withLibrary
              $ testPackage pn Custom
          finalKeys = packageToFinalKeys pn pkg (Set.fromList
            [ CTest (unqualCompFromString "tests")
            ])
      finalKeys `shouldBe` [ComponentKey pn CLib]

    it "mixed test+bench components produce both keys" $ do
      let pn = mkPackageName "pkg"
          pkg = withTests ["tests"]
              $ withBenchmarks ["bench"]
              $ withLibrary
              $ testPackage pn Simple
          finalKeys = packageToFinalKeys pn pkg (Set.fromList
            [ CTest (unqualCompFromString "tests")
            , CBench (unqualCompFromString "bench")
            ])
      length finalKeys `shouldBe` 2

    it "empty components for splittable package falls back to CLib" $ do
      let pn = mkPackageName "pkg"
          pkg = withLibrary $ testPackage pn Simple
          finalKeys = packageToFinalKeys pn pkg Set.empty
      finalKeys `shouldBe` [ComponentKey pn CLib]

  describe "addInstantiationTasks" $ do
    it "no mixins → no instantiation tasks added" $ do
      let consumerPn = mkPackageName "consumer"
          consumerPkg = withLibrary $ testPackage consumerPn Simple
          consumerTask = mockTask consumerPkg
          origAdrs = [(consumerPn, ADRToInstall consumerTask)]
          expanded = [(ComponentKey consumerPn CLib, ADRToInstall consumerTask)]
          (result, _warnings) = addInstantiationTasks Map.empty origAdrs expanded
      -- No CInst tasks should be created.
      let instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      instKeys `shouldBe` []

    it "mixin referencing indefinite dep creates CInst task" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, _warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instEntries = [ (ck, t)
                        | (ck@(ComponentKey pn (CInst _)), ADRToInstall t)
                            <- result
                        , pn == sigPn
                        ]
      -- Exactly one CInst task for sig-pkg.
      length instEntries `shouldBe` 1

    it "CInst task has correct backpackInstEntries" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, _warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instTasks = [ t
                      | (ComponentKey pn (CInst _), ADRToInstall t) <- result
                      , pn == sigPn
                      ]
      case instTasks of
        [t] -> do
          let entries = t.backpackInstEntries
          length entries `shouldBe` 1
          case entries of
            [(sig, ipn, imod)] -> do
              sig `shouldBe` mn "Str"
              ipn `shouldBe` implPn
              imod `shouldBe` mn "Str"
            _ -> error "unexpected entries length"
        _ -> error "expected exactly one CInst task"

    it "CInst task's missing includes impl-pkg dep" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, _warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instTasks = [ t
                      | (ComponentKey pn (CInst _), ADRToInstall t) <- result
                      , pn == sigPn
                      ]
      case instTasks of
        [t] ->
          -- The CInst task's missing should include impl-pkg's identifier.
          Set.member (packageIdentifier implPkg) t.configOpts.missing
            `shouldBe` True
        _ -> error "expected exactly one CInst task"

    it "consumer's instantiationDeps includes the CInst key" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, _warnings) = addInstantiationTasks Map.empty origAdrs expanded
          consumerTasks = [ t
                          | (ComponentKey pn CLib, ADRToInstall t) <- result
                          , pn == consumerPn
                          ]
      case consumerTasks of
        [t] -> do
          let deps = t.configOpts.instantiationDeps
          length deps `shouldBe` 1
          case deps of
            [ComponentKey pn (CInst _)] -> pn `shouldBe` sigPn
            _ -> error "expected CInst dep on sig-pkg"
        _ -> error "expected exactly one consumer task"

    it "mixin referencing non-indefinite dep creates no CInst task" $ do
      let depPn = mkPackageName "dep-pkg"
          consumerPn = mkPackageName "consumer"
          -- dep-pkg has a library but NO signatures (not indefinite).
          depPkg = withExposingLibrary [mn "Foo"]
                 $ testPackage depPn Simple
          consumerPkg = withDeps [depPn]
                      $ withMixins [defaultMixin depPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          depTask = mockTask depPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (depPn, ADRToInstall depTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey depPn CLib, ADRToInstall depTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, _warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      instKeys `shouldBe` []

    it "multiple signatures produce single CInst with multiple entries" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str", mn "Num"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str", mn "Num"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, _warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instTasks = [ t
                      | (ComponentKey pn (CInst _), ADRToInstall t) <- result
                      , pn == sigPn
                      ]
      case instTasks of
        [t] -> length t.backpackInstEntries `shouldBe` 2
        _ -> error "expected exactly one CInst task"

    it "DefaultRenaming maps signatures to identity" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, _warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instTasks = [ t
                      | (ComponentKey pn (CInst _), ADRToInstall t) <- result
                      , pn == sigPn
                      ]
      case instTasks of
        [t] -> case t.backpackInstEntries of
          [(sig, _, implMod)] -> do
            -- With DefaultRenaming, sig name == impl module name.
            sig `shouldBe` mn "Str"
            implMod `shouldBe` mn "Str"
          _ -> error "expected one entry"
        _ -> error "expected one CInst task"

    it "CInst task's missing preserves sig-pkg's original missing" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          extraDepPn = mkPackageName "extra-dep"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          -- Give sigTask a pre-existing missing dep.
          extraDepPid = PackageIdentifier extraDepPn (mkVersion [2, 0])
          sigTask = (mockTask sigPkg)
            { configOpts = (mockTask sigPkg).configOpts
                { missing = Set.singleton extraDepPid }
            }
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, _warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instTasks = [ t
                      | (ComponentKey pn (CInst _), ADRToInstall t) <- result
                      , pn == sigPn
                      ]
      case instTasks of
        [t] -> do
          -- Should contain both the original extra-dep AND impl-pkg.
          Set.member extraDepPid t.configOpts.missing `shouldBe` True
          Set.member (packageIdentifier implPkg) t.configOpts.missing
            `shouldBe` True
        _ -> error "expected exactly one CInst task"

    it "explicit ModuleRenaming maps sig to renamed module" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          -- sig-pkg has signature "Str"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          -- impl-pkg exposes "Data.Text.Impl" (not "Str")
          implPkg = withExposingLibrary [mn "Data.Text.Impl"]
                  $ testPackage implPn Simple
          -- consumer uses explicit renaming: Str = Data.Text.Impl
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins
                      [explicitRequiresMixin sigPn [(mn "Str", mn "Data.Text.Impl")]]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, _warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instTasks = [ t
                      | (ComponentKey pn (CInst _), ADRToInstall t) <- result
                      , pn == sigPn
                      ]
      case instTasks of
        [t] -> case t.backpackInstEntries of
          [(sig, ipn, implMod)] -> do
            sig `shouldBe` mn "Str"
            ipn `shouldBe` implPn
            implMod `shouldBe` mn "Data.Text.Impl"
          _ -> error "expected one entry"
        _ -> error "expected one CInst task"

    it "HidingRenaming hiding nothing instantiates all sigs" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          hidingMixin = Mixin
            { mixinPackageName = sigPn
            , mixinLibraryName = LMainLibName
            , mixinIncludeRenaming = IncludeRenaming
                { includeProvidesRn = DefaultRenaming
                , includeRequiresRn = HidingRenaming []
                }
            }
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [hidingMixin]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      length instKeys `shouldBe` 1
      length warnings `shouldBe` 0

    it "HidingRenaming with actual hidden sigs produces no CInst" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str", mn "Num"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str", mn "Num"]
                  $ testPackage implPn Simple
          -- Hide "Str" — can't fully instantiate, so no CInst.
          hidingMixin = Mixin
            { mixinPackageName = sigPn
            , mixinLibraryName = LMainLibName
            , mixinIncludeRenaming = IncludeRenaming
                { includeProvidesRn = DefaultRenaming
                , includeRequiresRn = HidingRenaming [mn "Str"]
                }
            }
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [hidingMixin]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      -- Partial instantiation not possible — no CInst, no warning.
      instKeys `shouldBe` []
      length warnings `shouldBe` 0

    it "HidingRenaming hiding all sigs produces no CInst" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          hidingMixin = Mixin
            { mixinPackageName = sigPn
            , mixinLibraryName = LMainLibName
            , mixinIncludeRenaming = IncludeRenaming
                { includeProvidesRn = DefaultRenaming
                , includeRequiresRn = HidingRenaming [mn "Str"]
                }
            }
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [hidingMixin]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      instKeys `shouldBe` []
      length warnings `shouldBe` 0

    it "no CInst when no dep exposes the required module, with warning" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          -- impl-pkg exposes "Foo" not "Str"
          implPkg = withExposingLibrary [mn "Foo"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      instKeys `shouldBe` []
      length warnings `shouldBe` 1

    it "ADRFound dep for sig-pkg is skipped with warning" $ do
      let sigPn = mkPackageName "sig-pkg"
          consumerPn = mkPackageName "consumer"
          consumerPkg = withDeps [sigPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          consumerTask = mockTask consumerPkg
          sigPid = PackageIdentifier sigPn (mkVersion [1, 0])
          sigFound = ADRFound Snap (Library sigPid
                       (installedLibraryInfoFromGhcPkgId
                         (error "ghcPkgId not needed")))
          origAdrs =
            [ (sigPn, sigFound)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, sigFound)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      instKeys `shouldBe` []
      length warnings `shouldBe` 1

    it "successful resolution produces no warnings" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      length instKeys `shouldBe` 1
      warnings `shouldSatisfy` null

    it "CInst hash is deterministic for same entries" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result1, _) = addInstantiationTasks Map.empty origAdrs expanded
          (result2, _) = addInstantiationTasks Map.empty origAdrs expanded
          getHash rs =
            [ h | (ComponentKey _ (CInst h), _) <- rs ]
      getHash result1 `shouldBe` getHash result2
      getHash result1 `shouldSatisfy` (not . null)

    it "deduplicates CInst tasks from multiple consumer components" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          -- Consumer has both a library and an exe, both with the same mixin
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withExes ["demo"]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          -- Two consumer components in expanded: CLib and CExe
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            , ( ComponentKey consumerPn
                  (CExe (unqualCompFromString "demo"))
              , ADRToInstall consumerTask
              )
            ]
          (result, _warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      -- Should produce exactly one CInst entry, not two
      length instKeys `shouldBe` 1

    it "multiple deps exposing same module produces warning and no CInst" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn1 = mkPackageName "impl-a"
          implPn2 = mkPackageName "impl-b"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          -- Both impl packages expose the same module "Str"
          implPkg1 = withExposingLibrary [mn "Str"]
                   $ testPackage implPn1 Simple
          implPkg2 = withExposingLibrary [mn "Str"]
                   $ testPackage implPn2 Simple
          consumerPkg = withDeps [sigPn, implPn1, implPn2]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask1 = mockTask implPkg1
          implTask2 = mockTask implPkg2
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn1, ADRToInstall implTask1)
            , (implPn2, ADRToInstall implTask2)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn1 CLib, ADRToInstall implTask1)
            , (ComponentKey implPn2 CLib, ADRToInstall implTask2)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      instKeys `shouldBe` []
      length warnings `shouldBe` 1

    it "partial resolution: one sig resolves, one warns" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          -- sig-pkg has two signatures: Str and Map
          sigPkg = withIndefiniteLibrary [mn "Str", mn "Map"]
                 $ testPackage sigPn Simple
          -- impl-pkg only exposes Str, not Map
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instTasks = [ t
                      | (ComponentKey _ (CInst _), ADRToInstall t) <- result
                      ]
      -- Str resolves so CInst is created, but Map warns
      length instTasks `shouldBe` 1
      length (concatMap (.backpackInstEntries) instTasks) `shouldBe` 1
      length warnings `shouldBe` 1

    it "ModuleRenaming with unresolvable module produces warning" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          -- impl-pkg exposes Foo, not RenamedStr
          implPkg = withExposingLibrary [mn "Foo"]
                  $ testPackage implPn Simple
          renameMixin = Mixin
            { mixinPackageName = sigPn
            , mixinLibraryName = LMainLibName
            , mixinIncludeRenaming = IncludeRenaming
                { includeProvidesRn = DefaultRenaming
                , includeRequiresRn =
                    ModuleRenaming [(mn "Str", mn "RenamedStr")]
                }
            }
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [renameMixin]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      instKeys `shouldBe` []
      length warnings `shouldBe` 1

    it "non-indefinite mixin dep produces no warning" $ do
      let regularPn = mkPackageName "regular-pkg"
          consumerPn = mkPackageName "consumer"
          -- regular-pkg is NOT indefinite (no signatures)
          regularPkg = withExposingLibrary [mn "Foo"]
                     $ testPackage regularPn Simple
          consumerPkg = withDeps [regularPn]
                      $ withMixins [defaultMixin regularPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          regularTask = mockTask regularPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (regularPn, ADRToInstall regularTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey regularPn CLib, ADRToInstall regularTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      instKeys `shouldBe` []
      warnings `shouldSatisfy` null

    it "sub-library mixin creates CInst task" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          -- Consumer has the mixin on a sub-library, not the main library
          consumerPkg = withDeps [sigPn, implPn]
                      $ withSubLibMixins "internal" [defaultMixin sigPn]
                      $ withSubLibs ["internal"]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      length instKeys `shouldBe` 1
      warnings `shouldSatisfy` null

    it "impl module in sub-library is found" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          -- impl-pkg exposes Str only via a sub-library, not the main library
          implPkg = withExposingSubLib "internal" [mn "Str"]
                  $ withLibrary
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      length instKeys `shouldBe` 1
      warnings `shouldSatisfy` null

    it "sub-library mixin + sub-library impl module resolves" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          -- impl-pkg exposes Str only via sub-library
          implPkg = withExposingSubLib "str-impl" [mn "Str"]
                  $ withLibrary
                  $ testPackage implPn Simple
          -- consumer has mixin on its sub-library
          consumerPkg = withDeps [sigPn, implPn]
                      $ withSubLibMixins "uses-sig" [defaultMixin sigPn]
                      $ withSubLibs ["uses-sig"]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      length instKeys `shouldBe` 1
      warnings `shouldSatisfy` null

    it "main lib and sub-lib both with same mixin deduplicates CInst" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          -- Both main library and sub-library reference the same mixin
          consumerPkg = withDeps [sigPn, implPn]
                      $ withSubLibMixins "internal" [defaultMixin sigPn]
                      $ withSubLibs ["internal"]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      -- Same mixin → same hash → deduplicated to one CInst
      length instKeys `shouldBe` 1
      warnings `shouldSatisfy` null

    it "impl module exposed by both main lib and sub-lib is not ambiguous" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          -- impl-pkg exposes Str in both main library and sub-library
          implPkg = withExposingSubLib "extra" [mn "Str"]
                  $ withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      -- Same package exposes it — should resolve (not count as two candidates)
      length instKeys `shouldBe` 1
      warnings `shouldSatisfy` null

    it "sub-lib with no mixins produces no CInst" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          -- Sub-library exists but has no mixins; only main lib matters
          consumerPkg = withSubLibs ["internal"]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      instKeys `shouldBe` []
      warnings `shouldSatisfy` null

    it "transitive chain: inherited sigs filled in CInst" $ do
      -- str-sig (indefinite, sig: Str)
      -- logger-sig (indefinite, sig: Logger, depends on str-sig)
      -- impl-pkg (concrete, exposes: Str, Logger)
      -- consumer (mixins: logger-sig requires (Logger as Logger))
      -- logger-sig CInst must fill BOTH Logger (own) and Str (inherited)
      let strSigPn = mkPackageName "str-sig"
          loggerSigPn = mkPackageName "logger-sig"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          strSigPkg = withIndefiniteLibrary [mn "Str"]
                    $ testPackage strSigPn Simple
          loggerSigPkg = withIndefiniteLibraryDeps [mn "Logger"] [strSigPn]
                       $ testPackage loggerSigPn Simple
          implPkg = withExposingLibrary [mn "Str", mn "Logger"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [strSigPn, loggerSigPn, implPn]
                      $ withMixins [defaultMixin loggerSigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          strSigTask = mockTask strSigPkg
          loggerSigTask = mockTask loggerSigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (strSigPn, ADRToInstall strSigTask)
            , (loggerSigPn, ADRToInstall loggerSigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey strSigPn CLib, ADRToInstall strSigTask)
            , (ComponentKey loggerSigPn CLib, ADRToInstall loggerSigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instEntries = [ (ck, t.backpackInstEntries)
                        | (ck@(ComponentKey pn (CInst _)), ADRToInstall t)
                            <- result
                        , pn == loggerSigPn
                        ]
      -- Exactly one CInst for logger-sig
      length instEntries `shouldBe` 1
      -- The CInst must have entries for BOTH Logger and Str
      case instEntries of
        [(_, entries)] -> do
          let sigNames = map (\(s, _, _) -> s) entries
          sigNames `shouldSatisfy` (mn "Logger" `elem`)
          sigNames `shouldSatisfy` (mn "Str" `elem`)
        _ -> error "expected exactly one CInst"
      warnings `shouldSatisfy` null

    it "transitive chain: no inherited sigs from concrete dep" $ do
      -- sig-pkg (indefinite, sig: Str)
      -- concrete-dep (concrete lib, depends on sig-pkg in build-depends)
      -- consumer (mixins: sig-pkg)
      -- sig-pkg CInst should only fill Str (no inherited sigs from concrete)
      let sigPn = mkPackageName "sig-pkg"
          concretePn = mkPackageName "concrete-dep"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          -- concrete-dep is NOT indefinite — just a regular package
          concretePkg = withLibrary $ testPackage concretePn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          concreteTask = mockTask concretePkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (concretePn, ADRToInstall concreteTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey concretePn CLib, ADRToInstall concreteTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instEntries = [ (ck, t.backpackInstEntries)
                        | (ck@(ComponentKey pn (CInst _)), ADRToInstall t)
                            <- result
                        , pn == sigPn
                        ]
      length instEntries `shouldBe` 1
      case instEntries of
        [(_, entries)] -> do
          let sigNames = map (\(s, _, _) -> s) entries
          sigNames `shouldBe` [mn "Str"]
        _ -> error "expected exactly one CInst"
      warnings `shouldSatisfy` null

    it "three-level transitive chain fills all inherited sigs" $ do
      -- base-sig (indefinite, sig: Base)
      -- mid-sig (indefinite, sig: Mid, depends on base-sig)
      -- top-sig (indefinite, sig: Top, depends on mid-sig)
      -- impl (concrete, exposes: Base, Mid, Top)
      -- consumer (mixins: top-sig)
      -- top-sig CInst must fill Top (own) + Mid (from mid-sig) + Base (from
      -- base-sig, transitively)
      let baseSigPn = mkPackageName "base-sig"
          midSigPn = mkPackageName "mid-sig"
          topSigPn = mkPackageName "top-sig"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          baseSigPkg = withIndefiniteLibrary [mn "Base"]
                     $ testPackage baseSigPn Simple
          midSigPkg = withIndefiniteLibraryDeps [mn "Mid"] [baseSigPn]
                    $ testPackage midSigPn Simple
          topSigPkg = withIndefiniteLibraryDeps [mn "Top"] [midSigPn]
                    $ testPackage topSigPn Simple
          implPkg = withExposingLibrary [mn "Base", mn "Mid", mn "Top"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [baseSigPn, midSigPn, topSigPn, implPn]
                      $ withMixins [defaultMixin topSigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          baseSigTask = mockTask baseSigPkg
          midSigTask = mockTask midSigPkg
          topSigTask = mockTask topSigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (baseSigPn, ADRToInstall baseSigTask)
            , (midSigPn, ADRToInstall midSigTask)
            , (topSigPn, ADRToInstall topSigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey baseSigPn CLib, ADRToInstall baseSigTask)
            , (ComponentKey midSigPn CLib, ADRToInstall midSigTask)
            , (ComponentKey topSigPn CLib, ADRToInstall topSigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          topInstEntries = [ t.backpackInstEntries
                           | (ComponentKey pn (CInst _), ADRToInstall t)
                               <- result
                           , pn == topSigPn
                           ]
      length topInstEntries `shouldBe` 1
      case topInstEntries of
        [entries] -> do
          let sigNames = map (\(s, _, _) -> s) entries
          length sigNames `shouldBe` 3
          sigNames `shouldSatisfy` (mn "Top" `elem`)
          sigNames `shouldSatisfy` (mn "Mid" `elem`)
          sigNames `shouldSatisfy` (mn "Base" `elem`)
        _ -> error "expected exactly one CInst for top-sig"
      warnings `shouldSatisfy` null

    it "diamond deps: shared indefinite dep's sigs not duplicated" $ do
      -- common-sig (indefinite, sig: Common)
      -- a-sig (indefinite, sig: SigA, depends on common-sig)
      -- b-sig (indefinite, sig: SigB, depends on common-sig)
      -- top-sig (indefinite, sig: Top, depends on a-sig AND b-sig)
      -- impl (concrete, exposes: Common, SigA, SigB, Top)
      -- consumer (mixins: top-sig)
      -- top-sig CInst must fill Top + SigA + SigB + Common (no duplicates)
      let commonSigPn = mkPackageName "common-sig"
          aSigPn = mkPackageName "a-sig"
          bSigPn = mkPackageName "b-sig"
          topSigPn = mkPackageName "top-sig"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          commonSigPkg = withIndefiniteLibrary [mn "Common"]
                       $ testPackage commonSigPn Simple
          aSigPkg = withIndefiniteLibraryDeps [mn "SigA"] [commonSigPn]
                  $ testPackage aSigPn Simple
          bSigPkg = withIndefiniteLibraryDeps [mn "SigB"] [commonSigPn]
                  $ testPackage bSigPn Simple
          topSigPkg = withIndefiniteLibraryDeps [mn "Top"] [aSigPn, bSigPn]
                    $ testPackage topSigPn Simple
          implPkg = withExposingLibrary [mn "Common", mn "SigA", mn "SigB", mn "Top"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [commonSigPn, aSigPn, bSigPn, topSigPn, implPn]
                      $ withMixins [defaultMixin topSigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          commonSigTask = mockTask commonSigPkg
          aSigTask = mockTask aSigPkg
          bSigTask = mockTask bSigPkg
          topSigTask = mockTask topSigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (commonSigPn, ADRToInstall commonSigTask)
            , (aSigPn, ADRToInstall aSigTask)
            , (bSigPn, ADRToInstall bSigTask)
            , (topSigPn, ADRToInstall topSigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey commonSigPn CLib, ADRToInstall commonSigTask)
            , (ComponentKey aSigPn CLib, ADRToInstall aSigTask)
            , (ComponentKey bSigPn CLib, ADRToInstall bSigTask)
            , (ComponentKey topSigPn CLib, ADRToInstall topSigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          topInstEntries = [ t.backpackInstEntries
                           | (ComponentKey pn (CInst _), ADRToInstall t)
                               <- result
                           , pn == topSigPn
                           ]
      length topInstEntries `shouldBe` 1
      case topInstEntries of
        [entries] -> do
          let sigNames = map (\(s, _, _) -> s) entries
          -- Exactly 4 unique sigs: Top (own) + SigA + SigB + Common (inherited)
          -- Common should NOT appear twice despite diamond.
          length sigNames `shouldBe` 4
          sigNames `shouldSatisfy` (mn "Top" `elem`)
          sigNames `shouldSatisfy` (mn "SigA" `elem`)
          sigNames `shouldSatisfy` (mn "SigB" `elem`)
          sigNames `shouldSatisfy` (mn "Common" `elem`)
        _ -> error "expected exactly one CInst for top-sig"
      warnings `shouldSatisfy` null

    it "inherited sig not resolvable produces warning" $ do
      -- str-sig (indefinite, sig: Str)
      -- logger-sig (indefinite, sig: Logger, depends on str-sig)
      -- impl-pkg only exposes Logger, NOT Str
      -- consumer (mixins: logger-sig)
      -- The inherited Str cannot be resolved → warning
      let strSigPn = mkPackageName "str-sig"
          loggerSigPn = mkPackageName "logger-sig"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          strSigPkg = withIndefiniteLibrary [mn "Str"]
                    $ testPackage strSigPn Simple
          loggerSigPkg = withIndefiniteLibraryDeps [mn "Logger"] [strSigPn]
                       $ testPackage loggerSigPn Simple
          -- impl-pkg only has Logger, NOT Str
          implPkg = withExposingLibrary [mn "Logger"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [strSigPn, loggerSigPn, implPn]
                      $ withMixins [defaultMixin loggerSigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          strSigTask = mockTask strSigPkg
          loggerSigTask = mockTask loggerSigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (strSigPn, ADRToInstall strSigTask)
            , (loggerSigPn, ADRToInstall loggerSigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey strSigPn CLib, ADRToInstall strSigTask)
            , (ComponentKey loggerSigPn CLib, ADRToInstall loggerSigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instEntries = [ t.backpackInstEntries
                        | (ComponentKey pn (CInst _), ADRToInstall t)
                            <- result
                        , pn == loggerSigPn
                        ]
      -- CInst still created (for the Logger sig that resolves)
      length instEntries `shouldBe` 1
      case instEntries of
        [entries] -> do
          let sigNames = map (\(s, _, _) -> s) entries
          -- Only Logger resolved; Str could not be found
          sigNames `shouldBe` [mn "Logger"]
        _ -> error "expected exactly one CInst"
      -- Warning for unresolvable inherited sig
      length warnings `shouldBe` 1

    it "two consumers with different impls produce two CInst tasks" $ do
      -- sig-pkg (indefinite, sig: Str)
      -- impl-a (concrete, exposes: Str)
      -- impl-b (concrete, exposes: Str)
      -- consumer-a (mixins: sig-pkg, depends on impl-a)
      -- consumer-b (mixins: sig-pkg, depends on impl-b)
      -- Should produce TWO CInst tasks for sig-pkg with different hashes.
      let sigPn = mkPackageName "sig-pkg"
          implAPn = mkPackageName "impl-a"
          implBPn = mkPackageName "impl-b"
          consumerAPn = mkPackageName "consumer-a"
          consumerBPn = mkPackageName "consumer-b"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implAPkg = withExposingLibrary [mn "Str"]
                   $ testPackage implAPn Simple
          implBPkg = withExposingLibrary [mn "Str"]
                   $ testPackage implBPn Simple
          consumerAPkg = withDeps [sigPn, implAPn]
                       $ withMixins [defaultMixin sigPn]
                       $ withLibrary
                       $ testPackage consumerAPn Simple
          consumerBPkg = withDeps [sigPn, implBPn]
                       $ withMixins [defaultMixin sigPn]
                       $ withLibrary
                       $ testPackage consumerBPn Simple
          sigTask = mockTask sigPkg
          implATask = mockTask implAPkg
          implBTask = mockTask implBPkg
          consumerATask = mockTask consumerAPkg
          consumerBTask = mockTask consumerBPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implAPn, ADRToInstall implATask)
            , (implBPn, ADRToInstall implBTask)
            , (consumerAPn, ADRToInstall consumerATask)
            , (consumerBPn, ADRToInstall consumerBTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implAPn CLib, ADRToInstall implATask)
            , (ComponentKey implBPn CLib, ADRToInstall implBTask)
            , (ComponentKey consumerAPn CLib, ADRToInstall consumerATask)
            , (ComponentKey consumerBPn CLib, ADRToInstall consumerBTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey pn (CInst _)), _) <- result
                          , pn == sigPn
                     ]
      -- Two different CInst tasks for sig-pkg (different impl hashes)
      length instKeys `shouldBe` 2
      warnings `shouldSatisfy` null

    it "two consumers with same impl produce one CInst task (dedup)" $ do
      -- sig-pkg (indefinite, sig: Str)
      -- impl-pkg (concrete, exposes: Str)
      -- consumer-a (mixins: sig-pkg, depends on impl-pkg)
      -- consumer-b (mixins: sig-pkg, depends on impl-pkg)
      -- Both consumers use the same impl → same hash → ONE CInst task.
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerAPn = mkPackageName "consumer-a"
          consumerBPn = mkPackageName "consumer-b"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          consumerAPkg = withDeps [sigPn, implPn]
                       $ withMixins [defaultMixin sigPn]
                       $ withLibrary
                       $ testPackage consumerAPn Simple
          consumerBPkg = withDeps [sigPn, implPn]
                       $ withMixins [defaultMixin sigPn]
                       $ withLibrary
                       $ testPackage consumerBPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerATask = mockTask consumerAPkg
          consumerBTask = mockTask consumerBPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerAPn, ADRToInstall consumerATask)
            , (consumerBPn, ADRToInstall consumerBTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerAPn CLib, ADRToInstall consumerATask)
            , (ComponentKey consumerBPn CLib, ADRToInstall consumerBTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey pn (CInst _)), _) <- result
                          , pn == sigPn
                     ]
      -- Same impl → same hash → deduplicated to one CInst task
      length instKeys `shouldBe` 1
      warnings `shouldSatisfy` null

    it "impl out of consumer scope is not used for resolution" $ do
      -- sig-pkg (indefinite, sig: Str)
      -- impl-pkg (concrete, exposes: Str)  — exists in plan
      -- consumer (mixins: sig-pkg, depends on sig-pkg only, NOT impl-pkg)
      -- impl-pkg is out of consumer's build-depends scope → no resolution → warning
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey pn (CInst _)), _) <- result
                          , pn == sigPn
                     ]
      -- impl-pkg not in consumer's build-depends → can't resolve → no CInst
      length instKeys `shouldBe` 0
      length warnings `shouldBe` 1

    it "three consumers: two same impl, one different → two CInsts" $ do
      -- sig-pkg (indefinite, sig: Str)
      -- impl-a, impl-b (concrete, expose: Str)
      -- consumer-1 depends on impl-a, consumer-2 depends on impl-a,
      -- consumer-3 depends on impl-b
      -- → two CInst tasks (impl-a deduped, impl-b separate)
      let sigPn = mkPackageName "sig-pkg"
          implAPn = mkPackageName "impl-a"
          implBPn = mkPackageName "impl-b"
          c1Pn = mkPackageName "consumer-1"
          c2Pn = mkPackageName "consumer-2"
          c3Pn = mkPackageName "consumer-3"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implAPkg = withExposingLibrary [mn "Str"]
                   $ testPackage implAPn Simple
          implBPkg = withExposingLibrary [mn "Str"]
                   $ testPackage implBPn Simple
          c1Pkg = withDeps [sigPn, implAPn]
                $ withMixins [defaultMixin sigPn]
                $ withLibrary
                $ testPackage c1Pn Simple
          c2Pkg = withDeps [sigPn, implAPn]
                $ withMixins [defaultMixin sigPn]
                $ withLibrary
                $ testPackage c2Pn Simple
          c3Pkg = withDeps [sigPn, implBPn]
                $ withMixins [defaultMixin sigPn]
                $ withLibrary
                $ testPackage c3Pn Simple
          sigTask = mockTask sigPkg
          implATask = mockTask implAPkg
          implBTask = mockTask implBPkg
          c1Task = mockTask c1Pkg
          c2Task = mockTask c2Pkg
          c3Task = mockTask c3Pkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implAPn, ADRToInstall implATask)
            , (implBPn, ADRToInstall implBTask)
            , (c1Pn, ADRToInstall c1Task)
            , (c2Pn, ADRToInstall c2Task)
            , (c3Pn, ADRToInstall c3Task)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implAPn CLib, ADRToInstall implATask)
            , (ComponentKey implBPn CLib, ADRToInstall implBTask)
            , (ComponentKey c1Pn CLib, ADRToInstall c1Task)
            , (ComponentKey c2Pn CLib, ADRToInstall c2Task)
            , (ComponentKey c3Pn CLib, ADRToInstall c3Task)
            ]
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey pn (CInst _)), _) <- result
                          , pn == sigPn
                     ]
      -- impl-a used twice (deduped) + impl-b once = 2 CInst tasks
      length instKeys `shouldBe` 2
      warnings `shouldSatisfy` null

    it "ADRFound impl resolved via installedModules map" $ do
      -- sig-pkg is ADRToInstall (indefinite), impl-pkg is ADRFound
      -- (already installed). The installed modules map should allow
      -- module resolution for the ADRFound impl.
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          consumerTask = mockTask consumerPkg
          implPid = PackageIdentifier implPn (mkVersion [1, 0])
          implGid = unsafeParseGhcPkgId "impl-pkg-1.0-abc"
          implFound = ADRFound Snap (Library implPid
                        (installedLibraryInfoFromGhcPkgId implGid))
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, implFound)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, implFound)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          -- Provide the impl's modules via the installed modules map
          instMods = Map.singleton implPn (Set.singleton (mn "Str"))
          (result, warnings) = addInstantiationTasks instMods origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey pn (CInst _)), _) <- result
                          , pn == sigPn
                     ]
      -- CInst task should be created using the installed modules map
      length instKeys `shouldBe` 1
      warnings `shouldSatisfy` null
      -- Verify the impl's GhcPkgId is in the CInst task's present map
      let instPresent = [ t.present
                         | (ComponentKey pn (CInst _), ADRToInstall t) <- result
                         , pn == sigPn
                         ]
      case instPresent of
        [pm] -> Map.lookup implPid pm `shouldBe` Just implGid
        _    -> expectationFailure "Expected exactly one CInst task"

    it "ADRFound impl not in installedModules produces warning" $ do
      -- impl-pkg is ADRFound but the installedModules map is empty,
      -- so module resolution fails and a warning is produced.
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          consumerTask = mockTask consumerPkg
          implPid = PackageIdentifier implPn (mkVersion [1, 0])
          implGid = unsafeParseGhcPkgId "impl-pkg-1.0-abc"
          implFound = ADRFound Snap (Library implPid
                        (installedLibraryInfoFromGhcPkgId implGid))
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, implFound)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, implFound)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          -- Empty installed modules map — impl can't be resolved
          (result, warnings) = addInstantiationTasks Map.empty origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      instKeys `shouldBe` []
      length warnings `shouldBe` 1

    it "ADRFound impl not in consumer scope is ignored" $ do
      -- impl-pkg is ADRFound and in installedModules, but the consumer
      -- doesn't list it in build-depends, so it's out of scope.
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          -- Consumer depends on sig-pkg only, NOT impl-pkg
          consumerPkg = withDeps [sigPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          consumerTask = mockTask consumerPkg
          implPid = PackageIdentifier implPn (mkVersion [1, 0])
          implGid = unsafeParseGhcPkgId "impl-pkg-1.0-abc"
          implFound = ADRFound Snap (Library implPid
                        (installedLibraryInfoFromGhcPkgId implGid))
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, implFound)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, implFound)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          instMods = Map.singleton implPn (Set.singleton (mn "Str"))
          (result, warnings) = addInstantiationTasks instMods origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      instKeys `shouldBe` []
      length warnings `shouldBe` 1

    it "ADRFound + ADRToInstall both exposing module is ambiguous" $ do
      -- Two packages expose the same module: one ADRToInstall, one ADRFound.
      -- This should produce an ambiguity warning.
      let sigPn = mkPackageName "sig-pkg"
          impl1Pn = mkPackageName "impl1"
          impl2Pn = mkPackageName "impl2"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          impl1Pkg = withExposingLibrary [mn "Str"]
                   $ testPackage impl1Pn Simple
          consumerPkg = withDeps [sigPn, impl1Pn, impl2Pn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          impl1Task = mockTask impl1Pkg
          consumerTask = mockTask consumerPkg
          impl2Pid = PackageIdentifier impl2Pn (mkVersion [1, 0])
          impl2Gid = unsafeParseGhcPkgId "impl2-1.0-abc"
          impl2Found = ADRFound Snap (Library impl2Pid
                         (installedLibraryInfoFromGhcPkgId impl2Gid))
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (impl1Pn, ADRToInstall impl1Task)
            , (impl2Pn, impl2Found)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey impl1Pn CLib, ADRToInstall impl1Task)
            , (ComponentKey impl2Pn CLib, impl2Found)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          -- impl2 also exposes Str via installedModules
          instMods = Map.singleton impl2Pn (Set.singleton (mn "Str"))
          (result, warnings) = addInstantiationTasks instMods origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      -- Ambiguous → no CInst, warning produced
      instKeys `shouldBe` []
      length warnings `shouldBe` 1

    it "two ADRFound impls exposing same module is ambiguous" $ do
      let sigPn = mkPackageName "sig-pkg"
          impl1Pn = mkPackageName "impl1"
          impl2Pn = mkPackageName "impl2"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          consumerPkg = withDeps [sigPn, impl1Pn, impl2Pn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          consumerTask = mockTask consumerPkg
          impl1Pid = PackageIdentifier impl1Pn (mkVersion [1, 0])
          impl1Gid = unsafeParseGhcPkgId "impl1-1.0-aaa"
          impl1Found = ADRFound Snap (Library impl1Pid
                         (installedLibraryInfoFromGhcPkgId impl1Gid))
          impl2Pid = PackageIdentifier impl2Pn (mkVersion [1, 0])
          impl2Gid = unsafeParseGhcPkgId "impl2-1.0-bbb"
          impl2Found = ADRFound Snap (Library impl2Pid
                         (installedLibraryInfoFromGhcPkgId impl2Gid))
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (impl1Pn, impl1Found)
            , (impl2Pn, impl2Found)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey impl1Pn CLib, impl1Found)
            , (ComponentKey impl2Pn CLib, impl2Found)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          instMods = Map.fromList
            [ (impl1Pn, Set.singleton (mn "Str"))
            , (impl2Pn, Set.singleton (mn "Str"))
            ]
          (result, warnings) = addInstantiationTasks instMods origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      instKeys `shouldBe` []
      length warnings `shouldBe` 1

    it "ADRFound impl with wrong module in installedModules not matched" $ do
      -- impl-pkg is ADRFound and in installedModules, but exposes "Other"
      -- not "Str", so resolution fails.
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = mockTask sigPkg
          consumerTask = mockTask consumerPkg
          implPid = PackageIdentifier implPn (mkVersion [1, 0])
          implGid = unsafeParseGhcPkgId "impl-pkg-1.0-abc"
          implFound = ADRFound Snap (Library implPid
                        (installedLibraryInfoFromGhcPkgId implGid))
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, implFound)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, implFound)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          -- impl exposes "Other", not "Str"
          instMods = Map.singleton implPn (Set.singleton (mn "Other"))
          (result, warnings) = addInstantiationTasks instMods origAdrs expanded
          instKeys = [ ck | (ck@(ComponentKey _ (CInst _)), _) <- result ]
      instKeys `shouldBe` []
      length warnings `shouldBe` 1

    it "CInst inherits buildHaddocks = True from sig task" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = (mockTask sigPkg) { buildHaddocks = True }
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, _) = addInstantiationTasks Map.empty origAdrs expanded
          instHaddocks = [ t.buildHaddocks
                         | (ComponentKey pn (CInst _), ADRToInstall t)
                             <- result
                         , pn == sigPn
                         ]
      instHaddocks `shouldBe` [True]

    it "CInst inherits buildHaddocks = False from sig task" $ do
      let sigPn = mkPackageName "sig-pkg"
          implPn = mkPackageName "impl-pkg"
          consumerPn = mkPackageName "consumer"
          sigPkg = withIndefiniteLibrary [mn "Str"]
                 $ testPackage sigPn Simple
          implPkg = withExposingLibrary [mn "Str"]
                  $ testPackage implPn Simple
          consumerPkg = withDeps [sigPn, implPn]
                      $ withMixins [defaultMixin sigPn]
                      $ withLibrary
                      $ testPackage consumerPn Simple
          sigTask = (mockTask sigPkg) { buildHaddocks = False }
          implTask = mockTask implPkg
          consumerTask = mockTask consumerPkg
          origAdrs =
            [ (sigPn, ADRToInstall sigTask)
            , (implPn, ADRToInstall implTask)
            , (consumerPn, ADRToInstall consumerTask)
            ]
          expanded =
            [ (ComponentKey sigPn CLib, ADRToInstall sigTask)
            , (ComponentKey implPn CLib, ADRToInstall implTask)
            , (ComponentKey consumerPn CLib, ADRToInstall consumerTask)
            ]
          (result, _) = addInstantiationTasks Map.empty origAdrs expanded
          instHaddocks = [ t.buildHaddocks
                         | (ComponentKey pn (CInst _), ADRToInstall t)
                             <- result
                         , pn == sigPn
                         ]
      instHaddocks `shouldBe` [False]

  describe "findGhcPkgId" $ do
    it "finds matching GhcPkgId by package name" $ do
      let implGid = unsafeParseGhcPkgId "impl-pkg-0.1.0.0-abc123"
          depsMap = Map.fromList
            [ (PackageIdentifier (mkPackageName "impl-pkg") (mkVersion [0,1,0,0])
              , implGid)
            ]
      findGhcPkgId depsMap (mkPackageName "impl-pkg") `shouldBe` Just implGid

    it "returns Nothing for missing package" $ do
      let depsMap = Map.fromList
            [ (PackageIdentifier (mkPackageName "other") (mkVersion [1,0])
              , unsafeParseGhcPkgId "other-1.0-xyz")
            ]
      findGhcPkgId depsMap (mkPackageName "impl-pkg") `shouldBe` Nothing

    it "returns first match when multiple versions exist" $ do
      let gid1 = unsafeParseGhcPkgId "pkg-1.0-aaa"
          gid2 = unsafeParseGhcPkgId "pkg-2.0-bbb"
          depsMap = Map.fromList
            [ (PackageIdentifier (mkPackageName "pkg") (mkVersion [1,0]), gid1)
            , (PackageIdentifier (mkPackageName "pkg") (mkVersion [2,0]), gid2)
            ]
      -- Map.toList iterates in key order; (pkg-1.0) < (pkg-2.0)
      findGhcPkgId depsMap (mkPackageName "pkg") `shouldBe` Just gid1

    it "returns Nothing for empty deps map" $ do
      findGhcPkgId Map.empty (mkPackageName "anything") `shouldBe` Nothing

  describe "mkInstantiateWithOpts" $ do
    it "produces correct --instantiate-with flag" $ do
      let implGid = unsafeParseGhcPkgId "impl-pkg-0.1.0.0-abc123"
          depsMap = Map.fromList
            [ (PackageIdentifier (mkPackageName "impl-pkg") (mkVersion [0,1,0,0])
              , implGid)
            ]
          entries = [(mn "Str", mkPackageName "impl-pkg", mn "Str")]
          result = mkInstantiateWithOpts entries depsMap
      result `shouldBe`
        ["--instantiate-with=Str=impl-pkg-0.1.0.0-abc123:Str"]

    it "produces empty list when no entries" $ do
      mkInstantiateWithOpts [] Map.empty `shouldBe` []

    it "skips entry when implementing package not in deps map" $ do
      let entries = [(mn "Str", mkPackageName "missing-pkg", mn "Str")]
          depsMap = Map.fromList
            [ (PackageIdentifier (mkPackageName "other") (mkVersion [1,0])
              , unsafeParseGhcPkgId "other-1.0-xyz")
            ]
      mkInstantiateWithOpts entries depsMap `shouldBe` []

    it "handles multiple instantiation entries" $ do
      let gid1 = unsafeParseGhcPkgId "impl-a-1.0-aaa"
          gid2 = unsafeParseGhcPkgId "impl-b-1.0-bbb"
          depsMap = Map.fromList
            [ (PackageIdentifier (mkPackageName "impl-a") (mkVersion [1,0]), gid1)
            , (PackageIdentifier (mkPackageName "impl-b") (mkVersion [1,0]), gid2)
            ]
          entries =
            [ (mn "SigA", mkPackageName "impl-a", mn "ModA")
            , (mn "SigB", mkPackageName "impl-b", mn "ModB")
            ]
          result = mkInstantiateWithOpts entries depsMap
      length result `shouldBe` 2
      result `shouldBe`
        [ "--instantiate-with=SigA=impl-a-1.0-aaa:ModA"
        , "--instantiate-with=SigB=impl-b-1.0-bbb:ModB"
        ]

    it "handles renamed module (sigName /= implModuleName)" $ do
      let implGid = unsafeParseGhcPkgId "impl-pkg-1.0-abc"
          depsMap = Map.fromList
            [ (PackageIdentifier (mkPackageName "impl-pkg") (mkVersion [1,0])
              , implGid)
            ]
          entries = [(mn "Signature", mkPackageName "impl-pkg", mn "ConcreteImpl")]
          result = mkInstantiateWithOpts entries depsMap
      result `shouldBe`
        ["--instantiate-with=Signature=impl-pkg-1.0-abc:ConcreteImpl"]

    it "handles hierarchical module names" $ do
      let implGid = unsafeParseGhcPkgId "impl-pkg-1.0-abc"
          depsMap = Map.fromList
            [ (PackageIdentifier (mkPackageName "impl-pkg") (mkVersion [1,0])
              , implGid)
            ]
          entries =
            [(mn "Data.Map.Sig", mkPackageName "impl-pkg", mn "Data.Map.Strict")]
          result = mkInstantiateWithOpts entries depsMap
      result `shouldBe`
        ["--instantiate-with=Data.Map.Sig=impl-pkg-1.0-abc:Data.Map.Strict"]

  describe "ConfigCacheType persistence" $ do
    it "round-trips ConfigCacheTypeConfig" $ do
      let v = ConfigCacheTypeConfig
      fromPersistValue (toPersistValue v) `shouldBe` Right v

    it "round-trips ConfigCacheTypeFlagLibrary" $ do
      let gid = unsafeParseGhcPkgId "foo-1.0-abc123"
          v = ConfigCacheTypeFlagLibrary gid
      fromPersistValue (toPersistValue v) `shouldBe` Right v

    it "round-trips ConfigCacheTypeFlagExecutable" $ do
      let pid = PackageIdentifier (mkPackageName "bar") (mkVersion [2,0])
          v = ConfigCacheTypeFlagExecutable pid
      fromPersistValue (toPersistValue v) `shouldBe` Right v

    it "round-trips ConfigCacheTypeInstantiation" $ do
      let v = ConfigCacheTypeInstantiation "941095d7fd7eb1e4"
      fromPersistValue (toPersistValue v) `shouldBe` Right v

    it "round-trips ConfigCacheTypeInstantiation with empty suffix" $ do
      let v = ConfigCacheTypeInstantiation ""
      fromPersistValue (toPersistValue v) `shouldBe` Right v

    it "rejects unknown cache type" $ do
      fromPersistValue (PersistText "unknown:foo")
        `shouldSatisfy` \case
          Left _ -> True
          Right (_ :: ConfigCacheType) -> False

-- | Parse a GhcPkgId from text, throwing an error on failure.
unsafeParseGhcPkgId :: Text -> GhcPkgId
unsafeParseGhcPkgId t = case parseGhcPkgId t of
  Just gid -> gid
  Nothing -> error $ "unsafeParseGhcPkgId: invalid GhcPkgId: " ++ show t

-- | Helper: compute the list of buildable components for a package (same
-- logic as expandToComponentKeys).
packageBuildableComponents :: Package -> [NamedComponent]
packageBuildableComponents pkg =
  let libComps = [CLib | isJust pkg.library && buildableLib]
      subLibComps =
        map CSubLib $ Set.toList $ getBuildableSet pkg.subLibraries
      flibComps =
        map CFlib $ Set.toList $ getBuildableSet pkg.foreignLibraries
      exeComps =
        map CExe $ Set.toList $ getBuildableSet pkg.executables
  in  libComps ++ subLibComps ++ flibComps ++ exeComps
 where
  buildableLib = case pkg.library of
    Just lib -> lib.buildInfo.buildable
    Nothing -> False

-- | Helper: simulate component key expansion for build tasks.
packageToComponentKeys :: PackageName -> Package -> [ComponentKey]
packageToComponentKeys pn pkg
  | shouldSplitComponents pkg =
      case packageBuildableComponents pkg of
        [] -> [ComponentKey pn CLib]
        comps -> map (ComponentKey pn) comps
  | otherwise = [ComponentKey pn CLib]

-- | Helper: simulate final key expansion (mirrors addFinal logic).
packageToFinalKeys ::
     PackageName -> Package -> Set NamedComponent -> [ComponentKey]
packageToFinalKeys pn pkg components
  | shouldSplitComponents pkg =
      let testComps = filter isCTest $ Set.toList components
          benchComps = filter isCBench $ Set.toList components
      in  case testComps ++ benchComps of
            [] -> [ComponentKey pn CLib]
            comps -> map (ComponentKey pn) comps
  | otherwise = [ComponentKey pn CLib]
 where
  isCTest (CTest _) = True
  isCTest _ = False
  isCBench (CBench _) = True
  isCBench _ = False

-- | Build a mock Task wrapping a Package. Uses 'error' for TaskConfigOpts
-- fields that addInstantiationTasks never accesses (envConfig, baseConfigOpts,
-- pkgConfigOpts).
mockTask :: Package -> Task
mockTask pkg = Task
  { taskType = TTRemotePackage Immutable pkg
      (error "mockTask: PackageLocationImmutable not needed")
  , configOpts = TaskConfigOpts
      { missing = Set.empty
      , envConfig = error "mockTask: envConfig not needed"
      , baseConfigOpts = error "mockTask: baseConfigOpts not needed"
      , isLocalNonExtraDep = False
      , isMutable = Immutable
      , pkgConfigOpts = error "mockTask: pkgConfigOpts not needed"
      , instantiationDeps = []
      }
  , buildHaddocks = False
  , present = Map.empty
  , cachePkgSrc = CacheSrcUpstream
  , buildTypeConfig = False
  , backpackInstEntries = []
  }

-- | Add a library with exposed modules (for testing module resolution).
withExposingLibrary :: [ModuleName] -> Package -> Package
withExposingLibrary mods pkg = pkg
  { library = Just StackLibrary
      { name = emptyCompName
      , buildInfo = testBuildInfo
      , exposedModules = mods
      , signatures = []
      }
  }

-- | Add mixins to a package's main library buildInfo.
withMixins :: [Mixin] -> Package -> Package
withMixins ms pkg = case pkg.library of
  Just (StackLibrary n bi em sigs) -> pkg
    { library = Just (StackLibrary n bi { mixins = ms } em sigs)
    }
  Nothing -> pkg

-- | Helper: create a simple mixin with default renaming.
defaultMixin :: PackageName -> Mixin
defaultMixin pn = Mixin
  { mixinPackageName = pn
  , mixinLibraryName = LMainLibName
  , mixinIncludeRenaming = defaultIncludeRenaming
  }

-- | Helper: create a mixin with explicit requires renaming.
explicitRequiresMixin ::
     PackageName
  -> [(ModuleName, ModuleName)]
  -> Mixin
explicitRequiresMixin pn reqs = Mixin
  { mixinPackageName = pn
  , mixinLibraryName = LMainLibName
  , mixinIncludeRenaming = IncludeRenaming
      { includeProvidesRn = DefaultRenaming
      , includeRequiresRn = ModuleRenaming reqs
      }
  }

-- | Add a sub-library with specific exposed modules.
withExposingSubLib :: String -> [ModuleName] -> Package -> Package
withExposingSubLib name mods pkg = pkg
  { subLibraries = foldAndMakeCollection id $
      StackLibrary
        { name = unqualCompFromString name
        , buildInfo = testBuildInfo
        , exposedModules = mods
        , signatures = []
        }
      : toList pkg.subLibraries
  }

-- | Add mixins to a specific sub-library's buildInfo.
withSubLibMixins :: String -> [Mixin] -> Package -> Package
withSubLibMixins subLibName ms pkg = pkg
  { subLibraries = foldAndMakeCollection id
      [ if lib.name == unqualCompFromString subLibName
          then let bi = lib.buildInfo :: StackBuildInfo
               in  StackLibrary lib.name (bi { mixins = ms })
                                lib.exposedModules lib.signatures
          else lib
      | lib <- toList pkg.subLibraries
      ]
  }

-- | Create an indefinite library package that depends on other packages.
-- Used to test transitive Backpack chains where one indefinite package
-- depends on another.
withIndefiniteLibraryDeps :: [ModuleName] -> [PackageName] -> Package -> Package
withIndefiniteLibraryDeps sigs depPkgs pkg = pkg
  { library = Just StackLibrary
      { name = emptyCompName
      , buildInfo = testBuildInfo
          { dependency = Map.fromList
              [ (pn, DepValue
                  { versionRange = anyVersion
                  , depType = AsLibrary DepLibrary
                      { main = True, subLib = Set.empty }
                  })
              | pn <- depPkgs
              ]
          }
      , exposedModules = []
      , signatures = sigs
      }
  }

-- | Add build-depends on the given packages to a package's main library.
-- This is needed so that module resolution is scoped to the consumer's deps.
withDeps :: [PackageName] -> Package -> Package
withDeps depPkgs pkg = case pkg.library of
  Just (StackLibrary n bi em sigs) ->
    let newDeps = Map.fromList
          [ (pn, DepValue
              { versionRange = anyVersion
              , depType = AsLibrary DepLibrary
                  { main = True, subLib = Set.empty }
              })
          | pn <- depPkgs
          ]
        bi' = bi { dependency = Map.union bi.dependency newDeps }
    in  pkg { library = Just (StackLibrary n bi' em sigs) }
  Nothing -> pkg

-- | Convenience: make a ModuleName from a String.
mn :: String -> ModuleName
mn = Cabal.fromString
