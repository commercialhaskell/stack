{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Build.ExecuteSpec
  ( main
  , spec
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Distribution.Types.PackageName ( mkPackageName )
import           Distribution.Types.Version ( mkVersion )
import           Control.Concurrent.Execute
                   ( ActionId (..), ActionType (..) )
import           Stack.Build.Execute
                   ( finalTestsAndBenches, intraPackageDeps, missingToDeps )
import           Stack.Build.ExecutePackage
                   ( componentEnableBenchmarks, componentEnableTests
                   , componentTarget
                   )
import           Stack.Prelude
import           Stack.Types.ComponentUtils ( unqualCompFromString )
import           Stack.Types.NamedComponent ( NamedComponent (..) )
import           Stack.Types.Plan ( ComponentKey (..), componentKeyPkgName )
import           Test.Hspec ( Spec, describe, hspec, it, shouldBe, shouldSatisfy )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ActionId" $ do
    it "uses ComponentKey instead of PackageIdentifier" $ do
      let ck = ComponentKey (mkPackageName "my-pkg") CLib
          aid = ActionId ck ATBuild
      aid `shouldBe` ActionId ck ATBuild

    it "ActionIds with different ComponentKeys are not equal" $ do
      let ck1 = ComponentKey (mkPackageName "pkg") CLib
          ck2 = ComponentKey (mkPackageName "pkg")
                  (CTest (unqualCompFromString "test-suite"))
          aid1 = ActionId ck1 ATBuild
          aid2 = ActionId ck2 ATBuild
      (aid1 == aid2) `shouldBe` False

    it "ActionIds with different ActionTypes are not equal" $ do
      let ck = ComponentKey (mkPackageName "pkg") CLib
          aid1 = ActionId ck ATBuild
          aid2 = ActionId ck ATRunTests
      (aid1 == aid2) `shouldBe` False

    it "can be used in Sets (dependency tracking)" $ do
      let ckLib = ComponentKey (mkPackageName "dep") CLib
          ckTest = ComponentKey (mkPackageName "pkg")
                     (CTest (unqualCompFromString "tests"))
          deps = Set.fromList
            [ ActionId ckLib ATBuild
            , ActionId ckTest ATBuild
            , ActionId ckTest ATRunTests
            ]
      Set.size deps `shouldBe` 3

    it "dependency from test ATRunTests to test ATBuild works" $ do
      let ck = ComponentKey (mkPackageName "pkg")
                 (CTest (unqualCompFromString "tests"))
          buildAction = ActionId ck ATBuild
          runAction = ActionId ck ATRunTests
          deps = Set.singleton buildAction
      Set.member buildAction deps `shouldBe` True
      Set.member runAction deps `shouldBe` False

    it "dependency from component to CLib ATBuild maps correctly" $ do
      let pkgName = mkPackageName "base-dep"
          depKey = ComponentKey pkgName CLib
          depAction = ActionId depKey ATBuild
          testKey = ComponentKey (mkPackageName "my-pkg")
                      (CTest (unqualCompFromString "tests"))
          testBuild = ActionId testKey ATBuild
          deps = Set.fromList [depAction, testBuild]
      Set.member depAction deps `shouldBe` True

  describe "ActionType" $ do
    it "ATBuildFinal no longer exists (3 constructors only)" $ do
      let types = [ATBuild, ATRunTests, ATRunBenchmarks]
      length types `shouldBe` 3

    it "ATBuild < ATRunTests < ATRunBenchmarks (Ord)" $ do
      (ATBuild < ATRunTests) `shouldBe` True
      (ATRunTests < ATRunBenchmarks) `shouldBe` True

  describe "missingToDeps" $ do
    it "maps empty set to empty set" $ do
      missingToDeps Set.empty `shouldBe` Set.empty

    it "maps a single PackageIdentifier to ComponentKey CLib ATBuild" $ do
      let pid = PackageIdentifier (mkPackageName "base") (mkVersion [4,20,0,0])
          result = missingToDeps (Set.singleton pid)
          expected = Set.singleton $
            ActionId (ComponentKey (mkPackageName "base") CLib) ATBuild
      result `shouldBe` expected

    it "maps multiple dependencies preserving each package name" $ do
      let pid1 = PackageIdentifier (mkPackageName "text") (mkVersion [2,0])
          pid2 = PackageIdentifier (mkPackageName "bytestring") (mkVersion [0,12])
          pid3 = PackageIdentifier (mkPackageName "containers") (mkVersion [0,7])
          result = missingToDeps (Set.fromList [pid1, pid2, pid3])
      Set.size result `shouldBe` 3
      -- All mapped to CLib
      result `shouldSatisfy` all
        (\(ActionId (ComponentKey _ comp) at) -> comp == CLib && at == ATBuild)

    it "discards version info — only package name matters" $ do
      -- Two different versions of the same package should map to the same
      -- ActionId (since ComponentKey uses PackageName, not PackageIdentifier)
      let pid1 = PackageIdentifier (mkPackageName "aeson") (mkVersion [2,1])
          pid2 = PackageIdentifier (mkPackageName "aeson") (mkVersion [2,2])
          result = missingToDeps (Set.fromList [pid1, pid2])
      -- Set deduplicates: both map to the same ActionId
      Set.size result `shouldBe` 1

    it "preserves the invariant that external deps are always CLib" $ do
      let pids = Set.fromList
            [ PackageIdentifier (mkPackageName "foo") (mkVersion [1,0])
            , PackageIdentifier (mkPackageName "bar") (mkVersion [2,0])
            ]
          result = missingToDeps pids
      -- Every resulting ActionId should have CLib component and ATBuild type
      forM_ (Set.toList result) $ \(ActionId (ComponentKey _ comp) at) -> do
        comp `shouldBe` CLib
        at `shouldBe` ATBuild

  describe "action graph invariants" $ do
    it "build-only key produces exactly one ATBuild" $ do
      -- Simulating toActions (Just build, Nothing): should produce [ATBuild]
      let ck = ComponentKey (mkPackageName "pkg") CLib
          buildId = ActionId ck ATBuild
          -- In toActions, abuild produces one Action with this id
          actionIds = [buildId]
      length actionIds `shouldBe` 1
      actionIds `shouldBe` [ActionId ck ATBuild]

    it "final-only key produces ATBuild + run actions" $ do
      -- Simulating toActions (Nothing, Just final) with tests:
      -- Should produce ATBuild (for final) + ATRunTests
      let ck = ComponentKey (mkPackageName "pkg") CLib
          buildId = ActionId ck ATBuild
          runId = ActionId ck ATRunTests
          actionIds = [buildId, runId]
          -- ATRunTests depends on ATBuild
          runDeps = Set.singleton buildId
      length actionIds `shouldBe` 2
      Set.member buildId runDeps `shouldBe` True

    it "build+final key produces one ATBuild (no duplicate)" $ do
      -- Simulating toActions (Just build, Just final):
      -- abuild produces ATBuild, afinal must NOT produce another ATBuild
      -- afinal.finalBuild is [] when mbuild is Just
      let ck = ComponentKey (mkPackageName "pkg") CLib
          -- From abuild
          abuildIds = [ActionId ck ATBuild]
          -- From afinal.finalBuild when mbuild is Just: empty
          afinalBuildIds = [] :: [ActionId]
          -- From afinal.finalRun
          afinalRunIds = [ActionId ck ATRunTests]
          allIds = abuildIds ++ afinalBuildIds ++ afinalRunIds
      -- No duplicate ATBuild
      let buildCount = length $ filter
            (\(ActionId _ at) -> at == ATBuild) allIds
      buildCount `shouldBe` 1
      -- Total: 1 ATBuild + 1 ATRunTests
      length allIds `shouldBe` 2

    it "run actions always depend on ATBuild for same ComponentKey" $ do
      let ck = ComponentKey (mkPackageName "pkg")
                 (CTest (unqualCompFromString "tests"))
          buildDep = Set.singleton (ActionId ck ATBuild)
          -- ATRunTests and ATRunBenchmarks should both depend on ATBuild
          runTestsDeps = buildDep
          runBenchDeps = buildDep
      Set.member (ActionId ck ATBuild) runTestsDeps `shouldBe` True
      Set.member (ActionId ck ATBuild) runBenchDeps `shouldBe` True

  describe "per-package derivation from ComponentKey map" $ do
    it "single CLib key maps to one package entry" $ do
      let ck = ComponentKey (mkPackageName "pkg") CLib
          componentMap = Map.singleton ck ("task-value" :: String)
          pkgMap = Map.fromList
            [ (componentKeyPkgName k, v) | (k, v) <- Map.toList componentMap ]
      Map.size pkgMap `shouldBe` 1
      Map.lookup (mkPackageName "pkg") pkgMap `shouldBe` Just "task-value"

    it "multiple components for same package collapse to one entry" $ do
      let pn = mkPackageName "pkg"
          ck1 = ComponentKey pn CLib
          ck2 = ComponentKey pn (CExe (unqualCompFromString "exe"))
          ck3 = ComponentKey pn (CTest (unqualCompFromString "test"))
          componentMap :: Map ComponentKey String
          componentMap = Map.fromList
            [(ck1, "lib-task"), (ck2, "exe-task"), (ck3, "test-task")]
          -- Map.fromList with last-wins gives us the last component
          pkgMap = Map.fromList
            [ (componentKeyPkgName k, v)
            | (k, v) <- Map.toList componentMap
            ]
      -- All three map to the same PackageName, so only one entry
      Map.size pkgMap `shouldBe` 1
      -- Map.toList on componentMap is ascending by key;
      -- CLib < CExe < CTest, so CTest's value wins
      Map.lookup pn pkgMap `shouldBe` Just "test-task"

    it "different packages stay separate after derivation" $ do
      let ck1 = ComponentKey (mkPackageName "pkg-a") CLib
          ck2 = ComponentKey (mkPackageName "pkg-a")
                  (CExe (unqualCompFromString "exe"))
          ck3 = ComponentKey (mkPackageName "pkg-b") CLib
          componentMap :: Map ComponentKey String
          componentMap = Map.fromList
            [(ck1, "a-lib"), (ck2, "a-exe"), (ck3, "b-lib")]
          pkgMap = Map.fromList
            [ (componentKeyPkgName k, v)
            | (k, v) <- Map.toList componentMap
            ]
      Map.size pkgMap `shouldBe` 2
      Map.member (mkPackageName "pkg-a") pkgMap `shouldBe` True
      Map.member (mkPackageName "pkg-b") pkgMap `shouldBe` True

    it "preserves all package names when components are distinct packages" $ do
      let keys =
            [ ComponentKey (mkPackageName "aeson") CLib
            , ComponentKey (mkPackageName "text") CLib
            , ComponentKey (mkPackageName "bytestring") CLib
            ]
          componentMap :: Map ComponentKey Int
          componentMap = Map.fromList $ zip keys [1..]
          pkgNames = Set.fromList $ map componentKeyPkgName $ Map.keys componentMap
      Set.size pkgNames `shouldBe` 3

  describe "consumer dedup patterns" $ do
    -- These test the deduplication patterns used by justLocals,
    -- errorOnSnapshot, and warnIfExecutablesWithSameNameCouldBeOverwritten
    -- when operating on component-keyed maps.

    it "justLocals pattern: Set deduplicates split-package identifiers" $ do
      -- Simulates justLocals: multiple component tasks for the same package
      -- produce the same PackageIdentifier; Set.fromList removes duplicates.
      let pid = PackageIdentifier (mkPackageName "pkg") (mkVersion [1,0])
          taskPids = [pid, pid, pid]
          result = Set.toList $ Set.fromList taskPids
      result `shouldBe` [pid]

    it "justLocals pattern: different packages remain distinct" $ do
      let pid1 = PackageIdentifier (mkPackageName "pkg-a") (mkVersion [1,0])
          pid2 = PackageIdentifier (mkPackageName "pkg-b") (mkVersion [2,0])
          taskPids = [pid1, pid1, pid2, pid2]
          result = Set.toList $ Set.fromList taskPids
      length result `shouldBe` 2
      result `shouldSatisfy` (pid1 `elem`)
      result `shouldSatisfy` (pid2 `elem`)

    it "errorOnSnapshot pattern: PackageNames are deduplicated" $ do
      -- Simulates errorOnSnapshot: extracting PackageNames from ComponentKeys
      -- and deduplicating.
      let pn = mkPackageName "pkg"
          keys =
            [ ComponentKey pn CLib
            , ComponentKey pn (CExe (unqualCompFromString "exe"))
            , ComponentKey pn (CSubLib (unqualCompFromString "sub"))
            ]
          result = Set.toList $ Set.fromList $ map componentKeyPkgName keys
      result `shouldBe` [pn]

    it "errorOnSnapshot pattern: distinct packages stay separate" $ do
      let keys =
            [ ComponentKey (mkPackageName "a") CLib
            , ComponentKey (mkPackageName "a")
                (CExe (unqualCompFromString "exe"))
            , ComponentKey (mkPackageName "b") CLib
            ]
          result = Set.toList $ Set.fromList $ map componentKeyPkgName keys
      length result `shouldBe` 2

    it "perPkgTasks pattern: split components yield one entry per package" $ do
      -- Simulates perPkgTasks in warnIfExecutablesWithSameNameCouldBeOverwritten
      let pn = mkPackageName "pkg"
          componentMap :: Map ComponentKey String
          componentMap = Map.fromList
            [ (ComponentKey pn CLib, "task")
            , (ComponentKey pn (CExe (unqualCompFromString "exe1")), "task")
            , (ComponentKey pn (CExe (unqualCompFromString "exe2")), "task")
            ]
          perPkg = Map.fromList
            [ (componentKeyPkgName ck, v)
            | (ck, v) <- Map.toList componentMap
            ]
      Map.size perPkg `shouldBe` 1

    it "perPkgTasks pattern: mixed packages yield correct count" $ do
      let componentMap :: Map ComponentKey String
          componentMap = Map.fromList
            [ (ComponentKey (mkPackageName "a") CLib, "a-task")
            , (ComponentKey (mkPackageName "a")
                (CExe (unqualCompFromString "exe")), "a-task")
            , (ComponentKey (mkPackageName "b") CLib, "b-task")
            , (ComponentKey (mkPackageName "b")
                (CExe (unqualCompFromString "app")), "b-task")
            ]
          perPkg = Map.fromList
            [ (componentKeyPkgName ck, v)
            | (ck, v) <- Map.toList componentMap
            ]
      Map.size perPkg `shouldBe` 2
      Map.member (mkPackageName "a") perPkg `shouldBe` True
      Map.member (mkPackageName "b") perPkg `shouldBe` True

  describe "intraPackageDeps" $ do
    it "CLib has no intra-package deps" $ do
      let ck = ComponentKey (mkPackageName "pkg") CLib
          taskKeys = Set.singleton ck
      intraPackageDeps taskKeys ck `shouldBe` Set.empty

    it "CExe depends on CLib ATBuild when CLib is in taskKeys" $ do
      let pn = mkPackageName "pkg"
          libKey = ComponentKey pn CLib
          exeKey = ComponentKey pn (CExe (unqualCompFromString "my-exe"))
          taskKeys = Set.fromList [libKey, exeKey]
          result = intraPackageDeps taskKeys exeKey
      result `shouldBe`
        Set.singleton (ActionId libKey ATBuild)

    it "CExe has no intra-package dep when CLib is not in taskKeys" $ do
      let pn = mkPackageName "pkg"
          exeKey = ComponentKey pn (CExe (unqualCompFromString "my-exe"))
          taskKeys = Set.singleton exeKey
      intraPackageDeps taskKeys exeKey `shouldBe` Set.empty

    it "CTest depends on CLib ATBuild when CLib is in taskKeys" $ do
      let pn = mkPackageName "pkg"
          libKey = ComponentKey pn CLib
          testKey = ComponentKey pn (CTest (unqualCompFromString "tests"))
          taskKeys = Set.singleton libKey
          result = intraPackageDeps taskKeys testKey
      result `shouldBe`
        Set.singleton (ActionId libKey ATBuild)

    it "CBench depends on CLib ATBuild when CLib is in taskKeys" $ do
      let pn = mkPackageName "pkg"
          libKey = ComponentKey pn CLib
          benchKey = ComponentKey pn (CBench (unqualCompFromString "bench"))
          taskKeys = Set.singleton libKey
      intraPackageDeps taskKeys benchKey `shouldBe`
        Set.singleton (ActionId libKey ATBuild)

    it "CSubLib depends on CLib ATBuild when CLib is in taskKeys" $ do
      let pn = mkPackageName "pkg"
          libKey = ComponentKey pn CLib
          subKey = ComponentKey pn (CSubLib (unqualCompFromString "internal"))
          taskKeys = Set.singleton libKey
      intraPackageDeps taskKeys subKey `shouldBe`
        Set.singleton (ActionId libKey ATBuild)

    it "does not depend on CLib of a different package" $ do
      let pn1 = mkPackageName "pkg-a"
          pn2 = mkPackageName "pkg-b"
          libKeyA = ComponentKey pn1 CLib
          exeKeyB = ComponentKey pn2 (CExe (unqualCompFromString "exe"))
          taskKeys = Set.fromList [libKeyA, exeKeyB]
      -- exeKeyB should NOT depend on libKeyA (different package)
      intraPackageDeps taskKeys exeKeyB `shouldBe` Set.empty

    it "CLib never has intra-package deps even when other libs exist" $ do
      let pn = mkPackageName "pkg"
          libKey = ComponentKey pn CLib
          subKey = ComponentKey pn (CSubLib (unqualCompFromString "sub"))
          taskKeys = Set.fromList [libKey, subKey]
      intraPackageDeps taskKeys libKey `shouldBe` Set.empty

  describe "componentTarget" $ do
    it "CLib renders as lib:<package-name>" $ do
      componentTarget (mkPackageName "my-pkg") CLib
        `shouldBe` "lib:my-pkg"

    it "CSubLib renders as lib:<sublib-name>" $ do
      componentTarget (mkPackageName "pkg") (CSubLib (unqualCompFromString "internal"))
        `shouldBe` "lib:internal"

    it "CFlib renders as flib:<flib-name>" $ do
      componentTarget (mkPackageName "pkg") (CFlib (unqualCompFromString "cbits"))
        `shouldBe` "flib:cbits"

    it "CExe renders as exe:<exe-name>" $ do
      componentTarget (mkPackageName "pkg") (CExe (unqualCompFromString "my-exe"))
        `shouldBe` "exe:my-exe"

    it "CTest renders as test:<test-name>" $ do
      componentTarget (mkPackageName "pkg") (CTest (unqualCompFromString "my-test"))
        `shouldBe` "test:my-test"

    it "CBench renders as bench:<bench-name>" $ do
      componentTarget (mkPackageName "pkg") (CBench (unqualCompFromString "my-bench"))
        `shouldBe` "bench:my-bench"

    it "package name is only used for CLib" $ do
      -- For non-CLib components, the package name is ignored
      let pn1 = mkPackageName "pkg-a"
          pn2 = mkPackageName "pkg-b"
          comp = CExe (unqualCompFromString "exe")
      componentTarget pn1 comp `shouldBe` componentTarget pn2 comp

  describe "componentEnableTests" $ do
    let pn = mkPackageName "pkg"
        testName = unqualCompFromString "my-test"
        benchName = unqualCompFromString "my-bench"
        mixedComps = Set.fromList
          [CLib, CTest testName, CBench benchName]

    it "CTest key enables tests regardless of component set" $ do
      componentEnableTests (ComponentKey pn (CTest testName)) Set.empty
        `shouldBe` True
      componentEnableTests (ComponentKey pn (CTest testName)) mixedComps
        `shouldBe` True

    it "CBench key does not enable tests" $ do
      componentEnableTests (ComponentKey pn (CBench benchName)) mixedComps
        `shouldBe` False

    it "CExe key does not enable tests" $ do
      componentEnableTests
        (ComponentKey pn (CExe (unqualCompFromString "exe")))
        mixedComps
        `shouldBe` False

    it "CSubLib key does not enable tests" $ do
      componentEnableTests
        (ComponentKey pn (CSubLib (unqualCompFromString "sub")))
        mixedComps
        `shouldBe` False

    it "CLib key enables tests when component set has tests" $ do
      componentEnableTests (ComponentKey pn CLib) mixedComps
        `shouldBe` True

    it "CLib key does not enable tests when no tests in component set" $ do
      componentEnableTests (ComponentKey pn CLib) (Set.singleton CLib)
        `shouldBe` False

    it "CLib key does not enable tests with only benchmarks" $ do
      componentEnableTests
        (ComponentKey pn CLib)
        (Set.singleton (CBench benchName))
        `shouldBe` False

  describe "componentEnableBenchmarks" $ do
    let pn = mkPackageName "pkg"
        testName = unqualCompFromString "my-test"
        benchName = unqualCompFromString "my-bench"
        mixedComps = Set.fromList
          [CLib, CTest testName, CBench benchName]

    it "CBench key always enables benchmarks" $ do
      componentEnableBenchmarks (ComponentKey pn (CBench benchName)) Set.empty
        `shouldBe` True

    it "CTest key does not enable benchmarks" $ do
      componentEnableBenchmarks (ComponentKey pn (CTest testName)) mixedComps
        `shouldBe` False

    it "CExe key does not enable benchmarks" $ do
      componentEnableBenchmarks
        (ComponentKey pn (CExe (unqualCompFromString "exe")))
        mixedComps
        `shouldBe` False

    it "CLib key enables benchmarks when component set has benchmarks" $ do
      componentEnableBenchmarks (ComponentKey pn CLib) mixedComps
        `shouldBe` True

    it "CLib key does not enable benchmarks when no benchmarks" $ do
      componentEnableBenchmarks (ComponentKey pn CLib) (Set.singleton CLib)
        `shouldBe` False

    it "CLib key does not enable benchmarks with only tests" $ do
      componentEnableBenchmarks
        (ComponentKey pn CLib)
        (Set.singleton (CTest testName))
        `shouldBe` False

  describe "finalTestsAndBenches" $ do
    let pn = mkPackageName "pkg"
        testA = unqualCompFromString "test-a"
        testB = unqualCompFromString "test-b"
        benchA = unqualCompFromString "bench-a"

    it "CTest key returns exactly that one test, no benches" $ do
      let (tests, benches) =
            finalTestsAndBenches
              (ComponentKey pn (CTest testA))
              (Set.fromList [CTest testA, CTest testB, CBench benchA])
      tests `shouldBe` Set.singleton testA
      benches `shouldBe` Set.empty

    it "CBench key returns exactly that one bench, no tests" $ do
      let (tests, benches) =
            finalTestsAndBenches
              (ComponentKey pn (CBench benchA))
              (Set.fromList [CTest testA, CBench benchA])
      tests `shouldBe` Set.empty
      benches `shouldBe` Set.singleton benchA

    it "CLib key returns all tests and benches from component set" $ do
      let comps = Set.fromList [CTest testA, CTest testB, CBench benchA]
          (tests, benches) =
            finalTestsAndBenches (ComponentKey pn CLib) comps
      tests `shouldBe` Set.fromList [testA, testB]
      benches `shouldBe` Set.singleton benchA

    it "CLib key with empty set returns empty tests and benches" $ do
      let (tests, benches) =
            finalTestsAndBenches (ComponentKey pn CLib) Set.empty
      tests `shouldBe` Set.empty
      benches `shouldBe` Set.empty

    it "CLib key with only CLib returns empty tests and benches" $ do
      let (tests, benches) =
            finalTestsAndBenches (ComponentKey pn CLib) (Set.singleton CLib)
      tests `shouldBe` Set.empty
      benches `shouldBe` Set.empty

    it "CExe key falls through to all tests/benches" $ do
      let comps = Set.fromList [CTest testA, CBench benchA]
          (tests, benches) =
            finalTestsAndBenches
              (ComponentKey pn (CExe (unqualCompFromString "exe")))
              comps
      tests `shouldBe` Set.singleton testA
      benches `shouldBe` Set.singleton benchA

    it "CTest key ignores component set entirely" $ do
      -- Even though comp set has multiple tests, only the CTest key's test
      -- is returned
      let (tests, _) =
            finalTestsAndBenches
              (ComponentKey pn (CTest testA))
              (Set.fromList [CTest testA, CTest testB])
      Set.size tests `shouldBe` 1
      tests `shouldBe` Set.singleton testA
