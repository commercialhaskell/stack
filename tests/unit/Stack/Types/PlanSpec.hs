{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.PlanSpec
  ( main
  , spec
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Distribution.Types.PackageName ( mkPackageName )
import           Stack.Prelude
import           Stack.Types.ComponentUtils ( unqualCompFromString )
import           Stack.Types.NamedComponent ( NamedComponent (..) )
import           Stack.Types.Plan
                   ( ComponentKey (..), componentKeyPkgName )
import           Test.Hspec ( Spec, describe, hspec, it, shouldBe )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ComponentKey" $ do
    it "componentKeyPkgName extracts the package name" $ do
      let pn = mkPackageName "my-pkg"
          ck = ComponentKey pn CLib
      componentKeyPkgName ck `shouldBe` pn

    it "componentKeyPkgName works for non-CLib components" $ do
      let pn = mkPackageName "my-pkg"
          cn = unqualCompFromString "test-suite"
          ck = ComponentKey pn (CTest cn)
      componentKeyPkgName ck `shouldBe` pn

    it "two ComponentKeys with different components are not equal" $ do
      let pn = mkPackageName "my-pkg"
          ckLib = ComponentKey pn CLib
          ckExe = ComponentKey pn (CExe (unqualCompFromString "my-exe"))
      (ckLib == ckExe) `shouldBe` False

    it "two ComponentKeys with different packages are not equal" $ do
      let ck1 = ComponentKey (mkPackageName "pkg-a") CLib
          ck2 = ComponentKey (mkPackageName "pkg-b") CLib
      (ck1 == ck2) `shouldBe` False

    it "two identical ComponentKeys are equal" $ do
      let ck1 = ComponentKey (mkPackageName "pkg") CLib
          ck2 = ComponentKey (mkPackageName "pkg") CLib
      (ck1 == ck2) `shouldBe` True

    it "ComponentKey has a well-defined Ord instance" $ do
      let pn = mkPackageName "pkg"
          ckLib = ComponentKey pn CLib
          ckExe = ComponentKey pn (CExe (unqualCompFromString "exe"))
          ckTest = ComponentKey pn (CTest (unqualCompFromString "test"))
      -- CLib, CSubLib, CFlib, CExe, CTest, CBench ordering comes from
      -- NamedComponent's derived Ord
      (ckLib < ckExe) `shouldBe` True
      (ckExe < ckTest) `shouldBe` True

    it "can be used as Map keys" $ do
      let ck1 = ComponentKey (mkPackageName "pkg-a") CLib
          ck2 = ComponentKey (mkPackageName "pkg-a")
                  (CTest (unqualCompFromString "tests"))
          ck3 = ComponentKey (mkPackageName "pkg-b") CLib
          m :: Map ComponentKey String
          m = Map.fromList [(ck1, "lib"), (ck2, "test"), (ck3, "lib-b")]
      Map.size m `shouldBe` 3
      Map.lookup ck1 m `shouldBe` Just "lib"
      Map.lookup ck2 m `shouldBe` Just "test"

    it "can be used in Sets" $ do
      let ck1 = ComponentKey (mkPackageName "pkg") CLib
          ck2 = ComponentKey (mkPackageName "pkg")
                  (CExe (unqualCompFromString "exe"))
          s = Set.fromList [ck1, ck2, ck1] -- duplicate ck1
      Set.size s `shouldBe` 2
