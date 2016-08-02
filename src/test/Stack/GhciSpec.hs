{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Test suite for GHCi like applications including both GHCi and Intero.
module Stack.GhciSpec where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Distribution.ModuleName
import           Stack.Types.Package
import           Stack.Types.PackageName
import           Stack.Types.Version
import           Test.Hspec
import           NeatInterpolation
import           Path

import           Stack.Ghci
import           Stack.Ghci.Script (scriptToLazyByteString)

spec :: Spec
spec = do
  describe "GHCi" $ do
    describe "Script rendering" $ do
      it "should render legacy script when given project:exe" $ do
        renderLegacyGhciScript [] (Just $(mkAbsFile "/Users/someone/src/project-a/exe/Main.hs"))
          `shouldBe` T.unpack ghciScript_projectWithMain

      it "should render legacy script when given project" $ do
        renderLegacyGhciScript ["Lib.A"] (Just $(mkAbsFile "/Users/someone/src/project-a/exe/Main.hs"))
          `shouldBe` T.unpack ghciScript_projectWithLibAndMain

      it "should render legacy script when given multiple project:lib" $ do
        renderLegacyGhciScript ["Lib.A", "Lib.B"] Nothing
          `shouldBe` T.unpack ghciScript_multipleProjectsWithLib

      it "should render GHCi scripts" $ do
        let pkgs =
              [ GhciPkgInfo
                { ghciPkgModules = S.fromList [fromString "Lib.A"]
                , ghciPkgDir = $(mkAbsDir "/src/package-a")
                , ghciPkgName = $(mkPackageName "package-a")
                , ghciPkgOpts = []
                , ghciPkgModFiles = S.empty
                , ghciPkgCFiles = S.empty
                , ghciPkgMainIs = M.empty
                , ghciPkgPackage = Package
                  { packageName = $(mkPackageName "package-a")
                  , packageVersion = $(mkVersion "0.1.0.0")
                  , packageFiles = GetPackageFiles undefined
                  , packageDeps = M.empty
                  , packageTools = []
                  , packageAllDeps = S.empty
                  , packageGhcOptions = []
                  , packageFlags = M.empty
                  , packageDefaultFlags = M.empty
                  , packageHasLibrary = True
                  , packageTests = M.empty
                  , packageBenchmarks = S.empty
                  , packageExes = S.empty
                  , packageOpts = GetPackageOpts undefined
                  , packageHasExposedModules = True
                  , packageSimpleType = True
                  }
                }
              ]
            res = scriptToLazyByteString $ renderScriptGhci pkgs
        res `shouldBe` ":add Lib.A\n:module + Lib.A\n"


      it "should render intero scripts" $ do
        let pkgs =
              [ GhciPkgInfo
                { ghciPkgModules = S.fromList [fromString "Lib.A"]
                , ghciPkgDir = $(mkAbsDir "/src/package-a")
                , ghciPkgName = $(mkPackageName "package-a")
                , ghciPkgOpts = []
                , ghciPkgModFiles = S.empty
                , ghciPkgCFiles = S.empty
                , ghciPkgMainIs = M.empty
                , ghciPkgPackage = Package
                  { packageName = $(mkPackageName "package-a")
                  , packageVersion = $(mkVersion "0.1.0.0")
                  , packageFiles = GetPackageFiles undefined
                  , packageDeps = M.empty
                  , packageTools = []
                  , packageAllDeps = S.empty
                  , packageGhcOptions = []
                  , packageFlags = M.empty
                  , packageDefaultFlags = M.empty
                  , packageHasLibrary = True
                  , packageTests = M.empty
                  , packageBenchmarks = S.empty
                  , packageExes = S.empty
                  , packageOpts = GetPackageOpts undefined
                  , packageHasExposedModules = True
                  , packageSimpleType = True
                  }
                }
              ]
            res = scriptToLazyByteString $ renderScriptIntero pkgs
        res `shouldBe` ":cd-ghc /src/package-a/\n:add Lib.A\n:module + Lib.A\n"

-- Exptected GHCi scripts

ghciScript_projectWithMain :: Text
ghciScript_projectWithMain = [text|
:add
:add /Users/someone/src/project-a/exe/Main.hs
:module +
|]

ghciScript_projectWithLibAndMain :: Text
ghciScript_projectWithLibAndMain = [text|
:add Lib.A
:add /Users/someone/src/project-a/exe/Main.hs
:module + Lib.A
|]

ghciScript_multipleProjectsWithLib :: Text
ghciScript_multipleProjectsWithLib = [text|
:add Lib.A Lib.B

:module + Lib.A Lib.B
|]
