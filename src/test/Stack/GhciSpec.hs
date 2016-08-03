{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Test suite for GHCi like applications including both GHCi and Intero.
module Stack.GhciSpec where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
      describe "should render legacy GHCi scripts" $ do
        it "should render legacy script when given project:exe" $ do
          renderLegacyGhciScript [] (Just $(mkAbsFile "/Users/someone/src/project-a/exe/Main.hs"))
            `shouldBe` T.unpack ghciLegacyScript_projectWithMain

        it "should render legacy script when given project" $ do
          renderLegacyGhciScript ["Lib.A"] (Just $(mkAbsFile "/Users/someone/src/project-a/exe/Main.hs"))
            `shouldBe` T.unpack ghciLegacyScript_projectWithLibAndMain

        it "should render legacy script when given multiple project:lib" $ do
          renderLegacyGhciScript ["Lib.A", "Lib.B"] Nothing
            `shouldBe` T.unpack ghciLegacyScript_multipleProjectsWithLib

      describe "should render GHCi scripts" $ do
        it "with one library package" $ do
          let res = scriptToLazyByteString $ renderScriptGhci packages_singlePackage Nothing
          res `shouldBe` (LBS.fromStrict $ T.encodeUtf8 ghciScript_projectWithLib)

        it "with one main package" $ do
          let res = scriptToLazyByteString $ renderScriptGhci []
                                                              (Just $(mkAbsFile "/Users/someone/src/project-a/exe/Main.hs"))
          res `shouldBe` (LBS.fromStrict $ T.encodeUtf8 ghciScript_projectWithMain)

        it "with one library and main package" $ do
          let res = scriptToLazyByteString $ renderScriptGhci packages_singlePackage
                                                              (Just $(mkAbsFile "/Users/someone/src/project-a/exe/Main.hs"))
          res `shouldBe` (LBS.fromStrict $ T.encodeUtf8 ghciScript_projectWithLibAndMain)

        it "with multiple library packages" $ do
          let res = scriptToLazyByteString $ renderScriptGhci packages_multiplePackages Nothing
          res `shouldBe` (LBS.fromStrict $ T.encodeUtf8 ghciScript_multipleProjectsWithLib)

      describe "should render intero scripts" $ do
        it "with one library package" $ do
          let res = scriptToLazyByteString $ renderScriptIntero packages_singlePackage Nothing
          res `shouldBe` (LBS.fromStrict $ T.encodeUtf8 interoScript_projectWithLib)

        it "with one main package" $ do
          let res = scriptToLazyByteString $ renderScriptIntero packages_singlePackage
                                                              (Just $(mkAbsFile "/Users/someone/src/project-a/exe/Main.hs"))
          res `shouldBe` (LBS.fromStrict $ T.encodeUtf8 interoScript_projectWithMain)

        it "with one library and main package" $ do
          let res = scriptToLazyByteString $ renderScriptIntero packages_singlePackage
                                                              (Just $(mkAbsFile "/Users/someone/src/project-a/exe/Main.hs"))
          res `shouldBe` (LBS.fromStrict $ T.encodeUtf8 interoScript_projectWithLibAndMain)

        it "with multiple library packages" $ do
          let res = scriptToLazyByteString $ renderScriptIntero packages_multiplePackages Nothing
          res `shouldBe` (LBS.fromStrict $ T.encodeUtf8 interoScript_multipleProjectsWithLib)

-- Exptected Intero scripts

interoScript_projectWithLib :: Text
interoScript_projectWithLib = [text|
:cd-ghc /Users/someone/src/project-a/
:add Lib.A
:module + Lib.A

|]

interoScript_projectWithMain :: Text
interoScript_projectWithMain = [text|
:cd-ghc /Users/someone/src/project-a/
:add Lib.A
:cd-ghc /Users/someone/src/project-a/
:add /Users/someone/src/project-a/exe/Main.hs
:module + Lib.A

|]

interoScript_projectWithLibAndMain :: Text
interoScript_projectWithLibAndMain = [text|
:cd-ghc /Users/someone/src/project-a/
:add Lib.A
:cd-ghc /Users/someone/src/project-a/
:add /Users/someone/src/project-a/exe/Main.hs
:module + Lib.A

|]

interoScript_multipleProjectsWithLib :: Text
interoScript_multipleProjectsWithLib = [text|
:cd-ghc /Users/someone/src/project-a/
:add Lib.A
:cd-ghc /Users/someone/src/project-b/
:add Lib.B
:module + Lib.A Lib.B

|]

-- Expected GHCi Scripts

ghciScript_projectWithLib :: Text
ghciScript_projectWithLib = [text|
:add Lib.A
:module + Lib.A

|]

ghciScript_projectWithMain :: Text
ghciScript_projectWithMain = [text|
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
:add Lib.A
:add Lib.B
:module + Lib.A Lib.B

|]

-- Expected Legacy GHCi scripts

ghciLegacyScript_projectWithMain :: Text
ghciLegacyScript_projectWithMain = [text|
:add
:add /Users/someone/src/project-a/exe/Main.hs
:module +
|]

ghciLegacyScript_projectWithLibAndMain :: Text
ghciLegacyScript_projectWithLibAndMain = [text|
:add Lib.A
:add /Users/someone/src/project-a/exe/Main.hs
:module + Lib.A
|]

ghciLegacyScript_multipleProjectsWithLib :: Text
ghciLegacyScript_multipleProjectsWithLib = [text|
:add Lib.A Lib.B

:module + Lib.A Lib.B
|]

-- Sample GHCi load configs

packages_singlePackage :: [GhciPkgInfo]
packages_singlePackage =
  [ GhciPkgInfo
    { ghciPkgModules = S.fromList [fromString "Lib.A"]
    , ghciPkgDir = $(mkAbsDir "/Users/someone/src/project-a")
    , ghciPkgName = $(mkPackageName "package-a")
    , ghciPkgOpts = []
    , ghciPkgModFiles = S.empty
    , ghciPkgCFiles = S.empty
    , ghciPkgMainIs = M.empty
    , ghciPkgPackage =
      Package
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

packages_multiplePackages :: [GhciPkgInfo]
packages_multiplePackages =
  [ GhciPkgInfo
    { ghciPkgModules = S.fromList [fromString "Lib.A"]
    , ghciPkgDir = $(mkAbsDir "/Users/someone/src/project-a")
    , ghciPkgName = $(mkPackageName "package-a")
    , ghciPkgOpts = []
    , ghciPkgModFiles = S.empty
    , ghciPkgCFiles = S.empty
    , ghciPkgMainIs = M.empty
    , ghciPkgPackage =
      Package
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
  , GhciPkgInfo
    { ghciPkgModules = S.fromList [fromString "Lib.B"]
    , ghciPkgDir = $(mkAbsDir "/Users/someone/src/project-b")
    , ghciPkgName = $(mkPackageName "package-b")
    , ghciPkgOpts = []
    , ghciPkgModFiles = S.empty
    , ghciPkgCFiles = S.empty
    , ghciPkgMainIs = M.empty
    , ghciPkgPackage =
      Package
      { packageName = $(mkPackageName "package-b")
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
