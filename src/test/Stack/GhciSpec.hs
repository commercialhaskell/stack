{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Test suite for GHCi like applications including both GHCi and Intero.
module Stack.GhciSpec where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Distribution.ModuleName
import           Stack.Types.Package
import           Stack.Types.PackageName
import           Stack.Types.Version
import           Test.Hspec
import           NeatInterpolation
import           Path
import           Path.Extra (pathToText)

import           Stack.Ghci
import           Stack.Ghci.Script (scriptToLazyByteString)

textToLazy :: Text -> LBS.ByteString
textToLazy = LBS.fromStrict . T.encodeUtf8

projDirA, projDirB :: Path Abs Dir
projDirA = $(mkAbsDir "/Users/someone/src/project-a")
projDirB = $(mkAbsDir "/Users/someone/src/project-b")

relFile :: Path Rel File
relFile = $(mkRelFile "exe/Main.hs")

absFile :: Path Abs File
absFile = projDirA </> relFile

projDirAT, projDirBT, relFileT, absFileT :: Text
projDirAT = pathToText projDirA
projDirBT = pathToText projDirB
relFileT = pathToText relFile
absFileT = pathToText absFile

spec :: Spec
spec = do
  describe "GHCi" $ do
    describe "Script rendering" $ do
      describe "should render GHCi scripts" $ do
        it "with one library package" $ do
          let res = scriptToLazyByteString $ renderScriptGhci packages_singlePackage Nothing
          res `shouldBe` textToLazy ghciScript_projectWithLib

        it "with one main package" $ do
          let res = scriptToLazyByteString $ renderScriptGhci []
                                                              (Just absFile)
          res `shouldBe` textToLazy ghciScript_projectWithMain

        it "with one library and main package" $ do
          let res = scriptToLazyByteString $ renderScriptGhci packages_singlePackage
                                                              (Just absFile)
          res `shouldBe` textToLazy ghciScript_projectWithLibAndMain

        it "with multiple library packages" $ do
          let res = scriptToLazyByteString $ renderScriptGhci packages_multiplePackages Nothing
          res `shouldBe` textToLazy ghciScript_multipleProjectsWithLib

      describe "should render intero scripts" $ do
        it "with one library package" $ do
          let res = scriptToLazyByteString $ renderScriptIntero packages_singlePackage Nothing
          res `shouldBe` textToLazy interoScript_projectWithLib

        it "with one main package" $ do
          let res = scriptToLazyByteString $ renderScriptIntero packages_singlePackage
                                                              (Just absFile)
          res `shouldBe` textToLazy interoScript_projectWithMain

        it "with one library and main package" $ do
          let res = scriptToLazyByteString $ renderScriptIntero packages_singlePackage
                                                              (Just absFile)
          res `shouldBe` textToLazy interoScript_projectWithLibAndMain

        it "with multiple library packages" $ do
          let res = scriptToLazyByteString $ renderScriptIntero packages_multiplePackages Nothing
          res `shouldBe` textToLazy interoScript_multipleProjectsWithLib

-- Exptected Intero scripts

interoScript_projectWithLib :: Text
interoScript_projectWithLib = [text|
:cd-ghc $projDirAT
:add Lib.A
:module + Lib.A

|]

interoScript_projectWithMain :: Text
interoScript_projectWithMain = [text|
:cd-ghc $projDirAT
:add Lib.A
:cd-ghc $projDirAT
:add $absFileT
:module + Lib.A

|]

interoScript_projectWithLibAndMain :: Text
interoScript_projectWithLibAndMain = [text|
:cd-ghc $projDirAT
:add Lib.A
:cd-ghc $projDirAT
:add $absFileT
:module + Lib.A

|]

interoScript_multipleProjectsWithLib :: Text
interoScript_multipleProjectsWithLib = [text|
:cd-ghc $projDirAT
:add Lib.A
:cd-ghc $projDirBT
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
:add $absFileT
:module +

|]

ghciScript_projectWithLibAndMain :: Text
ghciScript_projectWithLibAndMain = [text|
:add Lib.A
:add $absFileT
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
:add $absFileT
:module +
|]

ghciLegacyScript_projectWithLibAndMain :: Text
ghciLegacyScript_projectWithLibAndMain = [text|
:add Lib.A
:add $absFileT
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
    , ghciPkgDir = projDirA
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
    , ghciPkgDir = projDirA
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
    , ghciPkgDir = projDirB
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
