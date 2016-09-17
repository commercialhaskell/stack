{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Test suite for "Stack.Solver"
module Stack.SolverSpec where

import           Data.Text (unpack)
import           Stack.Types.FlagName
import           Stack.Types.PackageName
import           Stack.Types.Version
import           Test.Hspec
import qualified Data.Map as Map

import Stack.Solver

spec :: Spec
spec =
  describe "Stack.Solver" $ do
    successfulExample
      "text-1.2.1.1 (latest: 1.2.2.0) -integer-simple (via: parsec-3.1.9) (new package)"
      $(mkPackageName "text")
      $(mkVersion "1.2.1.1")
      [ ($(mkFlagName "integer-simple"), False)
      ]
    successfulExample
      "hspec-snap-1.0.0.0 *test (via: servant-snap-0.5) (new package)"
      $(mkPackageName "hspec-snap")
      $(mkVersion "1.0.0.0")
      []
    successfulExample
      "time-locale-compat-0.1.1.1 -old-locale (via: http-api-data-0.2.2) (new package)"
      $(mkPackageName "time-locale-compat")
      $(mkVersion "0.1.1.1")
      [ ($(mkFlagName "old-locale"), False)
      ]
    successfulExample
      "flowdock-rest-0.2.0.0 -aeson-compat *test (via: haxl-fxtra-0.0.0.0) (new package)"
      $(mkPackageName "flowdock-rest")
      $(mkVersion "0.2.0.0")
      [ ($(mkFlagName "aeson-compat"), False)
      ]
  where
    successfulExample input pkgName pkgVersion flags =
      it ("parses " ++ unpack input) $
        parseCabalOutputLine input `shouldBe` Right (pkgName, (pkgVersion, Map.fromList flags))
