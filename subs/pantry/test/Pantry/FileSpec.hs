module Pantry.FileSpec (spec) where

import Test.Hspec
import Pantry
import Path
import Path.IO
import Control.Monad (void)

spec :: Spec
spec = describe "loadCabalFilePath" $ do
  it "sanity" $ do
    abs' <- resolveDir' "."
    (f, name, cabalfp) <- runPantryApp $ loadCabalFilePath abs'
    suffix <- parseRelFile "pantry-tmp.cabal"
    cabalfp `shouldBe` abs' </> suffix
    name' <- parsePackageNameThrowing "pantry-tmp"
    name `shouldBe` name'
    void $ f NoPrintWarnings
