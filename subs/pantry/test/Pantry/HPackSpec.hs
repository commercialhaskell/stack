module Pantry.HPackSpec (spec) where

import Test.Hspec
import Pantry
import Pantry.HPack
import Path

isVersion :: Version -> Bool
isVersion _ = True

spec :: Spec
spec = describe "Parse HPack version" $ do
  it "Shipped hpack version" $ do
    version <- runPantryApp $ hpackVersion
    version `shouldSatisfy` isVersion

  -- it "External hpack version" $ do
  --   fp <- parseAbsFile "/home/sibi/.local/bin/hpack"
  --   version <- runPantryAppWithCustomHpack fp hpackVersion
  --   version `shouldSatisfy` isVersion
