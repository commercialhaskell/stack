module Pantry.HPackSpec (spec) where

import Test.Hspec
import RIO
import Pantry
import Pantry.HPack
import Path

isVersion :: Version -> Bool
isVersion _ = True

customHpack :: (HasLogFunc env, HasPantryConfig env) => FilePath -> RIO env a -> RIO env a
customHpack fp f = do
  env <- ask
  let a = set (pantryConfigL . hpackExecutableL) (HpackCommand fp) env
  local (const a) f

spec :: Spec
spec = describe "Parse HPack version" $ do
  it "Shipped hpack version" $ do
    version <- runPantryApp $ hpackVersion
    version `shouldSatisfy` isVersion

  -- it "External hpack version" $ do
  --   version <- runPantryApp $ customHpack "/home/sibi/.local/bin/hpack" hpackVersion
  --   version `shouldSatisfy` isVersion
