{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Pantry.HackageSpec (spec) where

import Test.Hspec
import Pantry
import RIO
import Distribution.Types.Version (mkVersion)

spec :: Spec
spec = do
  it "update works" $ asIO $ void $ runPantryApp $ updateHackageIndex Nothing
  it "fuzzy lookup kicks in" $ do
    let pir = PackageIdentifierRevision "thisisnot-tobe-foundon-hackage-please" (mkVersion [1..3]) CFILatest
    runPantryApp (loadPackage (PLIHackage pir Nothing))
      `shouldThrow` \e ->
        case e of
          UnknownHackagePackage pir' _  -> pir == pir'
          _ -> False
  -- Flaky test, can be broken by new packages on Hackage.
  it "finds acme-missiles" $ do
    x <- runPantryApp (getHackageTypoCorrections "acme-missile")
    x `shouldSatisfy` ("acme-missiles" `elem`)
