{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Pantry.TypesSpec (spec) where

import Test.Hspec
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Pantry
import RIO
import Distribution.Types.Version (mkVersion)

hh :: HasCallStack => String -> Property -> Spec
hh name p = it name $ do
  result <- check p
  unless result $ throwString "Hedgehog property failed" :: IO ()

spec :: Spec
spec = do
  describe "WantedCompiler" $ do
    hh "parse/render works" $ property $ do
      wc <- forAll $
        let ghc = WCGhc <$> genVersion
            ghcjs = WCGhcjs <$> genVersion <*> genVersion
            genVersion = mkVersion <$> Gen.list (Range.linear 1 5) (Gen.int (Range.linear 0 100))
         in Gen.choice [ghc, ghcjs]
      let text = utf8BuilderToText $ display wc
      case parseWantedCompiler text of
        Left e -> throwIO e
        Right actual -> liftIO $ actual `shouldBe` wc
