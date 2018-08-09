{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Pantry.TypesSpec (spec) where

import Test.Hspec
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Pantry
import Pantry.StaticSHA256
import Pantry.Types (parseTree, renderTree, Tree (..), TreeEntry (..), mkSafeFilePath)
import RIO
import Distribution.Types.Version (mkVersion)
import qualified RIO.Text as T

hh :: HasCallStack => String -> Property -> Spec
hh name p = it name $ do
  result <- check p
  unless result $ throwString "Hedgehog property failed" :: IO ()

genBlobKey :: Gen BlobKey
genBlobKey = BlobKey <$> genSha256 <*> (FileSize <$> (Gen.word (Range.linear 1 10000)))

genSha256 :: Gen StaticSHA256
genSha256 = mkStaticSHA256FromBytes <$> Gen.bytes (Range.linear 1 500)

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
  describe "Tree" $ do
    hh "parse/render works" $ property $ do
      tree <- forAll $
        let sfp = do
              pieces <- Gen.list (Range.linear 1 10) sfpComponent
              let combined = T.intercalate "/" pieces
              case mkSafeFilePath combined of
                Nothing -> error $ "Incorrect SafeFilePath in test suite: " ++ show pieces
                Just sfp -> pure sfp
            sfpComponent = Gen.text (Range.linear 1 100) Gen.alphaNum
            entry = TreeEntry
              <$> genBlobKey
              <*> Gen.choice (map pure [minBound..maxBound])
         in TreeMap <$> Gen.map (Range.linear 1 100) ((,) <$> sfp <*> entry)
      let bs = renderTree tree
      liftIO $ parseTree bs `shouldBe` Just tree
