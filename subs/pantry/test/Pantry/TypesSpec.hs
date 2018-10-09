{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Pantry.TypesSpec (spec) where

import Test.Hspec
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Pantry
import qualified Pantry.SHA256 as SHA256
import Pantry.Internal (parseTree, renderTree, Tree (..), TreeEntry (..), mkSafeFilePath)
import RIO
import Distribution.Types.Version (mkVersion)
import qualified RIO.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson.Extended (WithJSONWarnings (..), Value)
import qualified Data.ByteString.Char8 as S8

hh :: HasCallStack => String -> Property -> Spec
hh name p = it name $ do
  result <- check p
  unless result $ throwString "Hedgehog property failed" :: IO ()

genBlobKey :: Gen BlobKey
genBlobKey = BlobKey <$> genSha256 <*> (FileSize <$> (Gen.word (Range.linear 1 10000)))

genSha256 :: Gen SHA256
genSha256 = SHA256.hashBytes <$> Gen.bytes (Range.linear 1 500)

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
                Just sfp' -> pure sfp'
            sfpComponent = Gen.text (Range.linear 1 15) Gen.alphaNum
            entry = TreeEntry
              <$> genBlobKey
              <*> Gen.choice (map pure [minBound..maxBound])
         in TreeMap <$> Gen.map (Range.linear 1 20) ((,) <$> sfp <*> entry)
      let bs = renderTree tree
      liftIO $ parseTree bs `shouldBe` Just tree

  describe "SnapshotLayer" $ do
    let parseSl :: String -> IO SnapshotLayer
        parseSl str = case Yaml.decodeThrow . S8.pack $ str of
          (Just (WithJSONWarnings x _)) -> resolvePaths Nothing x
          Nothing -> fail "Can't parse SnapshotLayer"

    it "parses snapshot using 'resolver'" $ do
      SnapshotLayer{..} <- parseSl $
        "name: 'test'\n" ++
        "resolver: lts-2.10\n"
      slParent `shouldBe` ltsSnapshotLocation 2 10

    it "parses snapshot using 'snapshot'" $ do
      SnapshotLayer{..} <- parseSl $
        "name: 'test'\n" ++
        "snapshot: lts-2.10\n"
      slParent `shouldBe` ltsSnapshotLocation 2 10

    it "throws if both 'resolver' and 'snapshot' are present" $ do
      let go = parseSl $
                "name: 'test'\n" ++
                "resolver: lts-2.10\n" ++
                "snapshot: lts-2.10\n"
      go `shouldThrow` anyException

    it "throws if both 'snapshot' and 'compiler' are not present" $ do
      let go = parseSl "name: 'test'\n"
      go `shouldThrow` anyException

    it "works if no 'snapshot' specified" $ do
      SnapshotLayer{..} <- parseSl $
        "name: 'test'\n" ++
        "compiler: ghc-8.0.1\n"
      slParent `shouldBe` SLCompiler (WCGhc (mkVersion [8, 0, 1]))
