{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes#-}
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
import Data.Aeson.Extended (WithJSONWarnings (..))
import qualified Data.ByteString.Char8 as S8
import Data.String.Quote

hh :: HasCallStack => String -> Property -> Spec
hh name p = it name $ do
  result <- check p
  unless result $ throwString "Hedgehog property failed" :: IO ()

genBlobKey :: Gen BlobKey
genBlobKey = BlobKey <$> genSha256 <*> (FileSize <$> (Gen.word (Range.linear 1 10000)))

genSha256 :: Gen SHA256
genSha256 = SHA256.hashBytes <$> Gen.bytes (Range.linear 1 500)

samplePLIRepo :: ByteString
samplePLIRepo = [s|
subdir: wai
cabal-file:
  size: 1765
  sha256: eea52c4967d8609c2f79213d6dffe6d6601034f1471776208404781de7051410
name: wai
version: 3.2.1.2
git: https://github.com/yesodweb/wai.git
pantry-tree:
  size: 714
  sha256: ecfd0b4b75f435a3f362394807b35e5ef0647b1a25005d44a3632c49db4833d2
commit: d11d63f1a6a92db8c637a8d33e7953ce6194a3e0
|]

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

  describe "(Raw)SnapshotLayer" $ do
    let parseSl :: String -> IO RawSnapshotLayer
        parseSl str = case Yaml.decodeThrow . S8.pack $ str of
          (Just (WithJSONWarnings x _)) -> resolvePaths Nothing x
          Nothing -> fail "Can't parse RawSnapshotLayer"

    it "parses snapshot using 'resolver'" $ do
      RawSnapshotLayer{..} <- parseSl $
        "name: 'test'\n" ++
        "resolver: lts-2.10\n"
      rslParent `shouldBe` ltsSnapshotLocation 2 10

    it "parses snapshot using 'snapshot'" $ do
      RawSnapshotLayer{..} <- parseSl $
        "name: 'test'\n" ++
        "snapshot: lts-2.10\n"
      rslParent `shouldBe` ltsSnapshotLocation 2 10

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
      RawSnapshotLayer{..} <- parseSl $
        "name: 'test'\n" ++
        "compiler: ghc-8.0.1\n"
      rslParent `shouldBe` RSLCompiler (WCGhc (mkVersion [8, 0, 1]))

    it "FromJSON instance for Repo" $ do
      repValue <- case Yaml.decodeThrow samplePLIRepo of
        Just (WithJSONWarnings x _) -> pure x
        Nothing -> fail "Can't parse Repo"
      let repoValue = Repo {
                    repoSubdir = "wai",
                    repoType = RepoGit,
                    repoCommit = "d11d63f1a6a92db8c637a8d33e7953ce6194a3e0",
                    repoUrl = "https://github.com/yesodweb/wai.git"
                  }

      repValue `shouldBe` repoValue
