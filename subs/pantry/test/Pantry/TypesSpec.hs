{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Pantry.TypesSpec
    ( spec
    ) where

import Data.Aeson.Extended
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml as Yaml
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Types.Version (mkVersion)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Pantry
import Pantry.Internal
    ( Tree(..)
    , TreeEntry(..)
    , mkSafeFilePath
    , parseTree
    , renderTree
    )
import qualified Pantry.SHA256 as SHA256
import RIO
import qualified RIO.Text as T
import Test.Hspec
import Text.RawString.QQ
import RIO.Time (Day (..))

hh :: HasCallStack => String -> Property -> Spec
hh name p = it name $ do
  result <- check p
  unless result $ throwString "Hedgehog property failed" :: IO ()

genBlobKey :: Gen BlobKey
genBlobKey = BlobKey <$> genSha256 <*> (FileSize <$> (Gen.word (Range.linear 1 10000)))

genSha256 :: Gen SHA256
genSha256 = SHA256.hashBytes <$> Gen.bytes (Range.linear 1 500)

samplePLIRepo :: ByteString
samplePLIRepo =
    [r|
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

samplePLIRepo2 :: ByteString
samplePLIRepo2 =
    [r|
cabal-file:
  size: 1863
  sha256: 5ebffc39e75ea1016adcc8426dc31d2040d2cc8a5f4bbce228592ef35e233da2
name: merkle-log
version: 0.1.0.0
git: https://github.com/kadena-io/merkle-log.git
pantry-tree:
  size: 615
  sha256: 5a99e5e41ccd675a7721a733714ba2096f4204d9010f867c5fb7095b78e2959d
commit: a7ae61d7082afe3aa1a0fd0546fc1351a2f7c376
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

    hh "rendering an LTS gives a nice name" $ property $ do
      (major, minor) <- forAll $ (,)
        <$> Gen.integral (Range.linear 1 10000)
        <*> Gen.integral (Range.linear 1 10000)
      liftIO $
        Yaml.toJSON (ltsSnapshotLocation major minor) `shouldBe`
        Yaml.String (T.pack $ concat ["lts-", show major, ".", show minor])

    hh "rendering a nightly gives a nice name" $ property $ do
      days <- forAll $ Gen.integral $ Range.linear 1 10000000
      let day = ModifiedJulianDay days
      liftIO $
        Yaml.toJSON (nightlySnapshotLocation day) `shouldBe`
        Yaml.String (T.pack $ "nightly-" ++ show day)
    it "FromJSON instance for PLIRepo" $ do
      WithJSONWarnings unresolvedPli warnings <- Yaml.decodeThrow samplePLIRepo
      warnings `shouldBe` []
      pli <- resolvePaths Nothing unresolvedPli
      let repoValue =
              Repo
                  { repoSubdir = "wai"
                  , repoType = RepoGit
                  , repoCommit =
                        "d11d63f1a6a92db8c637a8d33e7953ce6194a3e0"
                  , repoUrl = "https://github.com/yesodweb/wai.git"
                  }
          cabalSha =
              SHA256.fromHexBytes
                  "eea52c4967d8609c2f79213d6dffe6d6601034f1471776208404781de7051410"
          pantrySha =
              SHA256.fromHexBytes
                  "ecfd0b4b75f435a3f362394807b35e5ef0647b1a25005d44a3632c49db4833d2"
      (csha, psha) <- case (cabalSha, pantrySha) of
        (Right csha, Right psha) -> pure (csha, psha)
        _ -> fail "Failed decoding sha256"
      let pkgValue =
              PackageMetadata
                  { pmIdent =
                        PackageIdentifier
                            (mkPackageName "wai")
                            (mkVersion [3, 2, 1, 2])
                  , pmTreeKey = TreeKey (BlobKey psha (FileSize 714))
                  , pmCabal = BlobKey csha (FileSize 1765)
                  }
      pli `shouldBe` PLIRepo repoValue pkgValue

      WithJSONWarnings reparsed warnings2 <- Yaml.decodeThrow $ Yaml.encode pli
      warnings2 `shouldBe` []
      reparsed' <- resolvePaths Nothing reparsed
      reparsed' `shouldBe` pli
    it "parseHackageText parses" $ do
      let txt =
              "persistent-2.8.2@sha256:df118e99f0c46715e932fe82d787fc09689d87898f3a8b13f5954d25af6b46a1,5058"
          hsha =
              SHA256.fromHexBytes
                  "df118e99f0c46715e932fe82d787fc09689d87898f3a8b13f5954d25af6b46a1"
      sha <- case hsha of
        Right sha' -> pure sha'
        _ -> fail "parseHackagetext: failed decoding the sha256"
      let Right (pkgIdentifier, blobKey) = parseHackageText txt
      blobKey `shouldBe` (BlobKey sha (FileSize 5058))
      pkgIdentifier `shouldBe`
          PackageIdentifier
              (mkPackageName "persistent")
              (mkVersion [2, 8, 2])
    it "roundtripping a PLIRepo" $ do
      WithJSONWarnings unresolvedPli warnings <- Yaml.decodeThrow samplePLIRepo2
      warnings `shouldBe` []
      pli <- resolvePaths Nothing unresolvedPli
      WithJSONWarnings unresolvedPli2 warnings2 <- Yaml.decodeThrow $ Yaml.encode pli
      warnings2 `shouldBe` []
      pli2 <- resolvePaths Nothing unresolvedPli2
      pli2 `shouldBe` (pli :: PackageLocationImmutable)
