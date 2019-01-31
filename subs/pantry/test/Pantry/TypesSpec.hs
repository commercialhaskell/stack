{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes#-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Pantry.TypesSpec (spec) where

import Test.Hspec
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified RIO.HashMap as HM
import Pantry
import qualified Pantry.SHA256 as SHA256
import Pantry.Internal (parseTree, renderTree, Tree (..), TreeEntry (..), mkSafeFilePath)
import RIO
import Distribution.Types.Version (mkVersion)
import qualified RIO.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson.Extended
import Distribution.Types.PackageName (mkPackageName)
import qualified Data.ByteString.Char8 as S8
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List
import Data.List.NonEmpty hiding (map)
import Data.String.Quote
import qualified Data.Vector as Vector
import Data.Semigroup

hh :: HasCallStack => String -> Property -> Spec
hh name p = it name $ do
  result <- check p
  unless result $ throwString "Hedgehog property failed" :: IO ()

decodeSHA :: ByteString -> SHA256
decodeSHA string = case SHA256.fromHexBytes string of
                     Right csha -> csha
                     Left err -> error $ "Failed decoding. Error:  " <> show err

toBlobKey :: ByteString -> Word -> BlobKey
toBlobKey string size = BlobKey (decodeSHA string) (FileSize size)

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
        Just x -> pure x
        Nothing -> fail "Can't parse Repo"
      let repoValue = Repo {
                    repoSubdir = "wai",
                    repoType = RepoGit,
                    repoCommit = "d11d63f1a6a92db8c637a8d33e7953ce6194a3e0",
                    repoUrl = "https://github.com/yesodweb/wai.git"
                  }
      repValue `shouldBe` repoValue

    it "FromJSON instance for PackageMetadata" $ do
      pkgMeta <- case Yaml.decodeThrow samplePLIRepo of
        Just x -> pure x
        Nothing -> fail "Can't parse Repo"
      let cabalSha = SHA256.fromHexBytes "eea52c4967d8609c2f79213d6dffe6d6601034f1471776208404781de7051410"
          pantrySha = SHA256.fromHexBytes "ecfd0b4b75f435a3f362394807b35e5ef0647b1a25005d44a3632c49db4833d2"
      (csha, psha) <- case (cabalSha, pantrySha) of
                        (Right csha , Right psha) -> pure (csha, psha)
                        _ -> fail "Failed decoding sha256"
      let pkgValue = PackageMetadata {
                                    pmIdent = PackageIdentifier (mkPackageName "wai") (mkVersion [3,2,1,2]),
                                    pmTreeKey = TreeKey (BlobKey psha (FileSize 714)),
                                    pmCabal = BlobKey csha (FileSize 1765)
                                  }
      pkgMeta `shouldBe` pkgValue

    it "parseHackageText parses" $ do
      let txt = "persistent-2.8.2@sha256:df118e99f0c46715e932fe82d787fc09689d87898f3a8b13f5954d25af6b46a1,5058"
          hsha = SHA256.fromHexBytes "df118e99f0c46715e932fe82d787fc09689d87898f3a8b13f5954d25af6b46a1"
      sha <- case hsha of
        Right sha' -> pure sha'
        _ -> fail "parseHackagetext: failed decoding the sha256"
      let Right (pkgIdentifier, blobKey) = parseHackageText txt
      blobKey `shouldBe` (BlobKey sha (FileSize 5058))
      pkgIdentifier `shouldBe` PackageIdentifier (mkPackageName "persistent") (mkVersion [2,8,2])

    it "parses PackageLocationImmutable (Repo)" $ do
       let lockFile :: ByteString
           lockFile = [s|#some
dependencies:
- complete:
  - subdir: wai
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
resolver:
- original:
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/22.yaml
- complete:
    size: 527801
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/22.yaml
sha256: 7c8b1853da784bd7beb8728168bf4e879d8a2f6daf408ca0fa7933451864a96a
|]

       pkgImm <- case Yaml.decodeThrow lockFile of
         Just (pkgIm :: Value) -> do
           case Yaml.parseEither parseLockFile pkgIm of
             Left str -> fail $ "Can't parse PackageLocationImmutable - 1" <> str
             Right xs -> do
               xs' <- sequence xs
               pure $ NonEmpty.toList xs'
         Nothing -> fail "Can't parse PackageLocationImmutable - 2"
       pkgImm `shouldBe`
           [ PLIRepo
                 (Repo
                      { repoUrl = "https://github.com/yesodweb/wai.git"
                      , repoCommit = "d11d63f1a6a92db8c637a8d33e7953ce6194a3e0"
                      , repoSubdir = "wai"
                      , repoType = RepoGit
                      })
                 (PackageMetadata
                      { pmIdent =
                            PackageIdentifier
                                { pkgName = mkPackageName "wai"
                                , pkgVersion = mkVersion [3, 2, 1, 2]
                                }
                      , pmTreeKey =
                            TreeKey
                                (BlobKey
                                     (decodeSHA
                                          "ecfd0b4b75f435a3f362394807b35e5ef0647b1a25005d44a3632c49db4833d2")
                                     (FileSize 714))
                      , pmCabal =
                            toBlobKey
                                "eea52c4967d8609c2f79213d6dffe6d6601034f1471776208404781de7051410"
                                1765
                      })
           ]

    it "parses PackageLocationImmutable (Multiple Repos)" $ do
       let lockFile :: ByteString
           lockFile = [s|#some
dependencies:
- complete:
  - subdir: wai
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
  - subdir: warp
    cabal-file:
      size: 10725
      sha256: cfec5336260bb4b1ecbd833f7d6948fd1ee373770471fe79796cf9c389c71758
    name: warp
    version: 3.2.25
    git: https://github.com/yesodweb/wai.git
    pantry-tree:
      size: 5103
      sha256: f808e075811b002563d24c393ce115be826bb66a317d38da22c513ee42b7443a
    commit: d11d63f1a6a92db8c637a8d33e7953ce6194a3e0
resolver:
- original:
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/22.yaml
- complete:
    size: 527801
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/22.yaml
sha256: 7c8b1853da784bd7beb8728168bf4e879d8a2f6daf408ca0fa7933451864a96a
|]
       pkgImm <- case Yaml.decodeThrow lockFile of
         Just (pkgIm :: Value) -> do
           case Yaml.parseEither parseLockFile pkgIm of
             Left str -> fail $ "Can't parse PackageLocationImmutable - 1" <> str
             Right xs -> do
               xs' <- sequence xs
               pure $ NonEmpty.toList xs'
         Nothing -> fail "Can't parse PackageLocationImmutable - 2"
       pkgImm `shouldBe`
           [ PLIRepo
                 (Repo
                      { repoUrl = "https://github.com/yesodweb/wai.git"
                      , repoCommit = "d11d63f1a6a92db8c637a8d33e7953ce6194a3e0"
                      , repoSubdir = "wai"
                      , repoType = RepoGit
                      })
                 (PackageMetadata
                      { pmIdent =
                            PackageIdentifier
                                { pkgName = mkPackageName "wai"
                                , pkgVersion = mkVersion [3, 2, 1, 2]
                                }
                      , pmTreeKey =
                            TreeKey
                                (BlobKey
                                     (decodeSHA
                                          "ecfd0b4b75f435a3f362394807b35e5ef0647b1a25005d44a3632c49db4833d2")
                                     (FileSize 714))
                      , pmCabal =
                            toBlobKey
                                "eea52c4967d8609c2f79213d6dffe6d6601034f1471776208404781de7051410"
                                1765
                      }),
             PLIRepo
                 (Repo
                      { repoUrl = "https://github.com/yesodweb/wai.git"
                      , repoCommit = "d11d63f1a6a92db8c637a8d33e7953ce6194a3e0"
                      , repoSubdir = "warp"
                      , repoType = RepoGit
                      })
                 (PackageMetadata
                      { pmIdent =
                            PackageIdentifier
                                { pkgName = mkPackageName "warp"
                                , pkgVersion = mkVersion [3, 2, 25]
                                }
                      , pmTreeKey =
                            TreeKey
                                (BlobKey
                                     (decodeSHA
                                      "f808e075811b002563d24c393ce115be826bb66a317d38da22c513ee42b7443a")
                                     (FileSize 5103))
                      , pmCabal =
                            toBlobKey
                                "cfec5336260bb4b1ecbd833f7d6948fd1ee373770471fe79796cf9c389c71758"
                                10725
                      })
           ]
    it "parses PackageLocationImmutable (RPLIArchive)" $ do
        let lockFile :: ByteString
            lockFile = [s|#some
dependencies:
- complete:
  - size: 285152
    subdir: wai
    url: http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip
    cabal-file:
      size: 1717
      sha256: 7b46e7a8b121d668351fa8a684810afadf58c39276125098485203ef274fd056
    name: wai
    version: 3.0.2.3
    sha256: 3b6eb04f3763ca16432f3ab2135d239161fbe2c8811b8cd1778ffa67469289ba
    pantry-tree:
      size: 710
      sha256: 754e9b9d6949e23fa5ca730f50453d7e91fd2bc2d9170537fa2d33db8d6138fc
resolver:
- original:
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/22.yaml
- complete:
    size: 527801
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/22.yaml
sha256: 7c8b1853da784bd7beb8728168bf4e879d8a2f6daf408ca0fa7933451864a96a
|]
        pkgImm <-
            case Yaml.decodeThrow lockFile of
                Just (pkgIm :: Value) -> do
                    case Yaml.parseEither parseLockFile pkgIm of
                        Left str ->
                            fail $ "Can't parse PackageLocationImmutable - 1" <> str
                        Right xs -> do
                            xs' <- sequence xs
                            pure $ NonEmpty.toList xs'
                Nothing -> fail "Can't parse PackageLocationImmutable"
        pkgImm `shouldBe`
            [ PLIArchive
                  (Archive
                       { archiveLocation =
                             ALUrl
                                 "http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip"
                       , archiveHash =
                             decodeSHA
                                 "3b6eb04f3763ca16432f3ab2135d239161fbe2c8811b8cd1778ffa67469289ba"
                       , archiveSize = FileSize 285152
                       , archiveSubdir = "wai"
                       })
                  (PackageMetadata
                       { pmIdent =
                             PackageIdentifier
                                 { pkgName = mkPackageName "wai"
                                 , pkgVersion = mkVersion [3, 0, 2, 3]
                                 }
                       , pmTreeKey =
                             TreeKey
                                 (BlobKey
                                      (decodeSHA
                                           "754e9b9d6949e23fa5ca730f50453d7e91fd2bc2d9170537fa2d33db8d6138fc")
                                      (FileSize 710))
                       , pmCabal =
                             toBlobKey
                                 "7b46e7a8b121d668351fa8a684810afadf58c39276125098485203ef274fd056"
                                 1717
                       })
            ]
    it "parses PackageLocationImmutable (multiple RPLIArchive)" $ do
        let lockFile :: ByteString
            lockFile = [s|#some
dependencies:
- complete:
  - size: 285152
    subdir: wai
    url: http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip
    cabal-file:
      size: 1717
      sha256: 7b46e7a8b121d668351fa8a684810afadf58c39276125098485203ef274fd056
    name: wai
    version: 3.0.2.3
    sha256: 3b6eb04f3763ca16432f3ab2135d239161fbe2c8811b8cd1778ffa67469289ba
    pantry-tree:
      size: 710
      sha256: 754e9b9d6949e23fa5ca730f50453d7e91fd2bc2d9170537fa2d33db8d6138fc
  - size: 285152
    subdir: wai
    url: http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip
    cabal-file:
      size: 1717
      sha256: 7b46e7a8b121d668351fa8a684810afadf58c39276125098485203ef274fd056
    name: wai
    version: 3.0.2.3
    sha256: 3b6eb04f3763ca16432f3ab2135d239161fbe2c8811b8cd1778ffa67469289ba
    pantry-tree:
      size: 710
      sha256: 754e9b9d6949e23fa5ca730f50453d7e91fd2bc2d9170537fa2d33db8d6138fc
resolver:
- original:
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/22.yaml
- complete:
    size: 527801
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/22.yaml
sha256: 7c8b1853da784bd7beb8728168bf4e879d8a2f6daf408ca0fa7933451864a96a
|]
        pkgImm <-
            case Yaml.decodeThrow lockFile of
                Just (pkgIm :: Value) -> do
                    case Yaml.parseEither parseLockFile pkgIm of
                        Left str ->
                            fail $ "Can't parse PackageLocationImmutable - 1" <> str
                        Right xs -> do
                            xs' <- sequence xs
                            pure $ NonEmpty.toList xs'
                Nothing -> fail "Can't parse PackageLocationImmutable"
        pkgImm `shouldBe`
            [ PLIArchive
                  (Archive
                       { archiveLocation =
                             ALUrl
                                 "http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip"
                       , archiveHash =
                             decodeSHA
                                 "3b6eb04f3763ca16432f3ab2135d239161fbe2c8811b8cd1778ffa67469289ba"
                       , archiveSize = FileSize 285152
                       , archiveSubdir = "wai"
                       })
                  (PackageMetadata
                       { pmIdent =
                             PackageIdentifier
                                 { pkgName = mkPackageName "wai"
                                 , pkgVersion = mkVersion [3, 0, 2, 3]
                                 }
                       , pmTreeKey =
                             TreeKey
                                 (BlobKey
                                      (decodeSHA
                                           "754e9b9d6949e23fa5ca730f50453d7e91fd2bc2d9170537fa2d33db8d6138fc")
                                      (FileSize 710))
                       , pmCabal =
                             toBlobKey
                                 "7b46e7a8b121d668351fa8a684810afadf58c39276125098485203ef274fd056"
                                 1717
                       }),
              PLIArchive
                  (Archive
                       { archiveLocation =
                             ALUrl
                                 "http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip"
                       , archiveHash =
                             decodeSHA
                                 "3b6eb04f3763ca16432f3ab2135d239161fbe2c8811b8cd1778ffa67469289ba"
                       , archiveSize = FileSize 285152
                       , archiveSubdir = "wai"
                       })
                  (PackageMetadata
                       { pmIdent =
                             PackageIdentifier
                                 { pkgName = mkPackageName "wai"
                                 , pkgVersion = mkVersion [3, 0, 2, 3]
                                 }
                       , pmTreeKey =
                             TreeKey
                                 (BlobKey
                                      (decodeSHA
                                           "754e9b9d6949e23fa5ca730f50453d7e91fd2bc2d9170537fa2d33db8d6138fc")
                                      (FileSize 710))
                       , pmCabal =
                             toBlobKey
                                 "7b46e7a8b121d668351fa8a684810afadf58c39276125098485203ef274fd056"
                                 1717
                       })
            ]
    it "parses PackageLocationImmutable (PLIHackage)" $ do
        let lockFile :: ByteString
            lockFile = [s|#some
dependencies:
- complete:
  - hackage: persistent-2.8.2@sha256:df118e99f0c46715e932fe82d787fc09689d87898f3a8b13f5954d25af6b46a1,5058
    pantry-tree:
      size: 2165
      sha256: 3cb3a9ca3e373a152d9acf622706471578cc93b2ed20c893fc20a4814264f1ce
resolver:
- original:
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/22.yaml
- complete:
    size: 527801
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/22.yaml
sha256: 7c8b1853da784bd7beb8728168bf4e879d8a2f6daf408ca0fa7933451864a96a
|]
        pkgImm <-
            case Yaml.decodeThrow lockFile of
                Just (pkgIm :: Value) -> do
                    case Yaml.parseEither parseLockFile pkgIm of
                        Left str ->
                            fail $ "Can't parse PackageLocationImmutable - 1" <> str
                        Right xs -> do
                            xs' <- sequence xs
                            pure $ NonEmpty.toList xs'
                Nothing -> fail "Can't parse PackageLocationImmutable"
        pkgImm `shouldBe` [PLIHackage (PackageIdentifier {pkgName = mkPackageName "persistent", pkgVersion = mkVersion [2,8,2]}) (toBlobKey
                                 "df118e99f0c46715e932fe82d787fc09689d87898f3a8b13f5954d25af6b46a1" 5058) (TreeKey
                                 (BlobKey
                                      (decodeSHA
                                           "3cb3a9ca3e373a152d9acf622706471578cc93b2ed20c893fc20a4814264f1ce")
                                      (FileSize 2165)))]
    it "parses PackageLocationImmutable (PLIHackage & PLIArchive)" $ do
        let lockFile :: ByteString
            lockFile = [s|#some
dependencies:
- complete:
  - hackage: persistent-2.8.2@sha256:df118e99f0c46715e932fe82d787fc09689d87898f3a8b13f5954d25af6b46a1,5058
    pantry-tree:
      size: 2165
      sha256: 3cb3a9ca3e373a152d9acf622706471578cc93b2ed20c893fc20a4814264f1ce
  - size: 285152
    subdir: wai
    url: http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip
    cabal-file:
      size: 1717
      sha256: 7b46e7a8b121d668351fa8a684810afadf58c39276125098485203ef274fd056
    name: wai
    version: 3.0.2.3
    sha256: 3b6eb04f3763ca16432f3ab2135d239161fbe2c8811b8cd1778ffa67469289ba
    pantry-tree:
      size: 710
      sha256: 754e9b9d6949e23fa5ca730f50453d7e91fd2bc2d9170537fa2d33db8d6138fc
resolver:
- original:
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/22.yaml
- complete:
    size: 527801
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/22.yaml
sha256: 7c8b1853da784bd7beb8728168bf4e879d8a2f6daf408ca0fa7933451864a96a
|]
        pkgImm <-
            case Yaml.decodeThrow lockFile of
                Just (pkgIm :: Value) -> do
                    case Yaml.parseEither parseLockFile pkgIm of
                        Left str ->
                            fail $ "Can't parse PackageLocationImmutable - 1" <> str
                        Right xs -> do
                            xs' <- sequence xs
                            pure $ NonEmpty.toList xs'
                Nothing -> fail "Can't parse PackageLocationImmutable"
        pkgImm `shouldBe` [PLIHackage (PackageIdentifier {pkgName = mkPackageName "persistent", pkgVersion = mkVersion [2,8,2]}) (toBlobKey
                                 "df118e99f0c46715e932fe82d787fc09689d87898f3a8b13f5954d25af6b46a1" 5058) (TreeKey
                                 (BlobKey
                                      (decodeSHA
                                           "3cb3a9ca3e373a152d9acf622706471578cc93b2ed20c893fc20a4814264f1ce")
                                      (FileSize 2165))),
                  PLIArchive
                  (Archive
                       { archiveLocation =
                             ALUrl
                                 "http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip"
                       , archiveHash =
                             decodeSHA
                                 "3b6eb04f3763ca16432f3ab2135d239161fbe2c8811b8cd1778ffa67469289ba"
                       , archiveSize = FileSize 285152
                       , archiveSubdir = "wai"
                       })
                  (PackageMetadata
                       { pmIdent =
                             PackageIdentifier
                                 { pkgName = mkPackageName "wai"
                                 , pkgVersion = mkVersion [3, 0, 2, 3]
                                 }
                       , pmTreeKey =
                             TreeKey
                                 (BlobKey
                                      (decodeSHA
                                           "754e9b9d6949e23fa5ca730f50453d7e91fd2bc2d9170537fa2d33db8d6138fc")
                                      (FileSize 710))
                       , pmCabal =
                             toBlobKey
                                 "7b46e7a8b121d668351fa8a684810afadf58c39276125098485203ef274fd056"
                                 1717
                       })]
