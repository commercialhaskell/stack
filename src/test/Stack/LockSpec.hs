{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.LockSpec where

import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Yaml as Yaml
import Data.Yaml (Value)
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Types.Version (mkVersion)
import Pantry
import qualified Pantry.SHA256 as SHA256
import qualified Path
import Stack.Lock
import Test.Hspec
import Text.RawString.QQ

toBlobKey :: ByteString -> Word -> BlobKey
toBlobKey string size = BlobKey (decodeSHA string) (FileSize size)

decodeSHA :: ByteString -> SHA256
decodeSHA string =
    case SHA256.fromHexBytes string of
        Right csha -> csha
        Left err -> error $ "Failed decoding. Error:  " <> show err

spec :: Spec
spec = do
    it "parses lock file (empty)" $ do
        let lockFile :: ByteString
            lockFile =
                [r|#some
dependencies: []
resolver:
  original:
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/12/20.yaml
  complete:
    size: 508369
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/12/20.yaml
    sha256: 7373bd6e5bb08955cb30bc98afe38a06eadc44706d20aff896fd0376ec0de619
|]
        rootDir <- Path.parseAbsDir "/home/sibi"
        pkgImm <-
            case Yaml.decodeThrow lockFile of
                Just (pkgIm :: Value) -> do
                    case Yaml.parseEither (parsePackageLockFile rootDir) pkgIm of
                        Left str ->
                            fail $
                            "Can't parse PackageLocationImmutable - 1" <> str <>
                            show pkgIm
                        Right iopl -> lfPackageLocations <$> iopl
                Nothing -> fail "Can't parse PackageLocationImmutable"
        Map.toList pkgImm `shouldBe` []
    it "parses lock file (non empty)" $ do
        let lockFile :: ByteString
            lockFile =
                [r|#some
dependencies:
- original:
    subdir: wai
    git: https://github.com/yesodweb/wai.git
    commit: d11d63f1a6a92db8c637a8d33e7953ce6194a3e0
  complete:
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
- original:
    subdir: warp
    git: https://github.com/yesodweb/wai.git
    commit: d11d63f1a6a92db8c637a8d33e7953ce6194a3e0
  complete:
    subdir: warp
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
  original:
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/22.yaml
  complete:
    size: 527801
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/22.yaml
    sha256: 7c8b1853da784bd7beb8728168bf4e879d8a2f6daf408ca0fa7933451864a96a
|]
        rootDir <- Path.parseAbsDir "/home/sibi"
        pkgImm <-
            case Yaml.decodeThrow lockFile of
                Just (pkgIm :: Value) -> do
                    case Yaml.parseEither (parsePackageLockFile rootDir) pkgIm of
                        Left str ->
                            fail $
                            "Can't parse PackageLocationImmutable - 1" <> str <>
                            show pkgIm
                        Right iopl -> lfPackageLocations <$> iopl
                Nothing -> fail "Can't parse PackageLocationImmutable"
        let pkgImm' = map (\(a, b) -> (b, a)) (Map.toList pkgImm)
        pkgImm' `shouldBe`
            [ ( PLImmutable
                    (PLIRepo
                         (Repo
                              { repoType = RepoGit
                              , repoUrl = "https://github.com/yesodweb/wai.git"
                              , repoCommit =
                                    "d11d63f1a6a92db8c637a8d33e7953ce6194a3e0"
                              , repoSubdir = "wai"
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
                              }))
              , RPLImmutable
                    (RPLIRepo
                         (Repo
                              { repoType = RepoGit
                              , repoUrl = "https://github.com/yesodweb/wai.git"
                              , repoCommit =
                                    "d11d63f1a6a92db8c637a8d33e7953ce6194a3e0"
                              , repoSubdir = "wai"
                              })
                         (RawPackageMetadata
                              { rpmName = Nothing
                              , rpmVersion = Nothing
                              , rpmTreeKey = Nothing
                              , rpmCabal = Nothing
                              })))
            , ( PLImmutable
                    (PLIRepo
                         (Repo
                              { repoType = RepoGit
                              , repoUrl = "https://github.com/yesodweb/wai.git"
                              , repoCommit =
                                    "d11d63f1a6a92db8c637a8d33e7953ce6194a3e0"
                              , repoSubdir = "warp"
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
                              }))
              , RPLImmutable
                    (RPLIRepo
                         (Repo
                              { repoType = RepoGit
                              , repoUrl = "https://github.com/yesodweb/wai.git"
                              , repoCommit =
                                    "d11d63f1a6a92db8c637a8d33e7953ce6194a3e0"
                              , repoSubdir = "warp"
                              })
                         (RawPackageMetadata
                              { rpmName = Nothing
                              , rpmVersion = Nothing
                              , rpmTreeKey = Nothing
                              , rpmCabal = Nothing
                              })))
            ]
    it "parses snapshot lock file (non empty)" $ do
        let lockFile :: ByteString
            lockFile =
                [r|#some
dependencies:
- original:
    hackage: string-quote-0.0.1
  complete:
    hackage: string-quote-0.0.1@sha256:7d91a0ba1be44b2443497c92f2f027cd4580453b893f8b5ebf061e1d85befaf3,758
    pantry-tree:
      size: 273
      sha256: d291028785ad39f8d05cde91594f6b313e35ff76af66c0452ab599b1f1f59e5f
|]
        rootDir <- Path.parseAbsDir "/home/sibi"
        pkgImm <-
            case Yaml.decodeThrow lockFile of
                Just (pkgIm :: Value) -> do
                    case Yaml.parseEither
                             (resolveSnapshotLayerLockFile rootDir)
                             pkgIm of
                        Left str ->
                            fail $
                            "Can't parse PackageLocationImmutable - 1" <> str <>
                            show pkgIm
                        Right iopl -> iopl
                Nothing -> fail "Can't parse PackageLocationImmutable"
        let pkgImm' = map (\(a, b) -> (b, a)) (Map.toList pkgImm)
        pkgImm' `shouldBe`
            [ ( PLIHackage
                    (PackageIdentifier
                         { pkgName = mkPackageName "string-quote"
                         , pkgVersion = mkVersion [0, 0, 1]
                         })
                    (toBlobKey
                         "7d91a0ba1be44b2443497c92f2f027cd4580453b893f8b5ebf061e1d85befaf3"
                         758)
                    (TreeKey
                         (BlobKey
                              (decodeSHA
                                   "d291028785ad39f8d05cde91594f6b313e35ff76af66c0452ab599b1f1f59e5f")
                              (FileSize 273)))
              , RPLIHackage
                    (PackageIdentifierRevision
                         (mkPackageName "string-quote")
                         (mkVersion [0, 0, 1])
                         CFILatest)
                    Nothing)
            ]
