{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.LockSpec where

import Data.Aeson.Extended (WithJSONWarnings(..))
import Data.ByteString (ByteString)
import qualified Data.Yaml as Yaml
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Types.Version (mkVersion)
import Pantry
import qualified Pantry.SHA256 as SHA256
import RIO
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

decodeLocked :: ByteString -> IO Locked
decodeLocked bs = do
  val <- Yaml.decodeThrow  bs
  case Yaml.parseEither Yaml.parseJSON val of
    Left err -> throwIO $ Yaml.AesonException err
    Right (WithJSONWarnings res warnings) -> do
      unless (null warnings) $
        throwIO $ Yaml.AesonException $ "Unexpected warnings: " ++ show warnings
      -- we just assume no file references
      resolvePaths Nothing res

spec :: Spec
spec = do
    it "parses lock file (empty with GHC resolver)" $ do
        let lockFile :: ByteString
            lockFile =
                [r|#some
snapshots:
- completed:
    compiler: ghc-8.2.2
  original:
    compiler: ghc-8.2.2
packages: []
|]
        pkgImm <- lckPkgImmutableLocations <$> decodeLocked lockFile
        pkgImm `shouldBe` []
    it "parses lock file (empty with LTS resolver)" $ do
        let lockFile :: ByteString
            lockFile =
                [r|#some
snapshots:
- completed:
    size: 527801
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/22.yaml
    sha256: 7c8b1853da784bd7beb8728168bf4e879d8a2f6daf408ca0fa7933451864a96a
  original: lts-11.22
- completed:
    compiler: ghc-8.2.2
  original:
    compiler: ghc-8.2.2
packages: []
|]
        pkgImm <- lckPkgImmutableLocations <$> decodeLocked lockFile
        pkgImm `shouldBe` []
    it "parses lock file (LTS, wai + warp)" $ do
        let lockFile :: ByteString
            lockFile =
                [r|#some
snapshots:
- completed:
    size: 527801
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/22.yaml
    sha256: 7c8b1853da784bd7beb8728168bf4e879d8a2f6daf408ca0fa7933451864a96a
  original: lts-11.22
- completed:
    compiler: ghc-8.2.2
  original:
    compiler: ghc-8.2.2
packages:
- original:
    subdir: wai
    git: https://github.com/yesodweb/wai.git
    commit: d11d63f1a6a92db8c637a8d33e7953ce6194a3e0
  completed:
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
  completed:
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
|]
        pkgImm <- lckPkgImmutableLocations <$> decodeLocked lockFile
        let waiSubdirRepo subdir =
              Repo { repoType = RepoGit
                   , repoUrl = "https://github.com/yesodweb/wai.git"
                   , repoCommit =
                       "d11d63f1a6a92db8c637a8d33e7953ce6194a3e0"
                   , repoSubdir = subdir
                   }
            emptyRPM = RawPackageMetadata { rpmName = Nothing
                                          , rpmVersion = Nothing
                                          , rpmTreeKey = Nothing
                                          , rpmCabal = Nothing
                                          }
        pkgImm `shouldBe`
            [ LockedLocation
              (RPLIRepo (waiSubdirRepo "wai") emptyRPM)
              (PLIRepo (waiSubdirRepo "wai")
                    (PackageMetadata { pmIdent =
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
            , LockedLocation
              (RPLIRepo (waiSubdirRepo "warp") emptyRPM)
              (PLIRepo (waiSubdirRepo "warp")
                   (PackageMetadata { pmIdent =
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
            ]
