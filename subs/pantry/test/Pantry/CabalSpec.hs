{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Pantry.CabalSpec (spec) where

import Test.Hspec
import Pantry
import qualified Pantry.SHA256 as SHA256
import RIO
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Types.Version (mkVersion)

spec :: Spec
spec = describe "wrong cabal file" $ do
  let test name action = it name (runPantryApp action :: IO ())
      shouldThrow' x y = withRunInIO $ \run -> run x `shouldThrow` y
  test "Hackage" $ do
    sha <- either throwIO pure
         $ SHA256.fromHexBytes "71c2c685a932cd3a70ec52d7bd0ec96ecbfa5e31e22130099cd50fa073ad1a69"
    let rpli =
          RPLIHackage
            (PackageIdentifierRevision
              name
              version3
              (CFIHash sha (Just size)))
            Nothing
        go = loadCabalFileRawImmutable rpli
        name = mkPackageName "acme-missiles"
        version2 = mkVersion [0, 2]
        version3 = mkVersion [0, 3]
        size = FileSize 597
    go `shouldThrow'` \e ->
      case e of
        MismatchedPackageMetadata rpli' rpm _tree cabal ident ->
          rpli == rpli' &&
          rpm == RawPackageMetadata
            { rpmName = Just name
            , rpmVersion = Just version3
            , rpmTreeKey = Nothing
            , rpmCabal = Just $ BlobKey sha size
            } &&
          cabal == BlobKey sha size &&
          ident == PackageIdentifier name version2
        _ -> False

  test "tarball with wrong ident" $ do
    archiveHash' <- either throwIO pure
                  $ SHA256.fromHexBytes "b5a582209c50e4a61e4b6c0fb91a6a7d65177a881225438b0144719bc3682c3a"
    sha <- either throwIO pure
         $ SHA256.fromHexBytes "71c2c685a932cd3a70ec52d7bd0ec96ecbfa5e31e22130099cd50fa073ad1a69"
    let rpli = RPLIArchive archive rpm
        archive =
            RawArchive
              { raLocation = ALUrl "https://github.com/yesodweb/yesod/archive/yesod-auth-1.6.4.1.tar.gz"
              , raHash = Just archiveHash'
              , raSize = Just $ FileSize 309199
              , raSubdir = "yesod-auth"
              }
        rpm =
            RawPackageMetadata
              { rpmName = Just acmeMissiles
              , rpmVersion = Just version2
              , rpmCabal = Just $ BlobKey sha (FileSize 597)
              , rpmTreeKey = Nothing
              }
        go = loadCabalFileRawImmutable rpli
        acmeMissiles = mkPackageName "acme-missiles"
        version2 = mkVersion [0, 2]
    go `shouldThrow'` \e ->
      case e of
        MismatchedPackageMetadata rpli' rpm' _treeKey cabal ident ->
          rpli == rpli' &&
          rpm == rpm' &&
          cabal == BlobKey
            (either impureThrow id $ SHA256.fromHexBytes "940d82426ad1db0fcc978c0f386ac5d06df019546071993cb7c6633f1ad17d50")
            (FileSize 3038) &&
          ident == PackageIdentifier
            (mkPackageName "yesod-auth")
            (mkVersion [1, 6, 4, 1])
        _ -> False

  test "tarball with wrong cabal file" $ do
    sha <- either throwIO pure
         $ SHA256.fromHexBytes "71c2c685a932cd3a70ec52d7bd0ec96ecbfa5e31e22130099cd50fa073ad1a69"
    let rpli = RPLIArchive archive rpm
        archive =
            RawArchive
              { raLocation = ALUrl "https://github.com/yesodweb/yesod/archive/yesod-auth-1.6.4.1.tar.gz"
              , raHash = either impureThrow Just
                         $ SHA256.fromHexBytes "b5a582209c50e4a61e4b6c0fb91a6a7d65177a881225438b0144719bc3682c3a"
              , raSize = Just $ FileSize 309199
              , raSubdir = "yesod-auth"
              }
        rpm =
            RawPackageMetadata
              { rpmName = Just yesodAuth
              , rpmVersion = Just version
              , rpmCabal = Just $ BlobKey sha (FileSize 597)
              , rpmTreeKey = Nothing
              }
        go = loadCabalFileRawImmutable rpli
        yesodAuth = mkPackageName "yesod-auth"
        version = mkVersion [1, 6, 4, 1]
    go `shouldThrow'` \e ->
      case e of
        MismatchedPackageMetadata rpli' rpm' _treeKey cabal ident ->
          rpli == rpli' &&
          rpm == rpm' &&
          cabal == BlobKey
            (either impureThrow id $ SHA256.fromHexBytes "940d82426ad1db0fcc978c0f386ac5d06df019546071993cb7c6633f1ad17d50")
            (FileSize 3038) &&
          ident == PackageIdentifier yesodAuth version
        _ -> False
