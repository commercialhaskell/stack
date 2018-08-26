{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Pantry.ArchiveSpec (spec) where

import Test.Hspec
import RIO
import Pantry
import Path.IO (resolveFile')

spec :: Spec
spec = do
  it "cabal file from tarball" $ asIO $ runPantryApp $ do
    let rel = "attic/package-0.1.2.3.tar.gz"
    abs' <- resolveFile' rel
    ident <- getPackageLocationIdent $ PLIArchive
      Archive
        { archiveLocation = ALFilePath ResolvedPath
            { resolvedRelative = RelFilePath $ fromString rel
            , resolvedAbsolute = abs'
            }
        , archiveHash = Nothing
        , archiveSize = Nothing
        , archiveSubdir = ""
        }
      PackageMetadata
        { pmName = Nothing
        , pmVersion = Nothing
        , pmTreeKey = Nothing
        , pmCabal = Nothing
        }
    case parsePackageIdentifier "package-0.1.2.3" of
      Nothing -> error "should have parsed"
      Just expected -> liftIO $ ident `shouldBe` expected
  it "handles symlinks to parent dirs" $ do
    ident <- runPantryApp $ getPackageLocationIdent $ PLIArchive
      Archive
        { archiveLocation = ALUrl "https://github.com/commercialhaskell/stack/archive/2b846ff4fda13a8cd095e7421ce76df0a08b10dc.tar.gz"
        , archiveHash = Nothing
        , archiveSize = Nothing
        , archiveSubdir = "subs/pantry/"
        }
      PackageMetadata
        { pmName = Nothing
        , pmVersion = Nothing
        , pmTreeKey = Nothing
        , pmCabal = Nothing
        }
    case parsePackageIdentifier "pantry-0.1.0.0" of
      Nothing -> error "should have parsed"
      Just expected -> ident `shouldBe` expected
