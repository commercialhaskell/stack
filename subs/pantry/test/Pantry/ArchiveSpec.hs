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
        }
      PackageMetadata
        { pmName = Nothing
        , pmVersion = Nothing
        , pmTree = Nothing
        , pmCabal = Nothing
        , pmSubdir = ""
        }
    case parsePackageIdentifier "package-0.1.2.3" of
      Nothing -> error "should have parsed"
      Just expected -> liftIO $ ident `shouldBe` expected
