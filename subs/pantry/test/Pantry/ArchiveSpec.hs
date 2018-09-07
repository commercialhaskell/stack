{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Pantry.ArchiveSpec (spec) where

import Test.Hspec
import Data.Maybe (fromJust)
import RIO
import RIO.Text as T
import Pantry
import Path.IO (resolveFile')

data TestLocation
  = TLFilePath String
  | TLUrl Text

data TestArchive = TestArchive
  { testLocation :: !TestLocation
  , testSubdir :: !Text
  }

getPackageLocationIdent' :: TestArchive -> IO PackageIdentifier
getPackageLocationIdent' TestArchive{..} = do
  testLocation' <- case testLocation of
    TLFilePath relPath -> do
      absPath <- resolveFile' relPath
      return $ ALFilePath $ ResolvedPath
        { resolvedRelative = RelFilePath $ fromString relPath
        , resolvedAbsolute = absPath
        }
    TLUrl url -> return $ ALUrl url
  let archive = Archive
        { archiveLocation = testLocation'
        , archiveHash = Nothing
        , archiveSize = Nothing
        , archiveSubdir = testSubdir
        }
  runPantryApp $ getPackageLocationIdent $ PLIArchive archive metadata
  where
    metadata = PackageMetadata
      { pmName = Nothing
      , pmVersion = Nothing
      , pmTreeKey = Nothing
      , pmCabal = Nothing
      }

parsePackageIdentifier' :: String -> PackageIdentifier
parsePackageIdentifier' = fromJust . parsePackageIdentifier

urlToStackCommit :: Text -> TestLocation
urlToStackCommit commit = TLUrl $ T.concat
  [ "https://github.com/commercialhaskell/stack/archive/"
  , commit
  , ".tar.gz"
  ]

treeWithoutCabalFile :: Selector PantryException
treeWithoutCabalFile (TreeWithoutCabalFile _) = True
treeWithoutCabalFile _ = False

spec :: Spec
spec = do
  it "finds cabal file from tarball" $ do
    ident <- getPackageLocationIdent' TestArchive
      { testLocation = TLFilePath "attic/package-0.1.2.3.tar.gz"
      , testSubdir = ""
      }
    ident `shouldBe` parsePackageIdentifier' "package-0.1.2.3"
  it "finds cabal file from tarball with subdir '.'" $ do
    ident <- getPackageLocationIdent' TestArchive
      { testLocation = TLFilePath "attic/package-0.1.2.3.tar.gz"
      , testSubdir = "."
      }
    ident `shouldBe` parsePackageIdentifier' "package-0.1.2.3"
  it "finds cabal file from tarball with subdir 'subs/pantry/'" $ do
    ident <- getPackageLocationIdent' TestArchive
      { testLocation = urlToStackCommit "2b846ff4fda13a8cd095e7421ce76df0a08b10dc"
      , testSubdir = "subs/pantry/"
      }
    ident `shouldBe` parsePackageIdentifier' "pantry-0.1.0.0"
  it "matches whole directory name" $
    getPackageLocationIdent' TestArchive
      { testLocation = urlToStackCommit "2b846ff4fda13a8cd095e7421ce76df0a08b10dc"
      , testSubdir = "subs/pant"
      }
    `shouldThrow` treeWithoutCabalFile
