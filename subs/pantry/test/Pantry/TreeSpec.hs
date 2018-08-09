{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Pantry.TreeSpec (spec) where

import Test.Hspec
import RIO
import Pantry

spec :: Spec
spec = do
  let tarURL = "https://github.com/snoyberg/file-embed/archive/47b499c3c58ca465c56ee0295d0a76782a66751d.tar.gz"
      zipURL = "https://github.com/snoyberg/file-embed/archive/47b499c3c58ca465c56ee0295d0a76782a66751d.zip"
      pm = PackageMetadata
        { pmName = Nothing
        , pmVersion = Nothing
        , pmTree = Nothing
        , pmCabal = Nothing
        , pmSubdir = ""
        }
      mkArchive url =
        PLIArchive
          Archive
            { archiveLocation = ALUrl url
            , archiveHash = Nothing
            , archiveSize = Nothing
            }
          pm
      tarPL = mkArchive tarURL
      zipPL = mkArchive zipURL
      repoPL =
          PLIRepo
            Repo
              { repoUrl = "https://github.com/snoyberg/file-embed.git"
              , repoCommit = "47b499c3c58ca465c56ee0295d0a76782a66751d"
              , repoType = RepoGit
              }
            pm

  it "zip and tar.gz archives match" $ asIO $ runPantryApp $ do
    pair1 <- loadPackageLocation tarPL
    pair2 <- loadPackageLocation zipPL
    liftIO $ pair2 `shouldBe` pair1
  it "archive and repo match" $ asIO $ runPantryApp $ do
    pair1 <- loadPackageLocation tarPL
    pair2 <- loadPackageLocation repoPL
    liftIO $ pair2 `shouldBe` pair1
