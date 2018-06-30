{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.TemplateNameSpec where

import Stack.Types.TemplateName
import Path.Internal
import Test.Hspec

spec :: Spec
spec =
  describe "TemplateName" $ do
    describe "parseTemplateNameFromString" $ do
      let pathOf s = either error templatePath (parseTemplateNameFromString s)

      it "parses out the TemplatePath" $ do
        pathOf "github:user/name"     `shouldBe` (RepoPath $ RepoTemplatePath Github    "user" "name.hsfiles")
        pathOf "bitbucket:user/name"  `shouldBe` (RepoPath $ RepoTemplatePath Bitbucket "user" "name.hsfiles")
        pathOf "gitlab:user/name"     `shouldBe` (RepoPath $ RepoTemplatePath Gitlab    "user" "name.hsfiles")

        pathOf "http://www.com/file"  `shouldBe` (UrlPath "http://www.com/file")
        pathOf "https://www.com/file" `shouldBe` (UrlPath "https://www.com/file")
        
        pathOf "//home/file"          `shouldBe` (AbsPath $ Path "/home/file.hsfiles")
        pathOf "/home/file"           `shouldBe` (AbsPath $ Path "/home/file.hsfiles")
        pathOf "/home/file.hsfiles"   `shouldBe` (AbsPath $ Path "/home/file.hsfiles")
        
        pathOf "with/slash"           `shouldBe` (RelPath $ Path "with/slash.hsfiles")
        pathOf "with:colon"           `shouldBe` (RelPath $ Path "with:colon.hsfiles")
        pathOf "name"                 `shouldBe` (RelPath $ Path "name.hsfiles")
        pathOf "name.hsfile"          `shouldBe` (RelPath $ Path "name.hsfile.hsfiles")
        pathOf "name.hsfiles"         `shouldBe` (RelPath $ Path "name.hsfiles")
        pathOf "c:\\home\\file"       `shouldBe` (RelPath $ Path "c:\\home\\file.hsfiles")
        pathOf ""                     `shouldBe` (RelPath $ Path ".hsfiles")

