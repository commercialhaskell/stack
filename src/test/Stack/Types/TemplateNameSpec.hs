{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.TemplateNameSpec where

import Stack.Types.TemplateName
import Path.Internal
import System.Info (os)
import Test.Hspec

spec :: Spec
spec =
  describe "TemplateName" $ do
    describe "parseTemplateNameFromString" $ do
      let pathOf s = either error templatePath (parseTemplateNameFromString s)

      it "parses out the TemplatePath" $ do
        pathOf "github:user/name"     `shouldBe` RepoPath (RepoTemplatePath GitHub    "user" "name.hsfiles")
        pathOf "bitbucket:user/name"  `shouldBe` RepoPath (RepoTemplatePath Bitbucket "user" "name.hsfiles")
        pathOf "gitlab:user/name"     `shouldBe` RepoPath (RepoTemplatePath GitLab    "user" "name.hsfiles")

        pathOf "http://www.com/file"  `shouldBe` UrlPath "http://www.com/file"
        pathOf "https://www.com/file" `shouldBe` UrlPath "https://www.com/file"

        pathOf "name"                 `shouldBe` RelPath "name.hsfiles"        (Path "name.hsfiles")
        pathOf "name.hsfile"          `shouldBe` RelPath "name.hsfile.hsfiles" (Path "name.hsfile.hsfiles")
        pathOf "name.hsfiles"         `shouldBe` RelPath "name.hsfiles"        (Path "name.hsfiles")
        pathOf ""                     `shouldBe` RelPath ".hsfiles"            (Path ".hsfiles")

        if os == "mingw32"
        then do
          pathOf "//home/file"          `shouldBe` AbsPath (Path "\\\\home\\file.hsfiles")
          pathOf "/home/file"           `shouldBe` RelPath "/home/file.hsfiles" (Path "\\home\\file.hsfiles")
          pathOf "/home/file.hsfiles"   `shouldBe` RelPath "/home/file.hsfiles" (Path "\\home\\file.hsfiles")

          pathOf "c:\\home\\file"       `shouldBe` AbsPath                      (Path "C:\\home\\file.hsfiles")
          pathOf "with/slash"           `shouldBe` RelPath "with/slash.hsfiles" (Path "with\\slash.hsfiles")

          let colonAction =
                do
                  pure $! pathOf "with:colon"
          colonAction `shouldThrow` anyErrorCall

        else do
          pathOf "//home/file"          `shouldBe` AbsPath (Path "/home/file.hsfiles")
          pathOf "/home/file"           `shouldBe` AbsPath (Path "/home/file.hsfiles")
          pathOf "/home/file.hsfiles"   `shouldBe` AbsPath (Path "/home/file.hsfiles")

          pathOf "c:\\home\\file"       `shouldBe` RelPath "c:\\home\\file.hsfiles" (Path "c:\\home\\file.hsfiles")
          pathOf "with/slash"           `shouldBe` RelPath "with/slash.hsfiles"     (Path "with/slash.hsfiles")
          pathOf "with:colon"           `shouldBe` RelPath "with:colon.hsfiles"     (Path "with:colon.hsfiles")
