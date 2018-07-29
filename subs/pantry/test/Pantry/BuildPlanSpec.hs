{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.BuildPlanSpec where

import           Data.Aeson.Extended (WithJSONWarnings(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Yaml (decode)
import           Stack.Types.BuildPlan
import           Test.Hspec

spec :: Spec
spec =
  describe "PackageLocation" $ do
    describe "Archive" $ do
      describe "github" $ do
        let decode' :: ByteString -> Maybe (WithJSONWarnings (PackageLocation Subdirs))
            decode' = decode

        it "'github' and 'commit' keys" $ do
          let contents :: ByteString
              contents =
                S8.pack
                  (unlines
                    [ "github: oink/town"
                    , "commit: abc123"
                    ])
          let expected :: PackageLocation Subdirs
              expected =
                PLArchive Archive
                  { archiveUrl = "https://github.com/oink/town/archive/abc123.tar.gz"
                  , archiveSubdirs = DefaultSubdirs
                  , archiveHash = Nothing
                  }
          decode' contents `shouldBe` Just (WithJSONWarnings expected [])

        it "'github', 'commit', and 'subdirs' keys" $ do
          let contents :: ByteString
              contents =
                S8.pack
                  (unlines
                    [ "github: oink/town"
                    , "commit: abc123"
                    , "subdirs:"
                    , "  - foo"
                    ])
          let expected :: PackageLocation Subdirs
              expected =
                PLArchive Archive
                  { archiveUrl = "https://github.com/oink/town/archive/abc123.tar.gz"
                  , archiveSubdirs = ExplicitSubdirs ["foo"]
                  , archiveHash = Nothing
                  }
          decode' contents `shouldBe` Just (WithJSONWarnings expected [])

        it "does not parse GitHub repo with no slash" $ do
          let contents :: ByteString
              contents =
                S8.pack
                  (unlines
                    [ "github: oink"
                    , "commit: abc123"
                    ])
          decode' contents `shouldBe` Nothing

        it "does not parse GitHub repo with leading slash" $ do
          let contents :: ByteString
              contents =
                S8.pack
                  (unlines
                    [ "github: /oink"
                    , "commit: abc123"
                    ])
          decode' contents `shouldBe` Nothing

        it "does not parse GitHub repo with trailing slash" $ do
          let contents :: ByteString
              contents =
                S8.pack
                  (unlines
                    [ "github: oink/"
                    , "commit: abc123"
                    ])
          decode' contents `shouldBe` Nothing

        it "does not parse GitHub repo with more than one slash" $ do
          let contents :: ByteString
              contents =
                S8.pack
                  (unlines
                    [ "github: oink/town/here"
                    , "commit: abc123"
                    ])
          decode' contents `shouldBe` Nothing
