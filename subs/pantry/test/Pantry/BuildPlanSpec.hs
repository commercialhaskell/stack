{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Pantry.BuildPlanSpec where

import           Data.Aeson.Extended (WithJSONWarnings(..))
import           RIO
import qualified Data.ByteString.Char8 as S8
import           Data.Yaml (decodeThrow)
import           Pantry
import           Test.Hspec
import           Control.Monad.Catch (MonadThrow)
import           Data.List.NonEmpty (NonEmpty)

spec :: Spec
spec =
  describe "PackageLocation" $ do
    describe "Archive" $ do
      describe "github" $ do
        let decode' :: (HasCallStack, MonadThrow m) => ByteString -> m (WithJSONWarnings (Unresolved (NonEmpty PackageLocationImmutable)))
            decode' = decodeThrow

            decode'' :: HasCallStack => ByteString -> IO (NonEmpty PackageLocationImmutable)
            decode'' bs = do
              WithJSONWarnings unresolved warnings <- decode' bs
              unless (null warnings) $ error $ show warnings
              resolvePaths Nothing unresolved

        it "'github' and 'commit' keys" $ do
          let contents :: ByteString
              contents =
                S8.pack
                  (unlines
                    [ "github: oink/town"
                    , "commit: abc123"
                    ])
          let expected :: PackageLocationImmutable
              expected =
                PLIArchive
                  Archive
                    { archiveLocation = ALUrl "https://github.com/oink/town/archive/abc123.tar.gz"
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
          actual <- decode'' contents
          actual `shouldBe` pure expected

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
          let expected :: PackageLocationImmutable
              expected =
                PLIArchive
                  Archive
                    { archiveLocation = ALUrl "https://github.com/oink/town/archive/abc123.tar.gz"
                    , archiveHash = Nothing
                    , archiveSize = Nothing
                    , archiveSubdir = "foo"
                    }
                  PackageMetadata
                    { pmName = Nothing
                    , pmVersion = Nothing
                    , pmTreeKey = Nothing
                    , pmCabal = Nothing
                    }
          actual <- decode'' contents
          actual `shouldBe` pure expected

        it "does not parse GitHub repo with no slash" $ do
          let contents :: ByteString
              contents =
                S8.pack
                  (unlines
                    [ "github: oink"
                    , "commit: abc123"
                    ])
          void (decode' contents) `shouldBe` Nothing

        it "does not parse GitHub repo with leading slash" $ do
          let contents :: ByteString
              contents =
                S8.pack
                  (unlines
                    [ "github: /oink"
                    , "commit: abc123"
                    ])
          void (decode' contents) `shouldBe` Nothing

        it "does not parse GitHub repo with trailing slash" $ do
          let contents :: ByteString
              contents =
                S8.pack
                  (unlines
                    [ "github: oink/"
                    , "commit: abc123"
                    ])
          void (decode' contents) `shouldBe` Nothing

        it "does not parse GitHub repo with more than one slash" $ do
          let contents :: ByteString
              contents =
                S8.pack
                  (unlines
                    [ "github: oink/town/here"
                    , "commit: abc123"
                    ])
          void (decode' contents) `shouldBe` Nothing
