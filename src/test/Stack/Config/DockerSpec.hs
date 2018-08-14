{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Stack.Config.DockerSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Stack.Prelude
import Stack.Types.Resolver
import RIO.Time (fromGregorian)
import Stack.Config.Docker (parseLtsName, addDefaultTag)

spec :: Spec
spec = do
  prop "parseLtsName" $ \(abs -> x) (abs -> y) -> do
    case ltsSnapshotLocation Nothing x y of
      (_, SLUrl url _ _) ->
        case parseLtsName url of
          Just (x', y') -> do
            x `shouldBe` x'
            y `shouldBe` y'
          Nothing -> error "parseLtsName failed"
      (_, loc) -> error $ show loc
  describe "addDefaultTag" $ do
    it "succeeds fails no resolver" $ addDefaultTag "foo/bar" Nothing Nothing `shouldBe` Nothing
    it "succeeds on LTS" $
      addDefaultTag
        "foo/bar"
        Nothing
        (Just $ ARResolver $ fst $ ltsSnapshotLocation Nothing 1 2)
      `shouldBe` Just "foo/bar:lts-1.2"
    it "fails on nightly" $
      addDefaultTag
        "foo/bar"
        Nothing
        (Just $ ARResolver $ fst $ nightlySnapshotLocation Nothing $ fromGregorian 2018 1 1)
      `shouldBe` Nothing
