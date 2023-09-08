{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Config.DockerSpec
  ( spec
  ) where

import Test.Hspec
import Stack.Prelude
import Stack.Types.Resolver
import RIO.Time (fromGregorian)
import Stack.Config.Docker (addDefaultTag)

spec :: Spec
spec = do
  describe "addDefaultTag" $ do
    it "succeeds fails no resolver" $ addDefaultTag "foo/bar" Nothing Nothing `shouldBe` Nothing
    it "succeeds on LTS" $
      addDefaultTag
        "foo/bar"
        Nothing
        (Just $ ARResolver $ RSLSynonym $ LTS 1 2)
      `shouldBe` Just "foo/bar:lts-1.2"
    it "fails on nightly" $
      addDefaultTag
        "foo/bar"
        Nothing
        (Just $ ARResolver $ RSLSynonym $ Nightly $ fromGregorian 2018 1 1)
      `shouldBe` Nothing
