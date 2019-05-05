{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HiFileParserSpec (spec) where

import           Data.Foldable         (traverse_)
import           Data.Semigroup        ((<>))
import qualified HiFileParser          as Iface
import           RIO
import           Test.Hspec            (Spec, describe, it, shouldBe)

type Version = String
type Directory = FilePath
type Usage = String
type Module = ByteString

versions32 :: [Version]
versions32 = ["ghc7103", "ghc802", "ghc822", "ghc844"]

versions64 :: [Version]
versions64 = ["ghc822", "ghc844", "ghc864"]

spec :: Spec
spec = describe "should succesfully deserialize x32 interface for" $ do
   traverse_ (deserialize check32) (("x32/" <>) <$> versions32)
   traverse_ (deserialize check64) (("x64/" <>) <$> versions64)

check32 :: Iface.Interface -> IO ()
check32 iface = do
    hasExpectedUsage "some-dependency.txt" iface `shouldBe` True

check64 :: Iface.Interface -> IO ()
check64 iface = do
    hasExpectedUsage "Test.h" iface `shouldBe` True
    hasExpectedUsage "README.md" iface `shouldBe` True
    hasExpectedModule "X" iface `shouldBe` True

deserialize :: (Iface.Interface -> IO ()) -> Directory -> Spec
deserialize check d = do
    it d $ do
        let ifacePath = "test-files/iface/" <> d <> "/Main.hi"
        result <- Iface.fromFile ifacePath
        case result of
          (Left msg)    -> fail msg
          (Right iface) -> check iface

-- | `Usage` is the name given by GHC to TH dependency
hasExpectedUsage :: Usage -> Iface.Interface -> Bool
hasExpectedUsage u =
    elem u . fmap Iface.unUsage . Iface.unList . Iface.usage

hasExpectedModule :: Module -> Iface.Interface -> Bool
hasExpectedModule m =
    elem m . fmap fst . Iface.unList . Iface.dmods . Iface.deps
