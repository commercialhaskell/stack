{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.ModuleInterfaceSpec where

import           Data.Foldable         (traverse_)
import           Data.Semigroup        ((<>))
import qualified Stack.ModuleInterface as Iface
import           Stack.Prelude         hiding (Version)
import           Test.Hspec            (Spec, describe, it, shouldBe)

type Version = String
type Architecture = String
type Directory = FilePath
type Usage = String
type Module = ByteString

versions :: [Version]
versions = ["ghc822", "ghc844", "ghc864"]

-- TODO: add x32 when generated
archs :: [Architecture]
archs = ["x64"]

directories :: [FilePath]
directories = (<>) <$> ((<> "/") <$> archs) <*> versions

spec :: Spec
spec = describe "should succesfully deserialize interface from" $ traverse_ deserialize directories

deserialize :: Directory -> Spec
deserialize d = do
    it d $ do
        result <- Iface.fromFile $ "test/files/iface/" <> d <> "/Main.hi"
        case result of
            (Left msg) -> fail msg
            (Right iface) -> do
                hasExpectedUsage "Test.h" iface `shouldBe` True
                hasExpectedUsage "README.md" iface `shouldBe` True
                hasExpectedModule "X" iface `shouldBe` True

-- | `Usage` is the name given by GHC to TH dependency
hasExpectedUsage :: Usage -> Iface.Interface -> Bool
hasExpectedUsage u =
    elem u . fmap Iface.unUsage . Iface.unList . Iface.usage

hasExpectedModule :: Module -> Iface.Interface -> Bool
hasExpectedModule m =
    elem m . fmap fst . Iface.unList . Iface.dmods . Iface.deps
