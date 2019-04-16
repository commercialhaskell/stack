{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.ModuleInterfaceSpec where

import           Data.Foldable         (traverse_)
import           Data.Semigroup        ((<>))
import           Distribution.System   (Arch (..), buildArch)
import qualified Stack.ModuleInterface as Iface
import           Stack.Prelude         hiding (Version)
import           System.Directory      (doesFileExist)
import           Test.Hspec            (Spec, describe, it, shouldBe)

type Version = String

versions :: [Version]
versions = ["822", "844", "864"]

spec :: Spec
spec = describe "should succesfully deserialize interface from" $ traverse_ deserialize versions

deserialize :: Version -> Spec
deserialize v =
    it ("GHC" <> v) $ do
        let arch =
                case buildArch of
                    I386 -> "x32"
                    _    -> "x64"
            ifacePath = "test/files/iface/" <> arch <> "/ghc" <> v <> "/Main.hi"
        exists <- doesFileExist ifacePath
        when exists $ do
            result <- Iface.fromFile ifacePath
            case result of
                (Left msg) -> fail msg
                (Right iface) -> do
                    hasExpectedUsage iface `shouldBe` True
                    hasExpectedModule iface `shouldBe` True

-- | `Usage` is the name given by GHC to TH dependency
hasExpectedUsage :: Iface.Interface -> Bool
hasExpectedUsage =
    elem "Test.h" . fmap Iface.unUsage . Iface.unList . Iface.usage

hasExpectedModule :: Iface.Interface -> Bool
hasExpectedModule =
    elem "X" . fmap fst . Iface.unList . Iface.dmods . Iface.deps
