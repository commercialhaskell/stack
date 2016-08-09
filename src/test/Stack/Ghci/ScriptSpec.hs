{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Test suite for the GhciScript DSL
module Stack.Ghci.ScriptSpec where

import           Data.Monoid
import qualified Data.Set as S
import           Distribution.ModuleName
import           Test.Hspec
import           Path

import           Stack.Ghci.Script

spec :: Spec
spec = do
  describe "GHCi" $ do
    describe "Script DSL" $ do

      describe "script" $ do
        it "should seperate commands with a newline" $ do
          let script = cmdCdGhc $(mkAbsDir "/src/package-a")
                    <> cmdAdd [fromString "Lib.A"]
          scriptToLazyByteString script `shouldBe`
            ":cd-ghc /src/package-a/\n:add Lib.A\n"

      describe ":add" $ do
        it "should not render empty add commands" $ do
          let script = cmdAdd []
          scriptToLazyByteString script `shouldBe` ""

        it "should ensure that a space exists between each module in an add command" $ do
          let script = cmdAdd (S.fromList [fromString "Lib.A", fromString "Lib.B"])
          scriptToLazyByteString script `shouldBe` ":add Lib.A Lib.B\n"

      describe ":add (by file)" $ do
        it "should render a full file path" $ do
          let script = cmdAddFile $(mkAbsFile "/Users/someone/src/project/package-a/src/Main.hs")
          scriptToLazyByteString script `shouldBe`
            ":add /Users/someone/src/project/package-a/src/Main.hs\n"

      describe ":cd-ghc" $ do
        it "should render a full absolute path" $ do
          let script = cmdCdGhc $(mkAbsDir "/Users/someone/src/project/package-a")
          scriptToLazyByteString script `shouldBe`
            ":cd-ghc /Users/someone/src/project/package-a/\n"

      describe ":module" $ do
        it "should render empty module as ':module +'" $ do
          let script = cmdModule []
          scriptToLazyByteString script `shouldBe` ":module +\n"

        it "should ensure that a space exists between each module in a module command" $ do
          let script = cmdModule [fromString "Lib.A", fromString "Lib.B"]
          scriptToLazyByteString script `shouldBe` ":module + Lib.A Lib.B\n"
