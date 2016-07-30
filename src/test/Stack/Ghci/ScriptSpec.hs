{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Test suite for the GhciScript DSL
module Stack.Ghci.ScriptSpec where

import Test.Hspec
import Path

import Stack.Ghci.Script

spec :: Spec
spec = do
  describe "GHCi" $ do
    describe "Script DSL" $ do

      describe ":add" $ do
        it "should not render empty add commands" $ do
          let script = appendCommand (Add []) emptyScript
          scriptToText script `shouldBe` ""

        it "should ensure that a space exists between each module in an add command" $ do
          let script = appendCommand (Add ["Lib.A", "Lib.B"]) emptyScript
          scriptToText script `shouldBe` ":add Lib.A Lib.B\n"

      describe ":cd-ghc" $ do
        it "should render a full absolute path" $ do
          let script = appendCommand (CdGhc $(mkAbsDir "/Users/someone/src/project/package-a")) emptyScript
          scriptToText script `shouldBe`
            ":cd-ghc /Users/someone/src/project/package-a/"

      describe ":module" $ do
        it "should render empty module as ':module +'" $ do
          let script = appendCommand (Module []) emptyScript
          scriptToText script `shouldBe` ":module +"

        it "should ensure that a space exists between each module in a module command" $ do
          let script = appendCommand (Module ["Lib.A", "Lib.B"]) emptyScript
          scriptToText script `shouldBe` ":module + Lib.A Lib.B\n"
