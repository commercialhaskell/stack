{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Test suite for the GhciScript DSL
module Stack.Ghci.ScriptSpec
  ( spec
  ) where

import qualified Data.Set as S
import           Distribution.ModuleName
import           Path
import           Path.Extra ( pathToLazyByteString )
import           Stack.Ghci.FakePaths
import           Stack.Ghci.Script
import           Stack.Prelude hiding ( fromString )
import qualified System.FilePath as FP
import           Test.Hspec

spec :: Spec
spec = do
  describe "GHCi" $ do
    describe "Script DSL" $ do

      describe "script" $ do
        it "should separate commands with a newline" $ do
          let dir = $(mkAbsDir $ defaultDrive FP.</> "src" FP.</> "package-a")
              script = cmdCdGhc dir
                    <> cmdAdd [Left (fromString "Lib.A")]
          scriptToLazyByteString script `shouldBe`
            ":cd-ghc " <> pathToLazyByteString dir <> "\n:add Lib.A\n"

      describe ":add" $ do
        it "should not render empty add commands" $ do
          let script = cmdAdd S.empty
          scriptToLazyByteString script `shouldBe` ""

        it "should ensure that a space exists between each module in an add command" $ do
          let script = cmdAdd (S.fromList [Left (fromString "Lib.A"), Left (fromString "Lib.B")])
          scriptToLazyByteString script `shouldBe` ":add Lib.A Lib.B\n"

      describe ":add (by file)" $ do
        it "should render a full file path" $ do
          let file = $(mkAbsFile $ defaultDrive FP.</> "Users" FP.</> "someone" FP.</> "src" FP.</> "project" FP.</> "package-a" FP.</> "src" FP.</> "Main.hs")
              script = cmdAdd (S.fromList [Right file])
          scriptToLazyByteString script `shouldBe`
            ":add " <> pathToLazyByteString file <> "\n"

      describe ":cd-ghc" $ do
        it "should render a full absolute path" $ do
          let dir = $(mkAbsDir $ defaultDrive FP.</> "Users" FP.</> "someone" FP.</> "src" FP.</> "project" FP.</> "package-a")
              script = cmdCdGhc dir
          scriptToLazyByteString script `shouldBe`
            ":cd-ghc " <> pathToLazyByteString dir <> "\n"

      describe ":module" $ do
        it "should render empty module as ':module +'" $ do
          let script = cmdModule []
          scriptToLazyByteString script `shouldBe` ":module +\n"

        it "should ensure that a space exists between each module in a module command" $ do
          let script = cmdModule [fromString "Lib.A", fromString "Lib.B"]
          scriptToLazyByteString script `shouldBe` ":module + Lib.A Lib.B\n"
