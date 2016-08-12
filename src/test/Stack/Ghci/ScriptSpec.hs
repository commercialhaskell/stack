{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Test suite for the GhciScript DSL
module Stack.Ghci.ScriptSpec where

import           Data.Monoid
import qualified Data.Set as S
import           Distribution.ModuleName
import           Test.Hspec
import qualified System.FilePath as FP
import           Stack.Ghci.PortableFakePaths
import           Path
import           Path.Extra (pathToLazyByteString)

import           Stack.Ghci.Script

spec :: Spec
spec = do
  describe "GHCi" $ do
    describe "Script DSL" $ do

      describe "script" $ do
        it "should seperate commands with a newline" $ do
          let dir = $(mkAbsDir $ defaultDrive FP.</> "src" FP.</> "package-a")
              script = cmdCdGhc dir
                    <> cmdAdd [fromString "Lib.A"]
          scriptToLazyByteString script `shouldBe`
            ":cd-ghc " <> pathToLazyByteString dir <> "\n:add Lib.A\n"

      describe ":add" $ do
        it "should not render empty add commands" $ do
          let script = cmdAdd []
          scriptToLazyByteString script `shouldBe` ""

        it "should ensure that a space exists between each module in an add command" $ do
          let script = cmdAdd (S.fromList [fromString "Lib.A", fromString "Lib.B"])
          scriptToLazyByteString script `shouldBe` ":add Lib.A Lib.B\n"

      describe ":add (by file)" $ do
        it "should render a full file path" $ do
          let file = $(mkAbsFile $ defaultDrive FP.</> "Users" FP.</> "someone" FP.</> "src" FP.</> "project" FP.</> "package-a" FP.</> "src" FP.</> "Main.hs")
              script = cmdAddFile file
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
