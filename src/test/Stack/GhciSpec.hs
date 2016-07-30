{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Test suite for GHCi like applications including both GHCi and Intero.
module Stack.GhciSpec where

import           Data.Text (Text)
import qualified Data.Text as T
import           Test.Hspec
import           NeatInterpolation
import           Path

import           Stack.Ghci

spec :: Spec
spec = do
  describe "GHCi" $ do
    describe "Script rendering" $ do
      it "should render legacy script when given project:exe" $ do
        renderLegacyGhciScript [] (Just $(mkAbsFile "/Users/someone/src/project-a/exe/Main.hs"))
          `shouldBe` T.unpack ghciScript_projectWithMain

      it "should render legacy script when given project" $ do
        renderLegacyGhciScript ["Lib.A"] (Just $(mkAbsFile "/Users/someone/src/project-a/exe/Main.hs"))
          `shouldBe` T.unpack ghciScript_projectWithLibAndMain

      it "should render legacy script when given multiple project:lib" $ do
        renderLegacyGhciScript ["Lib.A", "Lib.B"] Nothing
          `shouldBe` T.unpack ghciScript_multipleProjectsWithLib

-- Exptected GHCi scripts

ghciScript_projectWithMain :: Text
ghciScript_projectWithMain = [text|
:add
:add /Users/someone/src/project-a/exe/Main.hs
:module +
|]

ghciScript_projectWithLibAndMain :: Text
ghciScript_projectWithLibAndMain = [text|
:add Lib.A
:add /Users/someone/src/project-a/exe/Main.hs
:module + Lib.A
|]

ghciScript_multipleProjectsWithLib :: Text
ghciScript_multipleProjectsWithLib = [text|
:add Lib.A Lib.B

:module + Lib.A Lib.B
|]
