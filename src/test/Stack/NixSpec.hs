{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Stack.NixSpec where

import Control.Exception
import Control.Monad.Logger
import Data.Monoid
import Network.HTTP.Conduit (Manager)
import Path
import Prelude -- to remove the warning about Data.Monoid being redundant on GHC 7.10
import Stack.Config
import Stack.Config.Nix
import Stack.Types.Config
import Stack.Types.Nix
import Stack.Types.StackT
import System.Directory
import System.Environment
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

sampleConfig :: String
sampleConfig =
  "resolver: lts-2.10\n" ++
  "packages: ['.']\n" ++
  "nix:\n" ++
  "   enable: True\n" ++
  "   packages: [glpk]"

stackDotYaml :: Path Rel File
stackDotYaml = $(mkRelFile "stack.yaml")

data T = T
  { manager :: Manager
  }

setup :: IO T
setup = do
  manager <- newTLSManager
  unsetEnv "STACK_YAML"
  return T{..}

teardown :: T -> IO ()
teardown _ = return ()

spec :: Spec
spec = beforeAll setup $ afterAll teardown $ do
  let loadConfig' m = runStackLoggingT m LevelDebug False False (loadConfig mempty Nothing Nothing)
      inTempDir action = do
        currentDirectory <- getCurrentDirectory
        withSystemTempDirectory "Stack_ConfigSpec" $ \tempDir -> do
          let enterDir = setCurrentDirectory tempDir
              exitDir = setCurrentDirectory currentDirectory
          bracket_ enterDir exitDir action
  describe "nix" $ do
    it "sees that the nix shell is enabled" $ \T{..} -> inTempDir $ do
      writeFile (toFilePath stackDotYaml) sampleConfig
      lc <- loadConfig' manager
      (nixEnable $ configNix $ lcConfig lc) `shouldBe` True
    it "sees that the only package asked for is glpk and adds GHC from nixpkgs mirror of LTS resolver" $ \T{..} -> inTempDir $ do
      writeFile (toFilePath stackDotYaml) sampleConfig
      lc <- loadConfig' manager
      (nixPackages $ configNix $ lcConfig lc) `shouldBe` ["glpk"]
      nixCompiler (lcConfig lc) Nothing Nothing `shouldBe` "haskell.packages.lts-2_10.ghc"
