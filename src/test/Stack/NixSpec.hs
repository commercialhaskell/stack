{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Stack.NixSpec where

import Control.Exception
import Control.Monad.Logger
import Data.Monoid
import Network.HTTP.Conduit (Manager)
import Network.HTTP.Client.TLS (getGlobalManager)
import Path
import Prelude -- to remove the warning about Data.Monoid being redundant on GHC 7.10
import Stack.Config
import Stack.Config.Nix
import Stack.Types.Config
import Stack.Types.Compiler
import Stack.Types.Nix
import Stack.Types.StackT
import Stack.Types.Version
import System.Directory
import System.Environment
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

sampleConfig :: String
sampleConfig =
  "resolver: lts-2.10\n" ++
  "packages: ['.']\n" ++
  "system-ghc: true\n" ++
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
  manager <- getGlobalManager
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
      nixEnable (configNix $ lcConfig lc) `shouldBe` True
    it "sees that the only package asked for is glpk and asks for the correct GHC derivation" $
      \T{..} -> inTempDir $ do
        writeFile (toFilePath stackDotYaml) sampleConfig
        lc <- loadConfig' manager
        nixPackages (configNix $ lcConfig lc) `shouldBe` ["glpk"]
        v <- parseVersion "7.10.3"
        nixCompiler (GhcVersion v) `shouldBe` "haskell.compiler.ghc7103"
