{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Stack.NixSpec where

import Control.Exception
import Control.Monad.Logger
import Data.Maybe
import Data.Monoid
import Options.Applicative
import Path
import Prelude -- to remove the warning about Data.Monoid being redundant on GHC 7.10
import Stack.Config
import Stack.Options.NixParser
import Stack.Config.Nix
import Stack.Types.Compiler
import Stack.Types.Config
import Stack.Types.Nix
import Stack.Types.StackT
import Stack.Types.Version
import System.Directory
import System.Environment
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

sampleConfigNixEnabled :: String
sampleConfigNixEnabled =
  "resolver: lts-2.10\n" ++
  "packages: ['.']\n" ++
  "system-ghc: true\n" ++
  "nix:\n" ++
  "   enable: True\n" ++
  "   packages: [glpk]"

sampleConfigNixDisabled :: String
sampleConfigNixDisabled =
  "resolver: lts-2.10\n" ++
  "packages: ['.']\n" ++
  "nix:\n" ++
  "   enable: False"

stackDotYaml :: Path Rel File
stackDotYaml = $(mkRelFile "stack.yaml")

setup :: IO ()
setup = unsetEnv "STACK_YAML"

spec :: Spec
spec = beforeAll setup $ do
  let loadConfig' cmdLineArgs = runStackT () LevelDebug True False ColorAuto False (loadConfig cmdLineArgs Nothing Nothing)
      inTempDir test = do
        currentDirectory <- getCurrentDirectory
        withSystemTempDirectory "Stack_ConfigSpec" $ \tempDir -> do
          let enterDir = setCurrentDirectory tempDir
              exitDir = setCurrentDirectory currentDirectory
          bracket_ enterDir exitDir test
      withStackDotYaml config test = inTempDir $ do
        writeFile (toFilePath stackDotYaml) config
        test
      parseNixOpts cmdLineOpts = fromJust $ getParseResult $ execParserPure
        defaultPrefs
        (info (nixOptsParser False) mempty)
        cmdLineOpts
      parseOpts cmdLineOpts = mempty { configMonoidNixOpts = parseNixOpts cmdLineOpts }
  describe "nix disabled in config file" $
    around_ (withStackDotYaml sampleConfigNixDisabled) $ do
      it "sees that the nix shell is not enabled" $ do
        lc <- loadConfig' mempty
        nixEnable (configNix $ lcConfig lc) `shouldBe` False
      describe "--nix given on command line" $
        it "sees that the nix shell is enabled" $ do
          lc <- loadConfig' (parseOpts ["--nix"])
          nixEnable (configNix $ lcConfig lc) `shouldBe` True
      describe "--nix-pure given on command line" $
        it "sees that the nix shell is enabled" $ do
          lc <- loadConfig' (parseOpts ["--nix-pure"])
          nixEnable (configNix $ lcConfig lc) `shouldBe` True
      describe "--no-nix given on command line" $
        it "sees that the nix shell is not enabled" $ do
          lc <- loadConfig' (parseOpts ["--no-nix"])
          nixEnable (configNix $ lcConfig lc) `shouldBe` False
      describe "--no-nix-pure given on command line" $
        it "sees that the nix shell is not enabled" $ do
          lc <- loadConfig' (parseOpts ["--no-nix-pure"])
          nixEnable (configNix $ lcConfig lc) `shouldBe` False
  describe "nix enabled in config file" $
    around_ (withStackDotYaml sampleConfigNixEnabled) $ do
      it "sees that the nix shell is enabled" $ do
        lc <- loadConfig' mempty
        nixEnable (configNix $ lcConfig lc) `shouldBe` True
      describe "--no-nix given on command line" $
        it "sees that the nix shell is not enabled" $ do
          lc <- loadConfig' (parseOpts ["--no-nix"])
          nixEnable (configNix $ lcConfig lc) `shouldBe` False
      describe "--nix-pure given on command line" $
        it "sees that the nix shell is enabled" $ do
          lc <- loadConfig' (parseOpts ["--nix-pure"])
          nixEnable (configNix $ lcConfig lc) `shouldBe` True
      describe "--no-nix-pure given on command line" $
        it "sees that the nix shell is enabled" $ do
          lc <- loadConfig' (parseOpts ["--no-nix-pure"])
          nixEnable (configNix $ lcConfig lc) `shouldBe` True
      it "sees that the only package asked for is glpk and asks for the correct GHC derivation" $ do
        lc <- loadConfig' mempty
        nixPackages (configNix $ lcConfig lc) `shouldBe` ["glpk"]
        v <- parseVersion "7.10.3"
        nixCompiler (GhcVersion v) `shouldBe` "haskell.compiler.ghc7103"
