{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.NixSpec
  ( sampleConfigNixEnabled
  , sampleConfigNixDisabled
  , setup
  , spec
  ) where

import Data.Maybe (fromJust)
import Options.Applicative
import Path
import Prelude (writeFile)
import Stack.Config
import Stack.Config.Nix
import Stack.Constants
import Stack.Options.GlobalParser (globalOptsFromMonoid)
import Stack.Options.NixParser
import Stack.Prelude
import Stack.Runners
import Stack.Types.Config
import Stack.Types.Nix
import System.Directory
import System.Environment
import Test.Hspec

sampleConfigNixEnabled :: String
sampleConfigNixEnabled =
  "resolver: lts-19.22\n" ++
  "packages: ['.']\n" ++
  "system-ghc: true\n" ++
  "nix:\n" ++
  "   enable: True\n" ++
  "   packages: [glpk]"

sampleConfigNixDisabled :: String
sampleConfigNixDisabled =
  "resolver: lts-19.22\n" ++
  "packages: ['.']\n" ++
  "nix:\n" ++
  "   enable: False"

setup :: IO ()
setup = unsetEnv "STACK_YAML"

spec :: Spec
spec = beforeAll setup $ do
  let loadConfig' :: ConfigMonoid -> (Config -> IO ()) -> IO ()
      loadConfig' cmdLineArgs inner = do
        globalOpts <- globalOptsFromMonoid False mempty { globalMonoidConfigMonoid = cmdLineArgs }
        withRunnerGlobal globalOpts { globalLogLevel = LevelOther "silent" } $
          loadConfig (liftIO . inner)
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
  let trueOnNonWindows = not osIsWindows
  describe "nix disabled in config file" $
    around_ (withStackDotYaml sampleConfigNixDisabled) $ do
      it "sees that the nix shell is not enabled" $ loadConfig' mempty $ \config ->
        nixEnable (configNix config) `shouldBe` False
      describe "--nix given on command line" $
        it "sees that the nix shell is enabled" $
          loadConfig' (parseOpts ["--nix"]) $ \config ->
          nixEnable (configNix config) `shouldBe` trueOnNonWindows
      describe "--nix-pure given on command line" $
        it "sees that the nix shell is enabled" $
          loadConfig' (parseOpts ["--nix-pure"]) $ \config ->
          nixEnable (configNix config) `shouldBe` trueOnNonWindows
      describe "--no-nix given on command line" $
        it "sees that the nix shell is not enabled" $
          loadConfig' (parseOpts ["--no-nix"]) $ \config ->
          nixEnable (configNix config) `shouldBe` False
      describe "--no-nix-pure given on command line" $
        it "sees that the nix shell is not enabled" $
          loadConfig' (parseOpts ["--no-nix-pure"]) $ \config ->
          nixEnable (configNix config) `shouldBe` False
  describe "nix enabled in config file" $
    around_ (withStackDotYaml sampleConfigNixEnabled) $ do
      it "sees that the nix shell is enabled" $
        loadConfig' mempty $ \config ->
        nixEnable (configNix config) `shouldBe` trueOnNonWindows
      describe "--no-nix given on command line" $
        it "sees that the nix shell is not enabled" $
          loadConfig' (parseOpts ["--no-nix"]) $ \config ->
          nixEnable (configNix config) `shouldBe` False
      describe "--nix-pure given on command line" $
        it "sees that the nix shell is enabled" $
          loadConfig' (parseOpts ["--nix-pure"]) $ \config ->
          nixEnable (configNix config) `shouldBe` trueOnNonWindows
      describe "--no-nix-pure given on command line" $
        it "sees that the nix shell is enabled" $
          loadConfig' (parseOpts ["--no-nix-pure"]) $ \config ->
          nixEnable (configNix config) `shouldBe` trueOnNonWindows
      it "sees that the only package asked for is glpk and asks for the correct GHC derivation" $ loadConfig' mempty $ \config -> do
        nixPackages (configNix config) `shouldBe` ["glpk"]
        v <- parseVersionThrowing "9.0.2"
        ghc <- either throwIO pure $ nixCompiler (WCGhc v)
        ghc `shouldBe` "haskell.compiler.ghc902"
