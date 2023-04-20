{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.NixSpec
  ( sampleConfigNixEnabled
  , sampleConfigNixDisabled
  , setup
  , spec
  ) where

import           Data.Maybe ( fromJust )
import           Options.Applicative
                   ( defaultPrefs, execParserPure, getParseResult, info )
import           Prelude ( writeFile )
import           Stack.Config ( loadConfig )
import           Stack.Config.Nix ( nixCompiler )
import           Stack.Constants ( osIsWindows, stackDotYaml )
import           Stack.Options.GlobalParser ( globalOptsFromMonoid )
import           Stack.Options.NixParser ( nixOptsParser )
import           Stack.Prelude
import           Stack.Runners ( withRunnerGlobal )
import           Stack.Types.Config ( Config (..) )
import           Stack.Types.ConfigMonoid ( ConfigMonoid (..) )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.GlobalOptsMonoid ( GlobalOptsMonoid (..) )
import           Stack.Types.Nix ( NixOpts (..) )
import           System.Directory ( getCurrentDirectory, setCurrentDirectory )
import           System.Environment ( unsetEnv )
import           Test.Hspec ( Spec, around_, beforeAll, describe, it, shouldBe )

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
