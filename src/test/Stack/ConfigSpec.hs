{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
module Stack.ConfigSpec where

import Control.Applicative
import Control.Exception
import Control.Monad.Logger
import Data.Aeson.Extended
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Yaml
import Path
import Path.IO
import Prelude -- Fix redundant import warnings
import Stack.Config
import Stack.Types.Config
import Stack.Types.StackT
import System.Directory
import System.Environment
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

sampleConfig :: String
sampleConfig =
  "resolver: lts-2.10\n" ++
  "packages: ['.']\n"

buildOptsConfig :: String
buildOptsConfig =
  "resolver: lts-2.10\n" ++
  "packages: ['.']\n" ++
  "build:\n" ++
  "  library-profiling: true\n" ++
  "  executable-profiling: true\n" ++
  "  haddock: true\n" ++
  "  haddock-deps: true\n" ++
  "  copy-bins: true\n" ++
  "  prefetch: true\n" ++
  "  force-dirty: true\n" ++
  "  keep-going: true\n" ++
  "  test: true\n" ++
  "  test-arguments:\n" ++
  "    rerun-tests: true\n" ++
  "    additional-args: ['-fprof']\n" ++
  "    coverage: true\n" ++
  "    no-run-tests: true\n" ++
  "  bench: true\n" ++
  "  benchmark-opts:\n" ++
  "    benchmark-arguments: -O2\n" ++
  "    no-run-benchmarks: true\n" ++
  "  reconfigure: true\n" ++
  "  cabal-verbose: true\n"

stackDotYaml :: Path Rel File
stackDotYaml = $(mkRelFile "stack.yaml")

setup :: IO ()
setup = unsetEnv "STACK_YAML"

noException :: Selector SomeException
noException = const False

spec :: Spec
spec = beforeAll setup $ do
  let logLevel = LevelDebug
  -- TODO(danburton): not use inTempDir
  let inTempDir action = do
        currentDirectory <- getCurrentDirectory
        withSystemTempDirectory "Stack_ConfigSpec" $ \tempDir -> do
          let enterDir = setCurrentDirectory tempDir
          let exitDir = setCurrentDirectory currentDirectory
          bracket_ enterDir exitDir action
  -- TODO(danburton): a safer version of this?
  let withEnvVar name newValue action = do
        originalValue <- fromMaybe "" <$> lookupEnv name
        let setVar = setEnv name newValue
        let resetVar = setEnv name originalValue
        bracket_ setVar resetVar action

  describe "loadConfig" $ do
    let loadConfig' = runStackT () logLevel True False ColorAuto False (loadConfig mempty Nothing Nothing)
    let loadBuildConfigRest = runStackT () logLevel True False ColorAuto False
    -- TODO(danburton): make sure parent dirs also don't have config file
    it "works even if no config file exists" $ example $ do
      _config <- loadConfig'
      return ()

    it "works with a blank config file" $ inTempDir $ do
      writeFile (toFilePath stackDotYaml) ""
      -- TODO(danburton): more specific test for exception
      loadConfig' `shouldThrow` anyException

    it "parses build config options" $ inTempDir $ do
      writeFile (toFilePath stackDotYaml) buildOptsConfig
      BuildOpts{..} <- configBuild . lcConfig <$> loadConfig'
      boptsLibProfile `shouldBe` True
      boptsExeProfile `shouldBe` True
      boptsHaddock `shouldBe` True
      boptsHaddockDeps `shouldBe` Just True
      boptsInstallExes `shouldBe` True
      boptsPreFetch `shouldBe` True
      boptsKeepGoing `shouldBe` Just True
      boptsForceDirty `shouldBe` True
      boptsTests `shouldBe` True
      boptsTestOpts `shouldBe` TestOpts {toRerunTests = True
                                         ,toAdditionalArgs = ["-fprof"]
                                         ,toCoverage = True
                                         ,toDisableRun = True}
      boptsBenchmarks `shouldBe` True
      boptsBenchmarkOpts `shouldBe` BenchmarkOpts {beoAdditionalArgs = Just "-O2"
                                                   ,beoDisableRun = True}
      boptsReconfigure `shouldBe` True
      boptsCabalVerbose `shouldBe` True

    it "finds the config file in a parent directory" $ inTempDir $ do
      writeFile (toFilePath stackDotYaml) sampleConfig
      parentDir <- getCurrentDirectory >>= parseAbsDir
      let childDir = "child"
      createDirectory childDir
      setCurrentDirectory childDir
      LoadConfig{..} <- loadConfig'
      bc <- loadBuildConfigRest (lcLoadBuildConfig Nothing)
      view projectRootL bc `shouldBe` parentDir

    it "respects the STACK_YAML env variable" $ inTempDir $ do
      withSystemTempDir "config-is-here" $ \dir -> do
        let stackYamlFp = toFilePath (dir </> stackDotYaml)
        writeFile stackYamlFp sampleConfig
        withEnvVar "STACK_YAML" stackYamlFp $ do
          LoadConfig{..} <- loadConfig'
          BuildConfig{..} <- loadBuildConfigRest
                                (lcLoadBuildConfig Nothing)
          bcStackYaml bcLocal `shouldBe` dir </> stackDotYaml
          parent (bcStackYaml bcLocal)`shouldBe` dir

    it "STACK_YAML can be relative" $ inTempDir $ do
        parentDir <- getCurrentDirectory >>= parseAbsDir
        let childRel = $(mkRelDir "child")
            yamlRel = childRel </> $(mkRelFile "some-other-name.config")
            yamlAbs = parentDir </> yamlRel
        createDirectoryIfMissing True $ toFilePath $ parent yamlAbs
        writeFile (toFilePath yamlAbs) "resolver: ghc-7.8"
        withEnvVar "STACK_YAML" (toFilePath yamlRel) $ do
            LoadConfig{..} <- loadConfig'
            BuildConfig{..} <- loadBuildConfigRest
                                (lcLoadBuildConfig Nothing)
            bcStackYaml bcLocal `shouldBe` yamlAbs

  describe "defaultConfigYaml" $
    it "is parseable" $ \_ -> do
        let parsed :: Either String (WithJSONWarnings ConfigMonoid)
            parsed = decodeEither defaultConfigYaml
        isRight parsed `shouldBe` True
