{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Stack.ConfigSpec where

import Control.Arrow
import Data.Aeson.Extended
import Data.Yaml
import Pantry.Internal (pcHpackExecutable)
import Path
import Path.IO hiding (withSystemTempDir)
import Stack.Config
import Stack.Prelude
import Stack.Types.Config
import Stack.Types.Runner
import System.Directory
import System.Environment
import System.IO (writeFile)
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
  "  keep-tmp-files: true\n" ++
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

hpackConfig :: String
hpackConfig =
  "resolver: lts-2.10\n" ++
  "with-hpack: /usr/local/bin/hpack\n" ++
  "packages: ['.']\n"

stackDotYaml :: Path Rel File
stackDotYaml = either impureThrow id (parseRelFile "stack.yaml")

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
    let loadConfig' inner =
          withRunner logLevel True False ColorAuto mempty Nothing False $ \runner ->
            runRIO runner $ loadConfig mempty Nothing SYLDefault inner
    -- TODO(danburton): make sure parent dirs also don't have config file
    it "works even if no config file exists" $ example $
      loadConfig' $ const $ return ()

    it "works with a blank config file" $ inTempDir $ do
      writeFile (toFilePath stackDotYaml) ""
      -- TODO(danburton): more specific test for exception
      loadConfig' (const (return ())) `shouldThrow` anyException

    let configOverrideHpack = pcHpackExecutable . view pantryConfigL

    it "parses config option with-hpack" $ inTempDir $ do
      writeFile (toFilePath stackDotYaml) hpackConfig
      loadConfig' $ \lc ->
        liftIO $ configOverrideHpack (lcConfig lc) `shouldBe`
        HpackCommand "/usr/local/bin/hpack"

    it "parses config bundled hpack" $ inTempDir $ do
      writeFile (toFilePath stackDotYaml) sampleConfig
      loadConfig' $ \lc ->
        liftIO $ configOverrideHpack (lcConfig lc) `shouldBe` HpackBundled

    it "parses build config options" $ inTempDir $ do
     writeFile (toFilePath stackDotYaml) buildOptsConfig
     loadConfig' $ \lc -> liftIO $ do
      let BuildOpts{..} = configBuild  $ lcConfig lc
      boptsLibProfile `shouldBe` True
      boptsExeProfile `shouldBe` True
      boptsHaddock `shouldBe` True
      boptsHaddockDeps `shouldBe` Just True
      boptsInstallExes `shouldBe` True
      boptsPreFetch `shouldBe` True
      boptsKeepGoing `shouldBe` Just True
      boptsKeepTmpFiles `shouldBe` Just True
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
      loadConfig' $ \LoadConfig{..} -> liftIO $ do
        bc <- lcLoadBuildConfig Nothing
        view projectRootL bc `shouldBe` parentDir

    it "respects the STACK_YAML env variable" $ inTempDir $ do
      withSystemTempDir "config-is-here" $ \dir -> do
        let stackYamlFp = toFilePath (dir </> stackDotYaml)
        writeFile stackYamlFp sampleConfig
        withEnvVar "STACK_YAML" stackYamlFp $ loadConfig' $ \LoadConfig{..} -> liftIO $ do
          BuildConfig{..} <- lcLoadBuildConfig Nothing
          bcStackYaml `shouldBe` dir </> stackDotYaml
          parent bcStackYaml `shouldBe` dir

    it "STACK_YAML can be relative" $ inTempDir $ do
        parentDir <- getCurrentDirectory >>= parseAbsDir
        let childRel = either impureThrow id (parseRelDir "child")
            yamlRel = childRel </> either impureThrow id (parseRelFile "some-other-name.config")
            yamlAbs = parentDir </> yamlRel
        createDirectoryIfMissing True $ toFilePath $ parent yamlAbs
        writeFile (toFilePath yamlAbs) "resolver: ghc-7.8"
        withEnvVar "STACK_YAML" (toFilePath yamlRel) $ loadConfig' $ \LoadConfig{..} -> liftIO $ do
            BuildConfig{..} <- lcLoadBuildConfig Nothing
            bcStackYaml `shouldBe` yamlAbs

  describe "defaultConfigYaml" $
    it "is parseable" $ \_ -> do
        curDir <- getCurrentDir
        let parsed :: Either String (Either String (WithJSONWarnings ConfigMonoid))
            parsed = parseEither (parseConfigMonoid curDir) <$> left show (decodeEither' defaultConfigYaml)
        case parsed of
            Right (Right _) -> return () :: IO ()
            _ -> fail "Failed to parse default config yaml"
