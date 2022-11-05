{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stack.ConfigSpec where

import Control.Arrow
import Distribution.Verbosity (verbose)
import Pantry.Internal.AesonExtended
import Data.Yaml
import Pantry.Internal (pcHpackExecutable)
import Path
import Path.IO hiding (withSystemTempDir)
import Stack.Config
import Stack.Prelude
import Stack.Runners
import Stack.Types.Config
import Stack.Options.GlobalParser (globalOptsFromMonoid)
import System.Directory
import System.Environment
import System.IO (writeFile)
import Test.Hspec

sampleConfig :: String
sampleConfig =
  "resolver: lts-19.22\n" ++
  "packages: ['.']\n"

buildOptsConfig :: String
buildOptsConfig =
  "resolver: lts-19.22\n" ++
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
  "resolver: lts-19.22\n" ++
  "with-hpack: /usr/local/bin/hpack\n" ++
  "packages: ['.']\n"

resolverConfig :: String
resolverConfig =
  "resolver: lts-19.22\n" ++
  "packages: ['.']\n"

snapshotConfig :: String
snapshotConfig =
  "snapshot: lts-19.22\n" ++
  "packages: ['.']\n"

resolverSnapshotConfig :: String
resolverSnapshotConfig =
  "resolver: lts-19.22\n" ++
  "snapshot: lts-19.22\n" ++
  "packages: ['.']\n"

stackDotYaml :: Path Rel File
stackDotYaml = either impureThrow id (parseRelFile "stack.yaml")

setup :: IO ()
setup = unsetEnv "STACK_YAML"

noException :: Selector SomeException
noException = const False

spec :: Spec
spec = beforeAll setup $ do
  let logLevel = LevelOther "silent"
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

  describe "parseProjectAndConfigMonoid" $ do
    let loadProject' fp inner = do
          globalOpts <- globalOptsFromMonoid False mempty
          withRunnerGlobal globalOpts { globalLogLevel = logLevel } $ do
              iopc <- loadConfigYaml (
                parseProjectAndConfigMonoid (parent fp)
                ) fp
              ProjectAndConfigMonoid project _ <- liftIO iopc
              liftIO $ inner project

        toAbsPath path = do
          parentDir <- getCurrentDirectory >>= parseAbsDir
          pure (parentDir </> path)

        loadProject config inner = do
          yamlAbs <- toAbsPath stackDotYaml
          writeFile (toFilePath yamlAbs) config
          loadProject' yamlAbs inner

    it "parses snapshot using 'resolver'" $ inTempDir $ do
      loadProject resolverConfig $ \Project{..} ->
        projectResolver `shouldBe` RSLSynonym (LTS 19 22)

    it "parses snapshot using 'snapshot'" $ inTempDir $ do
      loadProject snapshotConfig $ \Project{..} ->
        projectResolver `shouldBe` RSLSynonym (LTS 19 22)

    it "throws if both 'resolver' and 'snapshot' are present" $ inTempDir $ do
      loadProject resolverSnapshotConfig (const (pure ()))
        `shouldThrow` anyException

  describe "loadConfig" $ do
    let loadConfig' inner = do
          globalOpts <- globalOptsFromMonoid False mempty
          withRunnerGlobal globalOpts { globalLogLevel = logLevel } $
            loadConfig inner
    -- TODO(danburton): make sure parent dirs also don't have config file
    it "works even if no config file exists" $ example $
      loadConfig' $ const $ pure ()

    it "works with a blank config file" $ inTempDir $ do
      writeFile (toFilePath stackDotYaml) ""
      -- TODO(danburton): more specific test for exception
      loadConfig' (const (pure ())) `shouldThrow` anyException

    let configOverrideHpack = pcHpackExecutable . view pantryConfigL

    it "parses config option with-hpack" $ inTempDir $ do
      writeFile (toFilePath stackDotYaml) hpackConfig
      loadConfig' $ \config ->
        liftIO $ configOverrideHpack config `shouldBe`
        HpackCommand "/usr/local/bin/hpack"

    it "parses config bundled Hpack" $ inTempDir $ do
      writeFile (toFilePath stackDotYaml) sampleConfig
      loadConfig' $ \config ->
        liftIO $ configOverrideHpack config `shouldBe` HpackBundled

    it "parses build config options" $ inTempDir $ do
     writeFile (toFilePath stackDotYaml) buildOptsConfig
     loadConfig' $ \config -> liftIO $ do
      let BuildOpts{..} = configBuild  config
      boptsLibProfile `shouldBe` True
      boptsExeProfile `shouldBe` True
      boptsHaddock `shouldBe` True
      boptsHaddockDeps `shouldBe` Just True
      boptsInstallExes `shouldBe` True
      boptsPreFetch `shouldBe` True
      boptsKeepGoing `shouldBe` Just True
      boptsKeepTmpFiles `shouldBe` True
      boptsForceDirty `shouldBe` True
      boptsTests `shouldBe` True
      boptsTestOpts `shouldBe` TestOpts { toRerunTests = True
                                        , toAdditionalArgs = ["-fprof"]
                                        , toCoverage = True
                                        , toDisableRun = True
                                        , toMaximumTimeSeconds = Nothing
                                        , toAllowStdin = False
                                        }
      boptsBenchmarks `shouldBe` True
      boptsBenchmarkOpts `shouldBe` BenchmarkOpts { beoAdditionalArgs = Just "-O2"
                                                  , beoDisableRun = True
                                                  }
      boptsReconfigure `shouldBe` True
      boptsCabalVerbose `shouldBe` CabalVerbosity verbose

    it "finds the config file in a parent directory" $ inTempDir $ do
      writeFile "package.yaml" "name: foo"
      writeFile (toFilePath stackDotYaml) sampleConfig
      parentDir <- getCurrentDirectory >>= parseAbsDir
      let childDir = "child"
      createDirectory childDir
      setCurrentDirectory childDir
      loadConfig' $ \config -> liftIO $ do
        bc <- runRIO config $ withBuildConfig ask
        view projectRootL bc `shouldBe` parentDir

    it "respects the STACK_YAML env variable" $ inTempDir $ do
      withSystemTempDir "config-is-here" $ \dir -> do
        let stackYamlFp = toFilePath (dir </> stackDotYaml)
        writeFile stackYamlFp sampleConfig
        writeFile (toFilePath dir ++ "/package.yaml") "name: foo"
        withEnvVar "STACK_YAML" stackYamlFp $ loadConfig' $ \config -> liftIO $ do
          BuildConfig{..} <- runRIO config $ withBuildConfig ask
          bcStackYaml `shouldBe` dir </> stackDotYaml
          parent bcStackYaml `shouldBe` dir

    it "STACK_YAML can be relative" $ inTempDir $ do
        parentDir <- getCurrentDirectory >>= parseAbsDir
        let childRel = either impureThrow id (parseRelDir "child")
            yamlRel = childRel </> either impureThrow id (parseRelFile "some-other-name.config")
            yamlAbs = parentDir </> yamlRel
            packageYaml = childRel </> either impureThrow id (parseRelFile "package.yaml")
        createDirectoryIfMissing True $ toFilePath $ parent yamlAbs
        writeFile (toFilePath yamlAbs) "resolver: ghc-9.0"
        writeFile (toFilePath packageYaml) "name: foo"
        withEnvVar "STACK_YAML" (toFilePath yamlRel) $ loadConfig' $ \config -> liftIO $ do
            BuildConfig{..} <- runRIO config $ withBuildConfig ask
            bcStackYaml `shouldBe` yamlAbs

  describe "defaultConfigYaml" $
    it "is parseable" $ \_ -> do
        curDir <- getCurrentDir
        let parsed :: Either String (Either String (WithJSONWarnings ConfigMonoid))
            parsed = parseEither (parseConfigMonoid curDir) <$> left show (decodeEither' defaultConfigYaml)
        case parsed of
            Right (Right _) -> pure () :: IO ()
            _ -> fail "Failed to parse default config yaml"
