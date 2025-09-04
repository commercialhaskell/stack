{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Stack.ConfigSpec
  ( sampleConfig
  , buildOptsConfig
  , hpackConfig
  , resolverConfig
  , snapshotConfig
  , resolverSnapshotConfig
  , stackDotYaml
  , setup
  , noException
  , spec
  ) where

import           Control.Arrow ( left )
import           Data.Aeson.WarningParser ( WithJSONWarnings )
import           Data.Yaml ( decodeEither', parseEither )
import           Distribution.Verbosity ( verbose )
import           Pantry.Internal.Stackage ( pcHpackExecutable )
import           Path ( (</>), parent, parseAbsDir, parseRelDir, parseRelFile )
import           Path.IO ( getCurrentDir )
import           Stack.Config (defaultConfigYaml, loadConfig, loadConfigYaml )
import           Stack.Options.GlobalParser ( globalOptsFromMonoid )
import           Stack.Prelude
import           Stack.Runners ( withBuildConfig, withRunnerGlobal )
import           Stack.Types.BuildConfig ( BuildConfig (..), configFileRootL )
import           Stack.Types.BuildOpts
                   ( BenchmarkOpts (..), BuildOpts (..), HaddockOpts (..)
                   , TestOpts (..)
                   )
import           Stack.Types.BuildOptsMonoid ( CabalVerbosity (..), ProgressBarFormat (NoBar) )
import           Stack.Types.Config ( Config (..) )
import           Stack.Types.ConfigMonoid
                   ( ConfigMonoid (..), parseConfigMonoid )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.Project ( Project (..) )
import           Stack.Types.ProjectAndConfigMonoid
                   ( ProjectAndConfigMonoid (..), parseProjectAndConfigMonoid )
import           System.Directory
                   ( createDirectory, createDirectoryIfMissing
                   , getCurrentDirectory, setCurrentDirectory
                   )
import           System.Environment ( lookupEnv, setEnv, unsetEnv )
import           System.IO ( writeFile )
import           Test.Hspec
                   ( Selector, Spec, anyException, beforeAll, describe, example
                   , it, shouldBe, shouldThrow
                   )

sampleConfig :: String
sampleConfig =
  "snapshot: lts-24.8\n" ++
  "packages: ['.']\n"

buildOptsConfig :: String
buildOptsConfig =
  "snapshot: lts-24.8\n" ++
  "packages: ['.']\n" ++
  "build:\n" ++
  "  library-profiling: true\n" ++
  "  executable-profiling: true\n" ++
  "  library-stripping: false\n" ++
  "  executable-stripping: false\n" ++
  "  haddock: true\n" ++
  "  haddock-arguments:\n" ++
  "    haddock-args:\n" ++
  "    - \"--css=/home/user/my-css\"\n" ++
  "  open-haddocks: true\n" ++
  "  haddock-deps: true\n" ++
  "  haddock-executables: true\n" ++
  "  haddock-tests: true\n" ++
  "  haddock-benchmarks: true\n" ++
  "  haddock-internal: true\n" ++
  "  haddock-hyperlink-source: false\n" ++
  "  haddock-for-hackage: false\n" ++
  "  copy-bins: true\n" ++
  "  copy-compiler-tool: true\n" ++
  "  prefetch: true\n" ++
  "  keep-going: true\n" ++
  "  keep-tmp-files: true\n" ++
  "  force-dirty: true\n" ++
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
  "  cabal-verbosity: verbose\n" ++
  "  cabal-verbose: true\n" ++
  "  split-objs: true\n" ++
  "  skip-components: ['my-test']\n" ++
  "  interleaved-output: false\n" ++
  "  progress-bar: none\n" ++
  "  ddump-dir: my-ddump-dir\n"

buildOptsHaddockForHackageConfig :: String
buildOptsHaddockForHackageConfig =
  "snapshot: lts-24.8\n" ++
  "packages: ['.']\n" ++
  "build:\n" ++
  "  haddock: true\n" ++
  "  open-haddocks: true\n" ++
  "  haddock-deps: true\n" ++
  "  haddock-executables: true\n" ++
  "  haddock-tests: true\n" ++
  "  haddock-benchmarks: true\n" ++
  "  haddock-internal: true\n" ++
  "  haddock-hyperlink-source: false\n" ++
  "  haddock-for-hackage: true\n" ++
  "  force-dirty: false\n"

hpackConfig :: String
hpackConfig =
  "snapshot: lts-24.8\n" ++
  "with-hpack: /usr/local/bin/hpack\n" ++
  "packages: ['.']\n"

resolverConfig :: String
resolverConfig =
  "resolver: lts-24.8\n" ++
  "packages: ['.']\n"

snapshotConfig :: String
snapshotConfig =
  "snapshot: lts-24.8\n" ++
  "packages: ['.']\n"

resolverSnapshotConfig :: String
resolverSnapshotConfig =
  "resolver: lts-24.8\n" ++
  "snapshot: lts-24.8\n" ++
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
          globalOpts <- globalOptsFromMonoid "" Nothing False mempty
          withRunnerGlobal globalOpts { logLevel = logLevel } $ do
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
      loadProject resolverConfig $ \project ->
        project.snapshot `shouldBe` RSLSynonym (LTS 24 8)

    it "parses snapshot using 'snapshot'" $ inTempDir $ do
      loadProject snapshotConfig $ \project ->
        project.snapshot `shouldBe` RSLSynonym (LTS 24 8)

    it "throws if both 'resolver' and 'snapshot' are present" $ inTempDir $ do
      loadProject resolverSnapshotConfig (const (pure ()))
        `shouldThrow` anyException

  describe "loadConfig" $ do
    let loadConfig' inner = do
          globalOpts <- globalOptsFromMonoid "" Nothing False mempty
          withRunnerGlobal globalOpts { logLevel = logLevel } $
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
        let bopts = config.build
        bopts.libProfile `shouldBe` True
        bopts.exeProfile `shouldBe` True
        bopts.libStrip `shouldBe` False
        bopts.exeStrip `shouldBe` False
        bopts.buildHaddocks `shouldBe` True
        bopts.haddockOpts `shouldBe` HaddockOpts
          { additionalArgs = ["--css=/home/user/my-css"]
          }
        bopts.openHaddocks `shouldBe` True
        bopts.haddockDeps `shouldBe` Just True
        bopts.haddockExecutables `shouldBe` True
        bopts.haddockTests `shouldBe` True
        bopts.haddockBenchmarks `shouldBe` True
        bopts.haddockInternal `shouldBe` True
        bopts.haddockHyperlinkSource `shouldBe` False
        bopts.haddockForHackage `shouldBe` False
        bopts.installExes `shouldBe` True
        bopts.installCompilerTool `shouldBe` True
        bopts.preFetch `shouldBe` True
        bopts.keepGoing `shouldBe` Just True
        bopts.keepTmpFiles `shouldBe` True
        bopts.forceDirty `shouldBe` True
        bopts.tests `shouldBe` True
        bopts.testOpts `shouldBe` TestOpts
          { rerunTests = True
          , additionalArgs = ["-fprof"]
          , coverage = True
          , runTests = False
          , maximumTimeSeconds = Nothing
          , allowStdin = True
          }
        bopts.benchmarks `shouldBe` True
        bopts.benchmarkOpts `shouldBe` BenchmarkOpts
           { additionalArgs = Just "-O2"
           , runBenchmarks = False
           }
        bopts.reconfigure `shouldBe` True
        bopts.cabalVerbose `shouldBe` CabalVerbosity verbose
        bopts.splitObjs `shouldBe` True
        bopts.skipComponents `shouldBe` ["my-test"]
        bopts.interleavedOutput `shouldBe` False
        bopts.progressBar `shouldBe` NoBar
        bopts.ddumpDir `shouldBe` Just "my-ddump-dir"

    it "parses build config options with haddock-for-hackage" $ inTempDir $ do
      writeFile (toFilePath stackDotYaml) buildOptsHaddockForHackageConfig
      loadConfig' $ \config -> liftIO $ do
        let bopts = config.build
        bopts.buildHaddocks `shouldBe` True
        bopts.openHaddocks `shouldBe` False
        bopts.haddockDeps `shouldBe` Nothing
        bopts.haddockInternal `shouldBe` False
        bopts.haddockHyperlinkSource `shouldBe` True
        bopts.haddockForHackage `shouldBe` True
        bopts.forceDirty `shouldBe` True

    it "finds the config file in a parent directory" $ inTempDir $ do
      writeFile "package.yaml" "name: foo"
      writeFile (toFilePath stackDotYaml) sampleConfig
      parentDir <- getCurrentDirectory >>= parseAbsDir
      let childDir = "child"
      createDirectory childDir
      setCurrentDirectory childDir
      loadConfig' $ \config -> liftIO $ do
        bc <- runRIO config $ withBuildConfig ask
        view configFileRootL bc `shouldBe` parentDir

    it "respects the STACK_YAML env variable" $ inTempDir $ do
      withSystemTempDir "config-is-here" $ \dir -> do
        let stackYamlFp = toFilePath (dir </> stackDotYaml)
        writeFile stackYamlFp sampleConfig
        writeFile (toFilePath dir ++ "/package.yaml") "name: foo"
        withEnvVar "STACK_YAML" stackYamlFp $
          loadConfig' $ \config -> liftIO $ do
            bc <- runRIO config $ withBuildConfig ask
            bc.configFile `shouldBe` Right (dir </> stackDotYaml)

    it "STACK_YAML can be relative" $ inTempDir $ do
        parentDir <- getCurrentDirectory >>= parseAbsDir
        let childRel = either impureThrow id (parseRelDir "child")
            yamlRel =
              childRel </> either impureThrow id (parseRelFile "some-other-name.config")
            yamlAbs = parentDir </> yamlRel
            packageYaml =
              childRel </> either impureThrow id (parseRelFile "package.yaml")
        createDirectoryIfMissing True $ toFilePath $ parent yamlAbs
        writeFile (toFilePath yamlAbs) "snapshot: ghc-9.10.2"
        writeFile (toFilePath packageYaml) "name: foo"
        withEnvVar "STACK_YAML" (toFilePath yamlRel) $
          loadConfig' $ \config -> liftIO $ do
            bc <- runRIO config $ withBuildConfig ask
            bc.configFile `shouldBe` Right yamlAbs

  describe "defaultConfigYaml" $
    it "is parseable" $ \_ -> do
      curDir <- getCurrentDir
      let parsed :: Either String (Either String (WithJSONWarnings ConfigMonoid))
          parsed = parseEither
            (parseConfigMonoid curDir) <$> left show (decodeEither' defaultConfigYaml)
      case parsed of
        Right (Right _) -> pure () :: IO ()
        _ -> fail "Failed to parse default config yaml"
