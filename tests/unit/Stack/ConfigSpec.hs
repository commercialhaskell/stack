{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

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
import           Stack.Types.BuildConfig ( BuildConfig (..), projectRootL )
import           Stack.Types.BuildOpts
                   ( BenchmarkOpts (..), BuildOpts (..), CabalVerbosity (..)
                   , TestOpts (..)
                   )
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
      loadProject resolverConfig $ \project ->
        project.projectResolver `shouldBe` RSLSynonym (LTS 19 22)

    it "parses snapshot using 'snapshot'" $ inTempDir $ do
      loadProject snapshotConfig $ \project ->
        project.projectResolver `shouldBe` RSLSynonym (LTS 19 22)

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
      let bopts = config.build
      bopts.libProfile `shouldBe` True
      bopts.exeProfile `shouldBe` True
      bopts.haddock `shouldBe` True
      bopts.haddockDeps `shouldBe` Just True
      bopts.installExes `shouldBe` True
      bopts.preFetch `shouldBe` True
      bopts.keepGoing `shouldBe` Just True
      bopts.keepTmpFiles `shouldBe` True
      bopts.forceDirty `shouldBe` True
      bopts.tests `shouldBe` True
      bopts.testOpts `shouldBe` TestOpts
        { toRerunTests = True
        , toAdditionalArgs = ["-fprof"]
        , toCoverage = True
        , toDisableRun = True
        , toMaximumTimeSeconds = Nothing
        , toAllowStdin = True
        }
      bopts.benchmarks `shouldBe` True
      bopts.benchmarkOpts `shouldBe` BenchmarkOpts
         { beoAdditionalArgs = Just "-O2"
         , beoDisableRun = True
         }
      bopts.reconfigure `shouldBe` True
      bopts.cabalVerbose `shouldBe` CabalVerbosity verbose

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
        withEnvVar "STACK_YAML" stackYamlFp $
          loadConfig' $ \config -> liftIO $ do
            bc <- runRIO config $ withBuildConfig ask
            bc.stackYaml `shouldBe` dir </> stackDotYaml
            parent bc.stackYaml `shouldBe` dir

    it "STACK_YAML can be relative" $ inTempDir $ do
        parentDir <- getCurrentDirectory >>= parseAbsDir
        let childRel = either impureThrow id (parseRelDir "child")
            yamlRel =
              childRel </> either impureThrow id (parseRelFile "some-other-name.config")
            yamlAbs = parentDir </> yamlRel
            packageYaml =
              childRel </> either impureThrow id (parseRelFile "package.yaml")
        createDirectoryIfMissing True $ toFilePath $ parent yamlAbs
        writeFile (toFilePath yamlAbs) "resolver: ghc-9.0"
        writeFile (toFilePath packageYaml) "name: foo"
        withEnvVar "STACK_YAML" (toFilePath yamlRel) $
          loadConfig' $ \config -> liftIO $ do
            bc <- runRIO config $ withBuildConfig ask
            bc.stackYaml `shouldBe` yamlAbs

  describe "defaultConfigYaml" $
    it "is parseable" $ \_ -> do
      curDir <- getCurrentDir
      let parsed :: Either String (Either String (WithJSONWarnings ConfigMonoid))
          parsed = parseEither
            (parseConfigMonoid curDir) <$> left show (decodeEither' defaultConfigYaml)
      case parsed of
        Right (Right _) -> pure () :: IO ()
        _ -> fail "Failed to parse default config yaml"
