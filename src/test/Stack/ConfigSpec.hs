{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
module Stack.ConfigSpec where

import Control.Applicative
import Control.Monad.Logger
import Control.Exception
import Data.Maybe
import Data.Monoid
import Network.HTTP.Conduit (Manager)
import Path
import Path.IO
--import System.FilePath
import Prelude -- Fix redundant import warnings
import System.Directory
import System.Environment
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Stack.Config
import Stack.Types.Config
import Stack.Types.StackT

sampleConfig :: String
sampleConfig =
  "resolver: lts-2.10\n" ++
  "packages: ['.']\n"

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
    let loadConfig' m = runStackLoggingT m logLevel False False (loadConfig mempty Nothing Nothing)
    let loadBuildConfigRest m = runStackLoggingT m logLevel False False
    -- TODO(danburton): make sure parent dirs also don't have config file
    it "works even if no config file exists" $ \T{..} -> example $ do
      _config <- loadConfig' manager
      return ()

    it "works with a blank config file" $ \T{..} -> inTempDir $ do
      writeFile (toFilePath stackDotYaml) ""
      -- TODO(danburton): more specific test for exception
      loadConfig' manager `shouldThrow` anyException

    it "finds the config file in a parent directory" $ \T{..} -> inTempDir $ do
      writeFile (toFilePath stackDotYaml) sampleConfig
      parentDir <- getCurrentDirectory >>= parseAbsDir
      let childDir = "child"
      createDirectory childDir
      setCurrentDirectory childDir
      LoadConfig{..} <- loadConfig' manager
      bc@BuildConfig{..} <- loadBuildConfigRest manager
                            (lcLoadBuildConfig Nothing)
      bcRoot bc `shouldBe` parentDir

    it "respects the STACK_YAML env variable" $ \T{..} -> inTempDir $ do
      withSystemTempDir "config-is-here" $ \dir -> do
        let stackYamlFp = toFilePath (dir </> stackDotYaml)
        writeFile stackYamlFp sampleConfig
        withEnvVar "STACK_YAML" stackYamlFp $ do
          LoadConfig{..} <- loadConfig' manager
          BuildConfig{..} <- loadBuildConfigRest manager
                                (lcLoadBuildConfig Nothing)
          bcStackYaml `shouldBe` dir </> stackDotYaml
          parent bcStackYaml `shouldBe` dir

    it "STACK_YAML can be relative" $ \T{..} -> inTempDir $ do
        parentDir <- getCurrentDirectory >>= parseAbsDir
        let childRel = $(mkRelDir "child")
            yamlRel = childRel </> $(mkRelFile "some-other-name.config")
            yamlAbs = parentDir </> yamlRel
        createDirectoryIfMissing True $ toFilePath $ parent yamlAbs
        writeFile (toFilePath yamlAbs) "resolver: ghc-7.8"
        withEnvVar "STACK_YAML" (toFilePath yamlRel) $ do
            LoadConfig{..} <- loadConfig' manager
            BuildConfig{..} <- loadBuildConfigRest manager
                                (lcLoadBuildConfig Nothing)
            bcStackYaml `shouldBe` yamlAbs
