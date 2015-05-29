module Stack.ConfigSpec where

import Control.Applicative
import Control.Monad
import Control.Monad.Logger
import Control.Exception
import Data.Maybe
import Path
--import System.FilePath
import System.Directory
import System.IO.Temp
import System.Environment
import Test.Hspec

import Stack.Config
import Stack.Types.Config
import Stack.Types.StackT

spec :: Spec
spec = do
  manager <- runIO $ newTLSManager
  stackDotYaml <- runIO $ parseRelFile "stack.yaml"
  let logLevel = LevelDebug
  let inTempDir action = do
        currentDirectory <- getCurrentDirectory
        withSystemTempDirectory "Stack_ConfigSpec" $ \tempDir -> do
          let enterDir = setCurrentDirectory tempDir
          let exitDir = setCurrentDirectory currentDirectory
          bracket_ enterDir exitDir action
  let withEnvVar name newValue action = do
        originalValue <- fromMaybe "" <$> lookupEnv name
        let setVar = setEnv name newValue
        let resetVar = setEnv name originalValue
        bracket_ setVar resetVar action


  describe "loadConfig" $ do
    let loadConfig' = runStackLoggingT manager logLevel loadConfig

    -- TODO: make sure parent dirs also don't have config file
    it "works even if no config file exists" $ inTempDir $ do
      _config <- loadConfig'
      return ()

    -- TODO: should throw?
    it "works with a blank config file" $ inTempDir $ do
      writeFile (toFilePath stackDotYaml) ""
      _config <- loadConfig'
      return ()

    -- it "finds the config file in a parent directory" $ inTempDir $ do
    --   writeFile (toFilePath stackDotYaml) "packages: ['child']"
    --   parentDir <- getCurrentDirectory >>= parseAbsDir
    --   let childDir = "child"
    --   createDirectory childDir
    --   setCurrentDirectory childDir
    --   config <- loadConfig'
    --   configDir config `shouldBe` parentDir

    -- it "respects the STACK_YAML env variable" $ inTempDir $ do
    --   withSystemTempDirectory "config-is-here" $ \dirFilePath -> do
    --     dir <- parseAbsDir dirFilePath
    --     writeFile (toFilePath (dir </> stackDotYaml)) "packages: ['child']"
    --     withEnvVar "STACK_YAML" dirFilePath $ do
    --       config <- loadConfig'
    --       configDir config `shouldBe` dir
