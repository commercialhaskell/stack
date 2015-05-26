module Stack.ConfigSpec where

import Control.Monad.Logger
import Control.Exception
import System.Directory
import System.IO.Temp
import Test.Hspec

import Stack.Config
import Stack.Types.StackT

spec :: Spec
spec = do
  manager <- runIO $ newTLSManager
  let logLevel = LevelDebug
  let inTempDir act = do
        currentDirectory <- getCurrentDirectory
        withSystemTempDirectory "Stack_ConfigSpec" $ \tempDir -> do
          let enterDir = setCurrentDirectory tempDir
          let exitDir = setCurrentDirectory currentDirectory
          bracket_ enterDir exitDir act


  describe "loadConfig" $ do
    let loadConfig' = runStackLoggingT manager logLevel loadConfig

    -- TODO: make sure parent dirs also don't have config file
    it "works even if no config file exists" $ inTempDir $ do
      _config <- loadConfig'
      return ()

    -- TODO: should throw?
    it "works with a blank config file" $ inTempDir $ do
      writeFile "stack.yaml" ""
      _config <- loadConfig'
      return ()
