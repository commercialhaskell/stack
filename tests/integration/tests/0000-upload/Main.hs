{-# LANGUAGE NumericUnderscores #-}

import           Control.Concurrent

import           StackTest

import           System.Directory   (createDirectoryIfMissing,
                                     getCurrentDirectory)
import           System.Environment (getEnv, setEnv)
import           System.FilePath    ((</>))
import           System.Process

main :: IO ()
main =
    withFakeHackage $ do
        stackRoot <- getEnv "STACK_ROOT"
        -- Ensure there are credentials available for uploading
        createDirectoryIfMissing True (stackRoot </> "upload")
        writeFile
            (stackRoot </> "upload" </> "credentials.json")
            "{\"username\":\"fake\",\"password\":\"fake\"}"
        stack ["upload", "."]

-- | Start a fake Hackage server to test the upload
withFakeHackage :: IO a -> IO a
withFakeHackage act = do
    stackEnv <- stackExe
    -- Build the dependencies for the fake server
    stack $ withNetworkArgs ++ ["FakeHackageStart.hs"]
    -- Start the fake server
    withCreateProcess (proc stackEnv $ withNetworkArgs ++ ["FakeHackage.hs"]) $ \_ _ _ _ -> do
        -- Wait for the fake server to start accepting requests
        threadDelay 10_000_000
        act
  where
    withNetworkArgs = ["runghc", "--package", "network"]
