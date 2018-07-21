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
        disableGpg
        stackRoot <- getEnv "STACK_ROOT"
        -- Ensure there are credentials available for uploading
        createDirectoryIfMissing True (stackRoot </> "upload")
        writeFile
            (stackRoot </> "upload" </> "credentials.json")
            "{\"username\":\"fake\",\"password\":\"fake\"}"
        -- Test the upload with no signing
        stack ["upload", "--no-signature", "."]
        -- Test failure signing
        stackErr ["upload", "."]

-- | Ensure gpg is unusable by putting a broken one on PATH
disableGpg :: IO ()
disableGpg = do
    originalPath <- getEnv "PATH"
    cwd <- getCurrentDirectory
    setEnv "PATH" $ (cwd </> "gpg-disabled") ++ ":" ++ originalPath

-- | Start a fake Hackage server to test the upload
withFakeHackage :: IO a -> IO a
withFakeHackage act = do
    stackEnv <- stackExe
    -- Build the dependencies for the fake server
    stack $ withNetworkArgs ++ ["FakeHackageStart.hs"]
    -- Start the fake server
    withCreateProcess (proc stackEnv $ withNetworkArgs ++ ["FakeHackage.hs"]) $ \_ _ _ _ -> do
        -- Wait for the fake server to start accepting requests
        threadDelay 2000000
        act
  where
    withNetworkArgs = ["runghc", "--package", "network-simple"]
