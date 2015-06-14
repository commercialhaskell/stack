import Control.Exception
import Test.Hspec
import System.Process (callProcess)
import System.Directory
import System.IO.Temp

main :: IO ()
main = hspec spec

stack :: String -> [String] -> IO ()
stack x xs = callProcess "stack" (x:xs)


inTempDir :: IO () -> IO ()
inTempDir action = do
  currentDirectory <- getCurrentDirectory
  withSystemTempDirectory "stack-integration-tests" $ \tempDir -> do
    let enterDir = setCurrentDirectory tempDir
    let exitDir = setCurrentDirectory currentDirectory
    bracket_ enterDir exitDir action

inDir :: FilePath -> IO () -> IO ()
inDir fp action = do
  currentDirectory <- getCurrentDirectory
  let enterDir = setCurrentDirectory fp
  let exitDir = setCurrentDirectory currentDirectory
  bracket_ enterDir exitDir action


spec :: Spec
spec = describe "stack" $ do
  let textPkg = "text-1.2.1.1"

  describe "test" $ do
    it "tests cyclic dependencies" $ inTempDir $ do
      stack "unpack" [textPkg]
      inDir textPkg $ stack "test" []

  describe "unpack" $ do
    it "does not create a stack.yaml" $ inTempDir $ do
      stack "unpack" [textPkg]
      doesFileExist "stack.yaml" `shouldReturn` False
