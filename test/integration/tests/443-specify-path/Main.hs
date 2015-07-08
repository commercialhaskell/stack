import StackTest
import System.Directory

main :: IO ()
main = do
  -- install in relative path
  createDirectory "bin"
  stack ["-p", "./bin", "install" , "happy"]
  doesExist "./bin/happy"

  -- Default install
  -- This seems to fail due to direcory being cleaned up,
  -- a manual test of the default stack install is required
  -- defaultDir <- getAppUserDataDirectory "local"
  -- stack ["install", "happy"]
  -- doesExist (defaultDir ++ "/bin/happy")

  -- install in current dir
  stack ["-p", ".", "install", "happy" ]
  doesExist "happy"

  -- install in absolute path
  tmpDirectory <- getTemporaryDirectory
  stack ["-p", tmpDirectory, "install", "happy" ]
  doesExist "/tmp/happy"
