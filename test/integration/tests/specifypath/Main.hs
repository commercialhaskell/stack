import StackTest
import System.Directory

main :: IO ()
main = do
  -- install in relative path
  createDirectory "bin"
  stack ["install", "--path", "./bin", "happy"]
  doesExist "./bin/happy"

  -- Default install
  -- This seems to fail due to direcory being cleaned up, 
  -- a manual test of the default stack install is required
  -- defaultDir <- getAppUserDataDirectory "local"
  -- stack ["install", "happy"]
  -- doesExist (defaultDir ++ "/bin/happy")

-- install in current dir
stack ["install", "happy", "--path", "." ]
doesExist "happy"

-- install in absolute path
tmpDirectory <- getTemporaryDirectory
stack ["install", "happy", "--path", tmpDirectory ]
doesExist "/tmp/happy"
