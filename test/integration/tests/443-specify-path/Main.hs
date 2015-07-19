import StackTest
import System.Directory
import System.FilePath

main :: IO ()
main = do
  -- install in relative path
  createDirectory "bin"
  stack ["--local-bin-path", "./bin", "install" , "happy"]
  doesExist "./bin/happy"

  -- Default install
  -- This seems to fail due to direcory being cleaned up,
  -- a manual test of the default stack install is required
  -- defaultDir <- getAppUserDataDirectory "local"
  -- stack ["install", "happy"]
  -- doesExist (defaultDir ++ "/bin/happy")

  -- install in current dir
  stack ["--local-bin-path", ".", "install", "happy" ]
  doesExist "happy"

  -- install in absolute path
  tmpDirectory <- fmap (</> "absolute-bin") getCurrentDirectory
  createDirectory tmpDirectory
  stack ["--local-bin-path", tmpDirectory, "install", "happy" ]
  doesExist (tmpDirectory </> "happy")
