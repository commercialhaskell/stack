import StackTest
import System.Directory

main :: IO ()
main = do
  -- Custom Path
  createDirectory "bin"
  stack ["install", "--path", "./bin", "hindent"]
  doesExist "./bin/hindent"

  -- Default install
  defaultDir <- getAppUserDataDirectory "local"
  stack ["install", "hindent"]
  doesExist (defaultDir ++ "/bin/hindent")

  -- install in current dir
  stack ["install", "hindent", "--path", "." ]
  doesExist "hindent"

  -- install in absolute path
  tmpDirectory <- getTemporaryDirectory
  stack ["install", "hindent", "--path", tmpDirectory ]
  doesExist "/tmp/hindent"
