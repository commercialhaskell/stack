import StackTest
import System.Directory
import System.FilePath
import System.Info (os)

main :: IO ()
main = do
  -- install in relative path
  createDirectory "bin"
  stack [defaultResolverArg, "--local-bin-path", "./bin", "install" , "happy"]
  doesExist ("./bin/happy" ++ exeExt)

  -- Default install
  -- This seems to fail due to direcory being cleaned up,
  -- a manual test of the default stack install is required
  -- defaultDir <- getAppUserDataDirectory "local"
  -- stack ["install", "happy"]
  -- doesExist (defaultDir ++ "/bin/happy" ++ exeExt)

  -- install in current dir
  stack [defaultResolverArg, "--local-bin-path", ".", "install", "happy" ]
  doesExist ("happy" ++ exeExt)

  -- install in absolute path
  tmpDirectory <- fmap (</> "absolute-bin") getCurrentDirectory
  createDirectory tmpDirectory
  stack [defaultResolverArg, "--local-bin-path", tmpDirectory, "install", "happy" ]
  doesExist (tmpDirectory </> ("happy" ++ exeExt))
