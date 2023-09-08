import StackTest
import System.Directory
import Control.Monad (unless)

main :: IO ()
main = do
  let test args = do
        removeDirIgnore "bin"
        stackCleanFull
        stack args
        exists <- doesDirectoryExist "bin"
        unless exists $ error $ "Failed with: " ++ show args
  test ["install", "--local-bin-path", "bin"]
  test ["build", "--copy-bins", "--local-bin-path", "bin"]
