import StackTest
import System.Directory
import Control.Monad

main :: IO ()
main = do
  stack ["test"]
  exists1 <- doesFileExist "foo"
  unless exists1 $ error "exists1 should be True"
  removeFile "foo"
  stack ["test", "--no-rerun-tests"]
  exists2 <- doesFileExist "foo"
  when exists2 $ error "exists2 should be False"
