import StackTest
import System.Directory
import Control.Monad (unless)

main :: IO ()
main = do
  removeDirIgnore "somename"
  stack ["new", "somename", "./template.hsfiles"]
  exists <- doesFileExist "somename/somename.cabal"
  unless exists $ error "does not exist"
