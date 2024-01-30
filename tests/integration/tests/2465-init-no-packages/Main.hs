import StackTest
import System.Directory
import Control.Monad (unless)

main :: IO ()
main = do
  removeFileIgnore "stack.yaml"
  stack ["init", "--snapshot", "ghc-9.2.4"]
  exists <- doesFileExist "stack.yaml"
  unless exists $ error "stack.yaml not created!"
