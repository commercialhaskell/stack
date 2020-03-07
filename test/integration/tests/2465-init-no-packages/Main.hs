import StackTest
import System.Directory
import Control.Monad (unless)

main :: IO ()
main = do
  removeFileIgnore "stack.yaml"
  stack ["init", "--resolver", "ghc-8.6.5"]
  exists <- doesFileExist "stack.yaml"
  unless exists $ error "stack.yaml not created!"
