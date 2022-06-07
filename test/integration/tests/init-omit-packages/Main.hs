import Control.Monad (unless)
import StackTest
import System.IO (readFile)

main :: IO ()
main = do
  removeFileIgnore "stack.yaml"
  stackErr ["init", "--resolver", "nightly-2022-06-06"]
  stack ["init", "--resolver", "nightly-2022-06-06", "--omit-packages"]
  contents <- lines <$> readFile "stack.yaml"
  unless ("#- bad" `elem` contents) $
    error "commented out 'bad' package was expected"
