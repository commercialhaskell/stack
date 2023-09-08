import StackTest
import Control.Monad
import Data.List
import System.Directory

main :: IO ()
main = do
  putStrLn "Test disabled due to switch to pantry"
  {-
  copyFile "bad-stack.yaml" "stack.yaml"
  stackErrStderr ["build", "--dry-run"] $ \msg ->
    unless ("legacy 00-index.tar.gz" `isInfixOf` msg) $
      error "Expected a warning about 00-index usage"
  copyFile "good-stack.yaml" "stack.yaml"
  stack ["build", "--dry-run"]
  -}
