import Control.Monad (unless, when)
import Data.List (isInfixOf)
import StackTest
import System.Directory

main :: IO ()
main = do
  copyFile "stack-2-extras" "stack.yaml"
  stack ["build"]
  lock1 <- readFile "stack.yaml.lock"
  unless ("acme-dont" `isInfixOf` lock1) $
    error "Package acme-dont wasn't found in Stack lock file"
  copyFile "stack-1-extra" "stack.yaml"
  stack ["build"]
  lock2 <- readFile "stack.yaml.lock"
  when ("acme-dont" `isInfixOf` lock2) $
    error "Package acme-dont shouldn't be in Stack lock file anymore"
