import StackTest
import Control.Monad (unless)
import Data.List (isInfixOf)
import System.Directory
import System.Environment (getEnv, setEnv)
import System.FilePath

main :: IO ()
main = do
  stack ["new", "tmp", templateUrl]
  removeDirectoryRecursive "tmp"
  setEnv "HTTPS_PROXY" "http://adfafdadfadf" -- make https requests fail
  stackCheckStderr ["new", "tmp", templateUrl] $ \stderr -> 
     unless ("Using cached local version" `isInfixOf` stderr) 
     (error "stack didn't load the cached template")

  where
    -- this templates has a `stack.yaml` file
    -- so `stack new` does not have to `stack init`
    -- and therefore the test runs faster
    templateUrl :: String
    templateUrl =
      "https://raw.githubusercontent.com/commercialhaskell/stack-templates/986836cc85b0c8c5bbb78d7b94347ba095089b03/tasty-discover.hsfiles"

