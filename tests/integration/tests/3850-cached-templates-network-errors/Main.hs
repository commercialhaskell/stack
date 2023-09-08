import StackTest
import Control.Monad (when, unless)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import System.Directory
import System.Environment (lookupEnv, setEnv)
import System.FilePath

main :: IO ()
main = when isLinux $ do
  performCachingTest templateUrl
  performCachingTest githubTemplate
  where
    performCachingTest :: String -> IO ()
    performCachingTest template = do
      let arguments = ["new", "tmp", template]
      originalHttpProxy <- lookupEnv "HTTPS_PROXY"
      stack arguments
      removeDirectoryRecursive "tmp"
      setEnv "HTTPS_PROXY" "http://sdsgsfgslfgsjflgkjs" -- make https requests fail
      stackCheckStderr arguments $ \stderr ->
        unless ("Using cached local version" `isInfixOf` stderr)
        (error "stack didn't load the cached template")

      removeDirectoryRecursive "tmp"
      setEnv "HTTPS_PROXY" (fromMaybe "" originalHttpProxy)

    -- this template has a `stack.yaml` file
    -- so `stack new` does not have to `stack init`
    -- and therefore the test runs faster
    templateUrl :: String
    templateUrl =
      "https://raw.githubusercontent.com/commercialhaskell/stack-templates/986836cc85b0c8c5bbb78d7b94347ba095089b03/tasty-discover.hsfiles"

    -- the same template, cached differently
    githubTemplate :: String
    githubTemplate = "github:commercialhaskell/tasty-discover.hsfiles"

