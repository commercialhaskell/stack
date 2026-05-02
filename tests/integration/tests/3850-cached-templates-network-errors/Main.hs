-- Stack can used cached Stack project templates.
--
-- See: https://github.com/commercialhaskell/stack/issues/3850

import           Control.Exception ( bracket )
import           Control.Monad ( unless, when )
import           Data.List ( isInfixOf )
import           Data.Maybe ( fromMaybe )
import           StackTest
import           System.Directory ( removeDirectoryRecursive )
import           System.Environment ( lookupEnv, setEnv )

main :: IO ()
main = when isLinux $ do
  performCachingTest "myProjectA" "myProjectB" templateUrl
  performCachingTest "myProjectC" "myProjectD" githubTemplate
 where
  performCachingTest :: String -> String -> String -> IO ()
  performCachingTest projectName1 projectName2 template = do
    let arguments = ["new", projectName1, template]
    bracket
      ( lookupEnv "HTTPS_PROXY" )
      ( (setEnv "HTTPS_PROXY") . (fromMaybe "") )
      ( const $ do
          stack ["new", projectName1, template]
          setEnv "HTTPS_PROXY" "http://sdsgsfgslfgsjflgkjs" -- make https requests fail
          stackCheckStderr ["new", projectName2, template] $ \stderr ->
            unless ("Using cached local version." `isInfixOf` stderr) $
              error "stack didn't load the cached template"
      )

  -- This template has a `stack.yaml` file so `stack new` does not have to
  -- `stack init` and therefore the test runs faster
  templateUrl :: String
  templateUrl =
    "https://raw.githubusercontent.com/commercialhaskell/stack-templates/refs/heads/master/tasty-discover.hsfiles"

  -- The same template, cached differently
  githubTemplate :: String
  githubTemplate = "github:commercialhaskell/tasty-discover.hsfiles"
