import StackTest
import Data.List (isInfixOf)
import System.Directory
import System.Environment (getEnv)
import System.FilePath

wrongDnsTemplateUrl :: String
wrongDnsTemplateUrl =
  "http://fake-host.fake-tld/simple.hsfiles"

main :: IO ()
main = do
  stackRoot <- getEnv "STACK_ROOT"
  run "mkdir" ["-p", templateDir stackRoot]
  copy "simple.hsfiles" (templatePath stackRoot)
  stackCheckStderr ["new", "tmp", wrongDnsTemplateUrl] $ \stderr -> do
    if ("Using cached local version" `isInfixOf` stderr)
      then return ()
      else error "stack didn't load the cached template"
    where
      templateDir :: FilePath -> FilePath
      templateDir stackRoot = foldl (</>) stackRoot
                        [ "templates"
                        , "http:"
                        , "fake-host.fake-tld"
                        ]

      templatePath :: FilePath -> FilePath
      templatePath sr = templateDir sr </> "simple.hsfiles"
