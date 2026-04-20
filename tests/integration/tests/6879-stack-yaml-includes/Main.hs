import StackTest

import Control.Monad (unless)
import Data.List (isInfixOf)
import System.Directory (getCurrentDirectory)
import System.Environment (setEnv)
import System.FilePath ( (</>) )

main :: IO ()
main = do
  let
    checkFor expected actual =
      unless (expected == actual) $
        error ("expected " <> show expected <> "but got: " <> show actual)

  -- Check that includes in stack.yaml files are included
  stackCheckStdout
    ["--stack-yaml","stack-including-flags.yaml","run"]
    (checkFor "TEST_FLAG was set\n")

  stackCheckStdout
    ["--stack-yaml","stack-including-flags-with-newline.yaml","run"]
    (checkFor "TEST_FLAG was set\n")

  stackCheckStdout
    ["--stack-yaml","stack-not-including-flags.yaml","run"]
    (checkFor "TEST_FLAG was not set\n")

  -- Check that includes in config.yaml files are included
  currentDir <- getCurrentDirectory
  setEnv "STACK_CONFIG" (currentDir </> "config-including-flags.yaml")
  stackCheckStdout
    ["--stack-yaml","stack-not-including-flags.yaml","run"]
    (checkFor "TEST_FLAG was set\n")

  -- Check that 'config set' raises an error when applied to a stack.yaml file
  -- that uses !include directives
  stackErrStderr
    ["--stack-yaml","stack-including-flags.yaml","config","set","snapshot","ghc-9.8.4"]
    (expectMessage "!include")

  -- Check that 'config set' raises an error when applied to a stack.yaml file
  -- that uses !include directives
  stackErrStderr
    ["--stack-yaml","stack-including-flags-with-newline.yaml","config","set","snapshot","ghc-9.8.4"]
    (expectMessage "!include")

expectMessage :: String -> String -> IO ()
expectMessage msg stderr' = do
  unless (msg `isInfixOf` stderr')
         (error $ "Expected stderr to contain " ++ show msg ++ " but got:\n" ++ stderr')
