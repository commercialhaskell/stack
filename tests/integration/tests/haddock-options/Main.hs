import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  removeDirIgnore ".stack-work"

  -- BAR is defined here and FOO in stack.yaml file
  stackCheckStderr ["haddock", "--haddock-arguments", "--optghc=-DBAR"] $ \s ->
    unless (errorMsg `isInfixOf` s) $
      error "FOO and BAR not both defined"
  stack ["clean"]
  -- Works just fine, test #3099 while at it.
  stack ["haddock", "--no-haddock-hyperlink-source"]
  stack ["clean"]
  -- Fails to work because we have bad argument
  stackErr ["haddock", "--haddock-arguments", "--stack_it_badhaddockargument"]

-- The error message differs by operating system
errorMsg :: String
errorMsg = if isLinux
  then "error: #error FOO and BAR is defined"
  else "error: FOO and BAR is defined"
