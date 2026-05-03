import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  -- VARIABLE_B is defined here and VARIABLE_A in stack.yaml file
  stackCheckStderr ["haddock", "--haddock-arguments", "--optghc=-DVARIABLE_B"] $ \s ->
    unless (errorMsg `isInfixOf` s) $
      error "VARIABLE_A and VARIABLE_B not both defined"
  stack ["clean"]
  -- Works just fine, test #3099 while at it.
  stack ["haddock", "--no-haddock-hyperlink-source"]
  stack ["clean"]
  -- Fails to work because we have bad argument
  stackErr ["haddock", "--haddock-arguments", "--stack_it_badhaddockargument"]

-- The error message differs by operating system
errorMsg :: String
errorMsg = if isLinux
  then "error: #error VARIABLE_A and VARIABLE_B is defined"
  else "error: VARIABLE_A and VARIABLE_B is defined"
