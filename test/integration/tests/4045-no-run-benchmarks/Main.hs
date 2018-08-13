import Control.Monad (unless,when)
import Data.List (isInfixOf)
import StackTest

main :: IO ()
main = do
    -- stack [defaultResolverArg, "clean"]
    stack [defaultResolverArg, "build"]
    -- given tests can be build, stack build --test
    stackOk ["test"]
    -- given bench can NOT be build, stack build --bench
    stackFail ["bench"]
    -- then running stack test --no-run-benchmarks" should fail
    stackFail ["test", "--no-run-benchmarks"]

stackOk :: [String] -> IO ()
stackOk args = stackCheckStderr args (\_ -> return ())
  
stackFail :: [String] -> IO ()
stackFail args = stackErrStderr args (\_ -> return ())
