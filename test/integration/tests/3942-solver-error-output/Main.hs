import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

planFailure :: String
planFailure =
  "While constructing the build plan, the following exceptions were encountered:"

main :: IO ()
main = do
  stackErrStderr ["./script.hs"] (expectMessage planFailure)

expectMessage :: String -> String -> IO ()
expectMessage msg stderr = do
  unless (words msg `isInfixOf` words stderr)
         (error $ "Expected a warning: \n" ++ show msg)
