import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

-- | Stack's error code for failing to construct a build plan.
planFailure :: String
planFailure =
  "[S-4804]"

main :: IO ()
main = do
  stackErrStderr ["./script.hs"] (expectMessage planFailure)

expectMessage :: String -> String -> IO ()
expectMessage msg stderr = do
  unless (words msg `isInfixOf` words stderr)
         (error $ "Expected a warning: \n" ++ show msg)
