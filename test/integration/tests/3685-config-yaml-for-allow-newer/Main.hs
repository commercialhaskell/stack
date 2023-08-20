import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest
import System.Directory

planRecommendation :: String
planRecommendation = "To ignore all version constraints"

main :: IO ()
main = do
  removeFileIgnore "stack.yaml"
  stack ["init", defaultResolverArg]
  stackErrStderr ["install", "intero-0.1.23"] (expectMessage planRecommendation)

expectMessage :: String -> String -> IO ()
expectMessage msg stderr = do
  unless (words msg `isInfixOf` words stderr)
         (error $ "Expected a recommendation: \n" ++ show msg)
