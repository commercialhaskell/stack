import StackTest
import Control.Monad
import Data.List

main :: IO ()
main = do
    stack ["build", "--stack-yaml", "./stack.yaml"]
    stackCheckStderr ["build", "--stack-yaml", "./stack.yaml"] $ \out ->
      unless ("precompiled" `isInfixOf` out) $ error "Didn't use precompiled!"