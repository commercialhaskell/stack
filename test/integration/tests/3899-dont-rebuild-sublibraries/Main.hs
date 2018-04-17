import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

main :: IO ()
main = do
  stack ["clean"]
  stack ["build"]
  res <- compilingModulesLines . snd <$> stackStderr ["build"]
  unless (null res) $ fail "Stack recompiled code"

-- Returns the lines where a module is compiled
compilingModulesLines :: String -> [String]
compilingModulesLines = filter (isInfixOf " Compiling ") . lines
