import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest
import System.Directory

main :: IO ()
main = do
  removeFileIgnore "stack.yaml"
  stack ["init", defaultResolverArg]
  (_, stdErr) <- stackStderr ["install", "intero-0.1.23"]
  -- here we check stderr for 'allow-newer: true' and
  -- config.yaml sitting either on the same line or on
  -- two consecutive lines
  let errLines = lines stdErr
      hasNewer l = "allow-newer: true" `isInfixOf` l
      withNewer = map hasNewer errLines
      userConfig = "config.yaml"
      hasConfigForAllowNewer prevNewer l =
         (prevNewer || hasNewer l) &&
        userConfig `isInfixOf` l
      hasProperLines =
        or $ zipWith hasConfigForAllowNewer (False:withNewer) errLines
  unless hasProperLines $
    error $ "Not stderr lines with 'allow-newer: true' and " ++ userConfig
