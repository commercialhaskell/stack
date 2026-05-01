-- Stack reports an error if a package component depends on a package that has
-- no library component.
--

import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = stackErrStderr
         ["build", "myPackageB"]
         (expectMessage "package provides no library")

expectMessage :: String -> String -> IO ()
expectMessage msg stderr =
  unless (msg `isInfixOf` stderr)
    (error $ "Expected a warning: \n" ++ show msg)
