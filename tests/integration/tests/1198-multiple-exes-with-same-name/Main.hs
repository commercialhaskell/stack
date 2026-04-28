-- Stack warns when more than one project package has an executable component of
-- the same name.
--
-- See: https://github.com/commercialhaskell/stack/issues/1198

import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  stackCheckStderr
    ["build", "myPackageA", "myPackageB"]
    (expectMessage buildMessage1)
  stackCheckStderr
    ["build", "myPackageC"]
    (expectMessage buildMessage2)

expectMessage :: String -> String -> IO ()
expectMessage msg stderr =
  unless (msg `isInfixOf` stderr)
  (error $ "Expected a warning: \n" ++ show msg)

-- Use short message fragment because prettyWarn formatting and colour
buildMessage1 = "Building several executables with the same name:"

-- Use short message fragment because prettyWarn formatting and colour
buildMessage2 = "Other executables with the same name"
