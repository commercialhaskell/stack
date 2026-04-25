-- | Stack should build only those executables requested by the program
-- arguments.
--
-- Issue: https://github.com/commercialhaskell/stack/issues/3229 is no longer
-- applicable.

module Main where

import           Control.Monad ( when )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  stackCheckStderr
    ["build", ":alpha"]
    (rejectMessage "Installing executable beta in")

rejectMessage :: String -> String -> IO ()
rejectMessage msg stderr =
    when (msg `isInfixOf` stderr)
            (error $ "Did not expect message here: \n" ++ show msg)
