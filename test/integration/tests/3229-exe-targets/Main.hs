-- | Stack should build all executables once, and in subsequent
-- invocations only build those executables requested by the program
-- arguments.
--
-- Issue: https://github.com/commercialhaskell/stack/issues/3229

module Main where

import           Control.Exception
import           Control.Monad (unless, when)
import qualified Data.ByteString as S
import           Data.List (isInfixOf)
import           StackTest

main :: IO ()
main = do
    removeDirIgnore ".stack-work"
    removeFileIgnore "stack.yaml"
    stack [defaultResolverArg, "init"]
    stack ["build", ":alpha"]
    bracket
        (S.readFile alphaFile)
        (S.writeFile alphaFile)
        (const
             (do appendFile alphaFile "\n--"
                 stackCheckStderr
                     ["build", ":alpha"]
                     (rejectMessage
                          (unlines
                               ["Preprocessing executable 'beta' for foo-0..."]))))
  where
    alphaFile = "app/Alpha.hs"

expectMessage :: String -> String -> IO ()
expectMessage msg stderr =
    unless (msg `isInfixOf` stderr)
        (error $ "Expected in output: \n" ++ show msg)

rejectMessage :: String -> String -> IO ()
rejectMessage msg stderr =
    when (msg `isInfixOf` stderr)
            (error $ "Did not expect message here: \n" ++ show msg)
