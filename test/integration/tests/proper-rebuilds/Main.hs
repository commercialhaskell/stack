import Control.Monad (unless, when)
import Data.List (isInfixOf)
import StackTest
import System.Directory

main :: IO ()
main = do
    let expectRecompilation stderr =
          unless ("> build" `isInfixOf` stderr) $
          error "package recompilation was expected"
        expectNoRecompilation stderr =
          when ("> build" `isInfixOf` stderr) $
          error "package recompilation was not expected"
    copyFile "src/Lib.hs.v1" "src/Lib.hs"
    stackCheckStderr ["build"] expectRecompilation
    stackCheckStderr ["build" , "--profile"] expectRecompilation
    stackCheckStderr ["build" , "--profile"] expectNoRecompilation
    -- changing source file to trigger recompilation
    copyFile "src/Lib.hs.v2" "src/Lib.hs"
    stackCheckStderr ["build" , "--profile"] expectRecompilation
    stackCheckStderr ["build"] expectRecompilation
