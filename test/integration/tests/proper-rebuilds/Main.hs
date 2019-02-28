import Control.Monad (unless, when)
import Data.List (isInfixOf)
import StackTest
import System.Directory

main :: IO ()
main = do
    let expectRecompilation stderr =
          unless ("files-1.0.0: build" `isInfixOf` stderr) $
          error "package recompilation was expected"
        expectNoRecompilation stderr =
          when ("files-1.0.0: build" `isInfixOf` stderr) $
          error "package recompilation was not expected"
    stackCheckStderr ["build"] expectRecompilation
    stackCheckStderr ["build" , "--profile"] expectRecompilation
    stackCheckStderr ["build" , "--profile"] expectNoRecompilation
    -- changing source file to trigger recompilation
    copyFile "src/Lib.hs.v2" "src/Lib.hs"
    stackCheckStderr ["build" , "--profile"] expectRecompilation
    stackCheckStderr ["build"] expectRecompilation
