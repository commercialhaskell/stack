import Control.Monad (forM_, unless, when)
import Data.List (isInfixOf)
import StackTest

main :: IO ()
main = unless isWindows $ do -- depedency issues on Windows
    let expectRecompilation pkgs stderr = forM_ pkgs $ \p ->
          unless ((p ++ ": build") `isInfixOf` stderr) $
          error $ "package " ++ show p ++ " recompilation was expected"
        expectNoRecompilation pkgs stderr = forM_ pkgs $ \p ->
          when ((p ++ ": build") `isInfixOf` stderr) $
          error $ "package " ++ show p ++ " recompilation was not expected"
        mutablePackages = [ "filepath-1.4.1.2"
                          , "directory-1.3.0.2"
                          , "filemanip-0.3.6.3"
                          , "files-1.0.0"
                          ]
    stackCheckStderr ["build"] $ expectRecompilation mutablePackages
    stackCheckStderr ["build" , "--profile"] $ expectRecompilation mutablePackages
    stackCheckStderr ["build"] $ expectNoRecompilation mutablePackages
    stackCheckStderr ["build" , "--profile"] $ expectNoRecompilation mutablePackages
