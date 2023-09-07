import Control.Monad (forM_, unless, when)
import Data.List (isInfixOf, stripPrefix)
import StackTest

main :: IO ()
main = unless isWindows $ do -- dependency issues on Windows
    let isBuild package line =
          case stripPrefix package line of
            Just x -> "> build" `isInfixOf` line
            Nothing -> False
        expectRecompilation pkgs stderr = forM_ pkgs $ \p ->
          unless (any (isBuild p) $ lines stderr) $
          error $ "package " ++ show p ++ " recompilation was expected"
        expectNoRecompilation pkgs stderr = forM_ pkgs $ \p ->
          when (any (isBuild p) $ lines stderr) $
          error $ "package " ++ show p ++ " recompilation was not expected"
        mutablePackages = [ "filepath"
                          , "directory"
                          , "filemanip"
                          , "files"
                          ]
    stackCheckStderr ["build"] $ expectRecompilation mutablePackages
    stackCheckStderr ["build" , "--profile"] $ expectRecompilation mutablePackages
    stackCheckStderr ["build"] $ expectNoRecompilation mutablePackages
    stackCheckStderr ["build" , "--profile"] $ expectNoRecompilation mutablePackages
