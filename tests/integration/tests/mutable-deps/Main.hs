import Control.Monad (forM_, unless, when)
import Data.List (isInfixOf, stripPrefix)
import StackTest

-- The package 'files' depends directly on filemanip, which depends directly on
-- packages directory, filepath and unix-compat. Package directory also depends
-- directly on filepath. Package unix-compat depends directly on directory,
-- filepath, time, unix and Win32.
-- The stack.yaml file, however, identifies filepath-1.4.100.4 as a local
-- package. Consequently, filepath is a mutable package and the packages
-- that depend on it should also be treated as mutable packages.

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
