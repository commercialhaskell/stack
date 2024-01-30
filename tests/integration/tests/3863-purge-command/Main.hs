{-# LANGUAGE ViewPatterns #-}

import StackTest
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Monad (unless)
import System.Directory
import System.FilePath

trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace

main :: IO ()
main =
  -- For these commands, we'll need to know the `dist` directory.
  -- This is usually `.stack-work/dist/$compiler-variant/Cabal-xxxx`
  stackCheckStdout [defaultSnapshotArg, "path", "--dist-dir"] $ \(trimEnd -> distDir) -> do
    stackCheckStdout [defaultSnapshotArg, "path", "--local-install-root"] $ \(trimEnd -> localInstallRoot) -> do
      -- Usually `.stack-work`
      let stackWork = fromMaybe (error "There must be a Stack working directory.") $
            listToMaybe (splitDirectories distDir)

      -- First, clean the .stack-work directory.
      -- This is only necessary when running individual tests.
      stackIgnoreException [defaultSnapshotArg, "purge"]
      -- See #4936 for details regarding the windows condition
      unless isWindows $ doesNotExist stackWork

      -- The dist directory should exist after a build
      stack [defaultSnapshotArg, "build"]
      doesExist distDir
      doesExist localInstallRoot
      doesExist stackWork

      -- The dist directory should not exist after a clean, whereas the
      -- .stack-work directory should
      stackIgnoreException [defaultSnapshotArg, "clean"]
      -- See #4936 for details regarding the windows condition
      unless isWindows $ do
        doesNotExist distDir
        doesExist localInstallRoot
        doesExist stackWork

      -- The .stack-work directory should not exist after a purge
      stackIgnoreException [defaultSnapshotArg, "purge"]
      -- See #4936 for details regarding the windows condition
      unless isWindows $ doesNotExist stackWork
