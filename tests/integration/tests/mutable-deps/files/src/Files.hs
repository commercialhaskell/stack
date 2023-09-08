module Files where

import System.FilePath.Glob

allCFiles :: IO [FilePath]
allCFiles = namesMatching "*.c"
