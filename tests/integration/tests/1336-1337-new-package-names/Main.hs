-- Stack's new command accepts project names that are valid Cabal package names
-- and rejects those that are not without creating a project directory.
--
-- See: https://github.com/commercialhaskell/stack/issues/1336
--      https://github.com/commercialhaskell/stack/issues/1337

import           Control.Monad ( unless, when )
import           StackTest
import           System.Directory
                   ( doesDirectoryExist, removeDirectoryRecursive )

main :: IO ()
main = do
  safeNew "1b3d-a2c4"
  doesExist "./1b3d-a2c4/stack.yaml"
  doesExist "./1b3d-a2c4/1b3d-a2c4.cabal"
  stackErr ["new", "1234-abcd"]
  doesNotExist "./1234-abcd"
  stackErr ["new", "abcd-1234"]
  -- The GitHub windows-latest (Microsoft Windows Server 2025) environment
  -- appears to be unable to handle these Unicode code points.
  unless isWindows $ do
    stackErr ["new", "1234-ば日本-4本"]
    safeNew "ば日本-4本"
    safeNew "אבהץש"
    safeNew "ΔΘΩϬ"

safeNew :: String -> IO ()
safeNew name = do
  exists <- doesDirectoryExist name
  when exists $ removeDirectoryRecursive name
  stack ["new", name]
