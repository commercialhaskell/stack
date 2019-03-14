import StackTest
import Data.Maybe (listToMaybe, fromMaybe)
import System.Directory
import System.FilePath
import Data.List (filter)

newline :: Char
newline = '\n'

main :: IO ()
main =
  -- For these commands, we'll need to know the `dist` directory.
  -- This is usually `.stack-work/dist/$compiler-variant/Cabal-xxxx`
  stackCheckStdout [defaultResolverArg, "path", "--dist-dir"] $ \distDir' -> do
    let distDir = filter (\x -> x /= newline) distDir'
    stackCheckStdout [defaultResolverArg, "path", "--local-install-root"] $ \localInstallRoot' -> do
      let localInstallRoot = filter (\x -> x /= newline) localInstallRoot'
      -- Usually `.stack-work`
      let stackWork = fromMaybe (error "There must be a stack working directory.") $
            listToMaybe (splitDirectories distDir)

      -- First, clean the .stack-work directory.
      -- This is only necessary when running individual tests.
      stack [defaultResolverArg, "purge"]
      doesNotExist stackWork
      -- The dist directory should exist after a build
      stack [defaultResolverArg, "build"]
      doesExist distDir
      doesExist localInstallRoot
      doesExist stackWork

      -- The dist directory should not exist after a clean, whereas the
      -- .stack-work directory should
      stack [defaultResolverArg, "clean"]
      doesNotExist distDir
      doesExist localInstallRoot
      doesExist stackWork

      -- The .stack-work directory should not exist after a purge
      stack [defaultResolverArg, "purge"]
      doesNotExist stackWork
