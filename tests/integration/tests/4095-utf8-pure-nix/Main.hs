import StackTest

import Control.Monad ( unless )
import Data.Maybe ( isJust )
import System.Environment ( lookupEnv )

-- This test requires that Nix is installed and that the NIX_PATH has been set
-- so as to allow the path <nixpkgs> to be used.
main :: IO ()
main
  | isWindows =
      logInfo "Disabled on Windows as Nix is not currently supported on \
              \Windows."
  | isMacOSX =
      logInfo "Disabled on macOS as it takes too long to run, since it tries \
              \to build GHC."
  | otherwise = do
      isInContainer <- getInContainer
      unless isInContainer $ do
         stack ["build", "--nix-pure"]
         stack ["exec", "--nix-pure", "ShowUnicode"]

-- | 'True' if we are currently running inside a Docker container.
getInContainer :: IO Bool
getInContainer = isJust <$> lookupEnv "STACK_IN_CONTAINER"
