import StackTest

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
       stack ["build", "--nix-pure"]
       stack ["exec", "--nix-pure", "ShowUnicode"]
