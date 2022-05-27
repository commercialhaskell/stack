import StackTest

main :: IO ()
main
  | isWindows = logInfo "Disabled on Windows as Nix is not currently supported on Windows."
  | isMacOSX = logInfo "Takes too long to run, since it tries to build GHC"
  | otherwise = do
       logInfo "This isn't even working on Linux anymore, disabling for now"
       -- stack ["build", "--nix-pure"]
       -- stack ["exec", "--nix-pure", "ShowUnicode"]
