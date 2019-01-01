import StackTest

main :: IO ()
main = do
  if isWindows
     then logInfo "Disabled on Windows as Nix is not currently supported on Windows."
     else do
       stack ["build", "--nix-pure"]
       stack ["exec", "--nix-pure", "ShowUnicode"]
