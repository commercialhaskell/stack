import StackTest

import Control.Monad ( when )

main :: IO ()
main = when isWindows $ do
  stack ["exec", "--", "pacman", "-S", "--noconfirm", "mingw-w64-ucrt-x86_64-gsl"]
  stack ["build"]
  stack ["exec", "--", "foo"]
