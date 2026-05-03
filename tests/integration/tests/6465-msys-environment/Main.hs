-- | On Windows, Stack supports different MSYS2 environments.
--
-- See: https://github.com/commercialhaskell/stack/issues/6465

import           Control.Monad ( when )
import           StackTest

main :: IO ()
main = when isWindows $ do
  stack ["exec", "--", "pacman", "-S", "--noconfirm", "mingw-w64-ucrt-x86_64-gsl"]
  stack ["build"]
  stack ["exec", "--", "myExe"]
