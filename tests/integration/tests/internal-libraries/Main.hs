import StackTest
import Control.Monad ( unless )

-- This does not work on Windows with snapshot: ghc-9.6.6 (Cabal-3.10.3.0). It
-- fails witb:
--
-- Preprocessing foreign library 'baz' for files-0.1.0.0..
-- Building foreign library 'baz' for files-0.1.0.0..
-- [1 of 1] Compiling Baz
-- [1 of 2] Compiling Baz [Flags changed]
-- [2 of 2] Linking .stack-work\dist\eebe39f7\build\baz\baz.dll
-- lld: error: unknown argument: -rpath
-- lld: error: unknown argument: -rpath
-- clang: error: linker command failed with exit code 1 (use -v to see invocation)
-- ghc-9.6.6.exe: `clang.exe' failed in phase `Linker'. (Exit code: 1)
--
-- The above is a regression from snapshot: ghc-9.6.4 (Cabal-3.10.1.0). See
-- https://github.com/haskell/cabal/issues/9982.
main :: IO ()
main = unless isWindows $ stack ["build"]
