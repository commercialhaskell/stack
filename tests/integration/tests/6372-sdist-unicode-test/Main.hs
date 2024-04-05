import StackTest

import Control.Monad (unless)

-- | The test fails at runtime on the Windows Server 2022 GitHub-hosted runner
-- only, at the point of outputting a Unicode character, with:
--
-- <stderr>: commitAndReleaseBuffer: invalid argument (cannot encode character '\1633')
--
-- That appears to be similar to
-- https://gitlab.haskell.org/ghc/ghc/-/issues/8118, however: (1) the locale is
-- set to C.UTF-8 and the active code page is 65001; and
-- (2) `GHC.IO.Encoding.setLocaleEncoding utf8` has no effect.
--
-- Until the origin of the problem is identified, we disable the test on
-- Windows.

main :: IO ()
main = unless isWindows $ do
  stack ["clean"]
  stack ["build", "--dry-run"]
  stack ["sdist", "."]
