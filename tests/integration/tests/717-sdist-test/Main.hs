-- Stack's test of an archive file produced by sdist should fail if the package
-- description did not list all files used by the package itself.
--
-- https://github.com/commercialhaskell/stack/issues/717

import           StackTest

main :: IO ()
main = do
  -- Verify building works:
  stack ["build"]
  -- Keep old behavior:
  stack ["sdist"]
  -- Successful sdist with --test-tarball:
  stack ["sdist", "working-package-with-th", "--test-tarball"]
  -- Fails because package contains TH which depends on files which are not put
  -- into sdist tarball:
  stackErr ["sdist", "failing-package-with-th", "--test-tarball"]
  -- Same, but inside a subdir:
  stackErr ["sdist", "subdirs/failing-in-subdir", "--test-tarball"]
  -- Depends on failing-package-with-th and failing-in-subdir - these would fail
  -- if they were the target of sdist, but since they are just dependencies, the
  -- operation should succeed:
  stack ["sdist", "subdirs/dependent-on-failing-packages", "--test-tarball"]
  -- Fails because a test depends on files which are not put into sdist tarball:
  stackErr ["sdist", "failing-package-with-test", "--test-tarball"]
