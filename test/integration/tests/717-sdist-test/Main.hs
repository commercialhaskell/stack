import StackTest

main :: IO ()
main = do
  -- verify building works
  stack ["build"]
  -- keep old behavior
  stack ["sdist"]
  -- successful sdist with --test-tarball
  stack ["sdist", "packageb", "--test-tarball"]
  -- fails because package contains TH which depends on files which are not put into sdist tarball
  stackErr ["sdist", "packagea", "--test-tarball"]
  -- same, but inside a subdir
  stackErr ["sdist", "packagecd/packagec", "--test-tarball"]
  -- depends on packagea and packagec - these would fail if they were the target of sdist,
  -- but since they are just dependencies, the operation should succeed
  stack ["sdist", "packagecd/packaged", "--test-tarball"]
  -- fails because a test depends on files which are not put into sdist tarball
  stackErr ["sdist", "packagee", "--test-tarball"]
