import StackTest

main :: IO ()
main = do
  -- verify building works
  stack ["build"]
  -- keep old behavior
  stack ["sdist"]
  -- successful sdist with --build-package
  stack ["sdist", "packageb", "--build-package"]
  -- fails because package contains TH which depends on files which are not put into sdist tarball
  stackErr ["sdist", "packagea", "--build-package"]
  -- same, but inside a subdir
  stackErr ["sdist", "packagecd/packagec", "--build-package"]
  -- depends on packagea and packagec - these would fail if they were the target of sdist,
  -- but since they are just dependencies, the operation should succeed
  stack ["sdist", "packagecd/packaged", "--build-package"]
  -- fails because a test depends on files which are not put into sdist tarball
  stackErr ["sdist", "packagee", "--build-package"]
