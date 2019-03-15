import StackTest

main :: IO ()
main = do

  stack [defaultResolverArg, "clean", "--full"]

  -- Nothing should be built here, so stack should succeed
  builds targetLib [skipLib]
  builds targetExe [skipLib, skipExe]
  builds targetTest [skipLib, skipTest]

  -- stack should try to build the library but fail due to missing library flag
  failsToBuild targetLib []

  -- stack should try to build the library and succeed
  builds targetLib [buildLib]

  -- stack should try to build the executable and fail due to skipped library
  -- TODO Or should it? The library has already been build. Perhaps it should succeed?
  failsToBuild targetExe [buildLib, skipLib]

  -- stack should try to build the executable and fail due to missing executable flag
  failsToBuild targetExe [buildLib]

  -- stack should try to build the executable and succeed
  builds targetExe [buildLib, buildExe]

  -- stack should try to build the tests and fail due to skipped library
  -- TODO Or should it? The library has already been build. Perhaps it should succeed?
  failsToBuild targetTest [buildLib, skipLib]

  -- stack should try to build the tests and fail due to missing tests flag
  failsToBuild targetTest [buildLib]

  -- stack should try to build the testcutable and succeed
  builds targetTest [buildLib, buildTest]

  stack [defaultResolverArg, "clean", "--full"]

withArgs :: ([String] -> IO()) -> String -> [[String]] -> IO()
withArgs stackFunction target argsList = stackFunction . mconcat $ [defaultResolverArg, "build"] : argsList <> [[target]]

builds, failsToBuild :: String -> [[String]] -> IO()
builds = withArgs stack
failsToBuild = withArgs stackErr

skipLib, skipExe, skipTest, buildLib, buildExe, buildTest :: [String]
skipLib   = ["--skip", "this:lib"]
skipExe   = ["--skip", "this:test"]
skipTest  = ["--skip", "this:exe"]
buildLib  = ["--flag", "this:library"]
buildExe  = ["--flag", "this:executable"]
buildTest = ["--flag", "this:tests"]

targetLib, targetExe, targetTest :: String
targetLib  = "this:lib"
targetExe  = "this:exe:this-exe"
targetTest = "this:test:this-test"
