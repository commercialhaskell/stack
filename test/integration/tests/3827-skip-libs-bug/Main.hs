import StackTest

main :: IO ()
main = do

  stack [defaultResolverArg, "clean", "--full"]

  -- Nothing should be built here, so stack should succeed
  stack [defaultResolverArg, "build", "--skip", "this:lib", "this:lib"]
  stack [defaultResolverArg, "build", "--skip", "this:lib", "--skip", "this:exe", "this:exe:this-exe"]
  stack [defaultResolverArg, "build", "--skip", "this:lib", "--skip", "this:test", "this:test:this-test"]

  -- stack should try to build the library but fail due to missing library flag
  stackErr [defaultResolverArg, "build", "this:lib"]

  -- stack should try to build the library and succeed
  stack [defaultResolverArg, "build", "--flag", "this:library", "this:lib"]

  -- stack should try to build the executable and fail due to skipped library
  -- TODO Or should it? The library has already been build. Perhaps it should succeed?
  stackErr [defaultResolverArg, "build", "--flag", "this:library", "--skip", "this:lib", "this:exe:this-exe"]

  -- stack should try to build the executable and fail due to missing executable flag
  stackErr [defaultResolverArg, "build", "--flag", "this:library", "this:exe:this-exe"]

  -- stack should try to build the executable and succeed
  stack [defaultResolverArg, "build", "--flag", "this:library", "--flag", "this:executable", "this:exe:this-exe"]

  -- stack should try to build the tests and fail due to skipped library
  -- TODO Or should it? The library has already been build. Perhaps it should succeed?
  stackErr [defaultResolverArg, "build", "--flag", "this:library", "--skip", "this:lib", "this:test:this-test"]

  -- stack should try to build the tests and fail due to missing tests flag
  stackErr [defaultResolverArg, "build", "--flag", "this:library", "this:test:this-test"]

  -- stack should try to build the testcutable and succeed
  stack [defaultResolverArg, "build", "--flag", "this:library", "--flag", "this:tests", "this:test:this-test"]

  stack [defaultResolverArg, "clean", "--full"]
