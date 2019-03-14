import StackTest

main :: IO ()
main = do

  stack [defaultResolverArg, "clean", "--full"]

  -- Nothing should be built here
  stack [defaultResolverArg, "build", "--skip", "this"]
  doesNotExist "./this/.stack-work"

  -- With no flags enabled, stack should fail to build
  stack [defaultResolverArg, "build"]
  -- TODO stackErr [defaultResolverArg, "build"]


  stack [defaultResolverArg, "build", "--skip", "this", "--skip", "this-exe", "this"]

  -- "That" should be built, but "this" should be skipped
  stack [defaultResolverArg, "build", "--skip", "this", "--skip", "this-exe"]
  stack [defaultResolverArg, "test", "--skip", "this-test", "--skip", "this-exe", "--skip", "this"]

  doesNotExist "./this/.stack-work"
