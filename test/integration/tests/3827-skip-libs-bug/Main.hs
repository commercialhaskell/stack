import StackTest

main :: IO ()
main = do

  stack [defaultResolverArg, "clean", "--full"]

  -- Nothing should be built here
  stack [defaultResolverArg, "build", "--skip", "this", "--skip", "this-exe", "this"]
  doesNotExist "./that/.stack-work"
  doesNotExist "./this/.stack-work"

  -- "That" should be built, but "this" should be skipped
  stack [defaultResolverArg, "build", "--skip", "this", "--skip", "this-exe"]
  stack [defaultResolverArg, "test", "--skip", "this-test", "--skip", "this-exe", "--skip", "this"]

  doesExist "./that/.stack-work"
  doesNotExist "./this/.stack-work"
