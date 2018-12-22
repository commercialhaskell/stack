import StackTest

main :: IO ()
main = do

  stack ["clean", "--full"]

  -- Nothing should be built here
  stack ["build", "--skip", "this", "--skip", "this-exe", "this"]
  doesNotExist "./that/.stack-work"
  doesNotExist "./this/.stack-work"

  -- "That" should be built, but "this" should be skipped
  stack ["build", "--skip", "this", "--skip", "this-exe"]
  stack ["test", "--skip", "this-test", "--skip", "this-exe", "--skip", "this"]

  doesExist "./that/.stack-work"
  doesNotExist "./this/.stack-work"
