import StackTest

main :: IO ()
main = do

  -- The lib, exe, and test components of "this" package will fail if built.
  -- But they should be skipped
  stack ["build", "--skip", "this-exe", "--skip", "this"]
  stack ["test", "--skip", "this-test", "--skip", "this-exe", "--skip", "this"]
