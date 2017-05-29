import StackTest

main :: IO ()
main = do
  stack ["build", "--test", "--bench", "--skip", "failing-test", "--skip", "failing-bench"]
  stack ["build", ":failing-test", ":failing-bench", "--skip", "failing-test", "--skip", "failing-bench"]
