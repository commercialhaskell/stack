import StackTest

main :: IO ()
main = do
  stack ["build", "--test", "--bench", "--skip-component", "failing-test", "--skip-component", "failing-bench"]
  stack ["build", ":failing-test", ":failing-bench", "--skip-component", "failing-test", "--skip-component", "failing-bench"]
