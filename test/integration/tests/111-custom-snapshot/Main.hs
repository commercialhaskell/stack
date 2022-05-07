import StackTest

main :: IO ()
main = do
  stack ["--verbose", "build"] -- More information, to try to diagnose the
                               -- macos-latest CI problem.
