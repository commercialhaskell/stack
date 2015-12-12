import StackTest

main :: IO ()
main = do stack ["build"]
          stack ["exec", "以-exe"]
