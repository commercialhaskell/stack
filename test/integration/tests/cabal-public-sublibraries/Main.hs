import StackTest

main :: IO ()
main = do
  putStrLn "Disabled: CI doesn't have GHC 8.8.1"
  --stack ["build"]
