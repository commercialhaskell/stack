import StackTest

main :: IO ()
main = do
  -- Custom Path
  stack ["install", "hindent", "--path", "./bin"]
  doesExist "./bin/hindent"

  -- Default install
  stack ["install", "hindent"]
  doesExist "$HOME/.local/bin/hindent"
