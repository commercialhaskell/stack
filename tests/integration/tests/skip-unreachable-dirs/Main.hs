import StackTest
import System.Directory
import Control.Exception (catch, IOException)

main :: IO ()
main = do
  removeFileIgnore "stack.yaml"
  createDirectory "unreachabledir" `catch` \(e :: IOException) -> pure ()
  setPermissions  "unreachabledir" emptyPermissions
  stack ["init"]
