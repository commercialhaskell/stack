import StackTest
import System.Directory (setPermissions, emptyPermissions, createDirectory)

main :: IO ()
main = do
    createDirectory "unreachabledir"
    setPermissions  "unreachabledir" emptyPermissions
    stack ["init"]
