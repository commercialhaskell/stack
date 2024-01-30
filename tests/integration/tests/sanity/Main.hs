import StackTest
import Control.Monad (unless)
import System.Directory (doesFileExist)

main :: IO ()
main = do
    stack ["--version"]
    stack ["--help"]
    removeDirIgnore "acme-missiles-0.2"
    removeDirIgnore "acme-missiles-0.3"
    stack ["unpack", "acme-missiles-0.2"]
    stack ["unpack", "acme-missiles"]
    stackErr ["command-does-not-exist"]
    stackErr ["unpack", "invalid-package-name-"]

    -- When running outside of IntegrationSpec.hs, this will use the
    -- stack.yaml from Stack itself
    exists <- doesFileExist "../../../../../stack.yaml"
    unless exists $ stackErr ["build"]

    doesNotExist "stack.yaml"

    if isWindows
        then stack [defaultSnapshotArg, "exec", "./foo.bat"]
        else stack [defaultSnapshotArg, "exec", "./foo.sh"]
