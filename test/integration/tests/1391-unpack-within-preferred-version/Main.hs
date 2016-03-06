import Control.Monad (when)
import StackTest
import System.Exit


main :: IO ()
main = do
    -- get version from snapshot
    stack ["--resolver=lts-4.0", "unpack", "stack"]
    doesExist "stack-1.0.0"

    -- check latest download querying hackage
    -- stack ["unpack", "--latest", "stack"]
    -- let latestCmd = unwords
    --         ["curl -H 'Accept: application/json'"
    --          ,"http://hackage.haskell.org/package/stack/preferred"
    --          ,"| sed -n  's/.*\"normal-version\":\\[\\([0-9,\"\\.]*\\)\\].*/\\1/p'"
    --          ,"| tr ',' '\n' | tr -d '\"'"
    --          ,"| sort"
    --          ,"| tail -1"
    --         ]
    -- ec <- run' "bash" ["-c", "ls stack-$("++ latestCmd ++")"]
    -- when (ec /= ExitSuccess) $ fail "didn't match latest on hackage."

    -- download deprecated version when asked explicitly
    stack ["unpack", "stack-9.9.9"]
    doesExist "stack-9.9.9"

    -- should fail when a package does not exist
    stackErr ["unpack", "i-dont-exists-0"]
