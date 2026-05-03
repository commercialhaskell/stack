-- | Stack allows allow-newer to be applied to the dependencies of specified
-- packages.

import           StackTest

main :: IO ()
main = stack ["build"]
