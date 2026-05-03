-- | Stack supports private named libraries (internal libraries) and foreign
-- libraries.

import           StackTest

main :: IO ()
main = stack ["build"]
