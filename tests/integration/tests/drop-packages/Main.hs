-- | Stack can drop packages from a snapshot.

import           StackTest

main :: IO ()
main = stackErr ["build"]
