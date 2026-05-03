-- | Stack supports GHC installation customisation shell scripts.

import           Control.Exception ( throwIO )
import           StackTest
import           System.Process ( rawSystem )

main :: IO ()
main = rawSystem "sh" ["run.sh"] >>= throwIO
