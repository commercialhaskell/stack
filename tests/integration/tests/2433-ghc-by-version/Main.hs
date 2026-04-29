-- If using a 'system' GHC, Stack uses the specified version of GHC on the PATH
-- and not 'ghc' (without a version) on the PATH.
--
-- See: https://github.com/commercialhaskell/stack/issues/2433

import           Control.Exception ( throwIO )
import           Control.Monad ( unless )
import           StackTest
import           System.Process ( rawSystem )

main :: IO ()
main = unless isWindows $
  rawSystem "bash" ["run.sh"] >>= throwIO
