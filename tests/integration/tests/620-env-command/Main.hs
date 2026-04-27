-- Stack's config env command produces output that can be evaluated by bash's
-- built-in eval command.
--
-- See: https://github.com/commercialhaskell/stack/issues/617

import           Control.Exception ( throwIO )
import           Control.Monad ( unless )
import           StackTest
import           System.Process ( rawSystem )

main :: IO ()
main = unless isWindows $ rawSystem "bash" ["run.sh"] >>= throwIO
