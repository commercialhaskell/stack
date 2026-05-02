-- Stack accepts module names that include Unicode code points outside of the
-- Basic Latin Unicode block (ASCII).
--
-- See: https://github.com/commercialhaskell/stack/issues/4938

import           Control.Monad ( unless )
import           StackTest

main :: IO ()
main = do
  -- Disabled on Windows due to an error occurred in the integration tests
  -- regarding Unicode character. Tried to fix it (https://github.com/commercialhaskell/stack/pull/5162/commits/8f04ad9e4cbaa54370dc5af476e3307a16c84405)
  -- but it didn't work
  unless isWindows $
    stack ["build"]
