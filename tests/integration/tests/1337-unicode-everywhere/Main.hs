-- Stack accepts Unicode code points outside of the Basic Latin Unicode block
-- (ASCII).
--
-- See: https://github.com/commercialhaskell/stack/issues/1337

import           Control.Monad ( unless )
import           StackTest

main :: IO ()
-- The GitHub windows-latest (Microsoft Windows Server 2025) environment appears
-- to be unable to handle these Unicode code points.
main = unless isWindows $ do
  stack ["build"]
  stack ["exec", "以-exe"]
