-- Stack supports a build target limited to locals.
--
-- See: https://github.com/commercialhaskell/stack/issues/5272

import           Control.Monad ( void )
import           StackTest

main :: IO ()
main = do
  -- Ensure that the acme-missiles package is not in a package database
  void $ stack' ["exec", "ghc-pkg", "unregister", "acme-missiles"]
  stackErr ["build", "--only-locals"]
  stack ["build", "--only-snapshot"]
  stack ["build", "--only-locals"]
