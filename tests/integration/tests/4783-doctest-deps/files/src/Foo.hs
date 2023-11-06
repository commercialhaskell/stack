module Foo where

-- |
--
-- >>> import Acme.Dont
-- >>> don't foo
foo :: IO ()
foo = error "foo"
