module Foo where

-- | The function below intentionally contains invalid Haddock
foo :: ()
foo = () -- ^ this should fail!!!
