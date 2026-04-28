module TH
  ( thFunc
  ) where

import           Language.Haskell.TH ( Exp (..), Lit (..), Q )

thFunc :: Q Exp
thFunc = pure $ LitE (IntegerL 5)
