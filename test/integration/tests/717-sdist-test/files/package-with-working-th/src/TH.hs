module TH (thFunc) where

import Language.Haskell.TH

thFunc :: Q Exp
thFunc =
  pure $ LitE (IntegerL 5)
