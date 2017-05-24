module TH (thFunc) where

import Language.Haskell.TH

thFunc :: Q Exp
thFunc =
  return $ LitE (IntegerL 5)
