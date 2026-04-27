module TH
  ( thFunc
  ) where

import           Language.Haskell.TH ( Exp (..), Lit (..), Q, runIO )

thFunc :: Q Exp
thFunc = runIO $ do
  readFile "files/file.txt"
  pure $ LitE (IntegerL 5)
