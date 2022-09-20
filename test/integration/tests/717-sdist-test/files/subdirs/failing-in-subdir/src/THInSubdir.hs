module THInSubdir (thFuncC) where

import Language.Haskell.TH

thFuncC :: Q Exp
thFuncC = runIO $ do
  readFile "files/file.txt"
  pure $ LitE (IntegerL 5)
