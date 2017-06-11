module TH (thFunc) where

import Language.Haskell.TH

thFunc :: Q Exp
thFunc = runIO $ do
  readFile "files/file.txt"
  return $ LitE (IntegerL 5)
