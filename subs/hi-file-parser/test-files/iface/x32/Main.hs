{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax

main :: IO ()
main = $(do
  qAddDependentFile "some-dependency.txt"
  [|pure ()|])
