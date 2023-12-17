{-# LANGUAGE OverloadedStrings #-}

import StackTest
import Control.Exception (throwIO)
import Data.Maybe (mapMaybe)
import Data.Foldable (for_)
import Data.List (stripPrefix)

main :: IO ()
main = do
  for_ ["lts-20.26", "lts-22.0"] $ \snapshot -> do
    stack ["init", "--force", "--resolver", snapshot]
    str <- readFile "stack.yaml"
    case mapMaybe (stripPrefix "resolver: ") $ lines str of
      [x] ->
        if filter (/= '\r') x == snapshot
          then pure ()
          else error $ "Mismatch: " ++ show (snapshot, x)
      _ -> error $ "Wrong number of resolvers: " ++ show str
