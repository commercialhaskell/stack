module Lib
  ( unicode
  ) where

{-# WARNING unicode "Warning itself emits unicode: 😊" #-}
unicode :: IO ()
unicode = putStrLn "Function outputs unicode: 😊"
