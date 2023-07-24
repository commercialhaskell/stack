{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main
  ) where

import           RIO ( IO )
import qualified Stack

-- | The entry point for the Stack executable.
main :: IO ()
main = Stack.main
