{-# LANGUAGE NoImplicitPrelude #-}
module System.IsWindows
  ( osIsWindows
  ) where

import RIO (Bool (..))

-- | False if not using Windows OS.
osIsWindows :: Bool
osIsWindows = False
