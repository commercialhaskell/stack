{-# LANGUAGE NoImplicitPrelude #-}
module System.IsWindows
  ( osIsWindows
  ) where

import RIO (Bool (..))

-- | True if using Windows OS.
osIsWindows :: Bool
osIsWindows = True
