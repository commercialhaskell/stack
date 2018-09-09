{-# LANGUAGE NoImplicitPrelude #-}
module System.Permissions
  ( osIsWindows
  ) where

import RIO (Bool (..))

-- | True if using Windows OS.
osIsWindows :: Bool
osIsWindows = True
