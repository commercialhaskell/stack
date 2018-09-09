{-# LANGUAGE NoImplicitPrelude #-}
module System.Permissions
  ( osIsWindows
  ) where

import RIO (Bool (..))

-- | False if not using Windows OS.
osIsWindows :: Bool
osIsWindows = False
