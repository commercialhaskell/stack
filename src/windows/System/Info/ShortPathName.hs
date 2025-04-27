{-|
Module      : System.Info.ShortPathName
License     : BSD-3-Clause

The module of this name differs as between Windows and non-Windows builds. This
is the Windows version.
-}

module System.Info.ShortPathName
  ( getShortPathName
  ) where

import           System.Win32.Info ( getShortPathName )
