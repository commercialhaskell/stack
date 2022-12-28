-- | The module of this name differs as between Windows and non-Windows builds.
-- This is the Windows version.
module System.Uname
  ( getRelease
  ) where

getRelease :: IO String
getRelease = error "getRelease not supported on Windows"
