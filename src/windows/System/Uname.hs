module System.Uname
  ( getRelease
  ) where

getRelease :: IO String
getRelease = error "getRelease not supported on Windows"
