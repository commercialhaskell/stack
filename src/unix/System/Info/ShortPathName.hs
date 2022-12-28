{-# LANGUAGE NoImplicitPrelude #-}

-- | The module of this name differs as between Windows and non-Windows builds.
-- This is the non-Windows version.
module System.Info.ShortPathName
  ( getShortPathName
  ) where

import RIO.FilePath (FilePath)
import RIO.Prelude (pure)
import RIO.Prelude.Types (IO)

getShortPathName :: FilePath -> IO FilePath
getShortPathName = pure
