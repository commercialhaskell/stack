{-# LANGUAGE NoImplicitPrelude #-}

module System.Info.ShortPathName
  ( getShortPathName
  ) where

import RIO.FilePath (FilePath)
import RIO.Prelude (pure)
import RIO.Prelude.Types (IO)

getShortPathName :: FilePath -> IO FilePath
getShortPathName = pure
