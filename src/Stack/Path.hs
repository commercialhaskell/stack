{-# LANGUAGE RecordWildCards #-}

module Stack.Path (getBinPaths) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Monoid
import Path
import System.FilePath

import Stack.Config


getBinPaths :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m String
getBinPaths = do
   Config{..} <- getConfig
   return $ toFilePath configGhcBinLocation <> [searchPathSeparator]
     <> toFilePath configCabalBinLocation
