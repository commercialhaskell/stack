{-# LANGUAGE RecordWildCards #-}

module Stackage.Path (getBinPaths) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Monoid
import Path
import System.FilePath

import Stackage.Config


getBinPaths :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m String
getBinPaths = do
   Config{..} <- getConfig Settings
   return $ toFilePath configGhcBinLocation <> [searchPathSeparator]
     <> toFilePath configCabalBinLocation
