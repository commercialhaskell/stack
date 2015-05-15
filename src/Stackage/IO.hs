{-# LANGUAGE RankNTypes #-}

-- | IO Functions

module Stackage.IO (tryIO) where

import Control.Exception (IOException, try)

-- | Control.Exception.try specialized to IOException
tryIO :: forall a.
         IO a -> IO (Either IOException a)
tryIO = try
