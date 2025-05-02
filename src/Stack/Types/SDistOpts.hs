{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NoFieldSelectors      #-}

{-|
Module      : Stack.Types.SDistOpts
Description : Types related to Stack's @sdist@ command.
License     : BSD-3-Clause

Types related to Stack's @sdist@ command.
-}

module Stack.Types.SDistOpts
  ( SDistOpts (..)
  ) where

import           Stack.Prelude
import           Stack.Types.PvpBounds ( PvpBounds )

-- | Type representing command line options for @stack sdist@ command.
data SDistOpts = SDistOpts
  { dirsToWorkWith :: [String]
    -- ^ Directories to package
  , pvpBounds :: Maybe PvpBounds
    -- ^ PVP Bounds overrides
  , ignoreCheck :: Bool
    -- ^ Whether to ignore check of the package for common errors
  , buildTarball :: Bool
    -- ^ Whether to build the tarball
  , tarPath :: Maybe FilePath
    -- ^ Where to copy the tarball
  }
