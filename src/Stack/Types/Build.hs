{-# LANGUAGE NoImplicitPrelude     #-}

{-|
Module      : Stack.Types.Build
Description : Build-specific types.
License     : BSD-3-Clause

Build-specific types.
-}

module Stack.Types.Build
  ( ExcludeTHLoading (..)
  , ConvertPathsToAbsolute (..)
  , KeepOutputOpen (..)
  ) where

import           Stack.Prelude

-- | Type representing treatments of GHC's informational messages during
-- compilation when it evaluates Template Haskell code.
data ExcludeTHLoading
  = ExcludeTHLoading
    -- ^ Suppress the messages.
  | KeepTHLoading
    -- ^ Do not suppress the messages.

data ConvertPathsToAbsolute
  = ConvertPathsToAbsolute
  | KeepPathsAsIs

-- | Special marker for expected failures in curator builds, using those we need
-- to keep log handle open as build continues further even after a failure.
data KeepOutputOpen
  = KeepOpen
  | CloseOnException
  deriving Eq
