{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors  #-}

{-|
Module      : Stack.Types.IdeOpts
Description : Types for command line options for Stack's @ide@ commands.
License     : BSD-3-Clause

Types for command line options for Stack's @ide@ commands.
-}

module Stack.Types.IdeOpts
  ( OutputStream (..)
  , ListPackagesCmd (..)
  ) where

-- | Type representing output stream choices for the @stack ide packages@ and
-- @stack ide targets@ commands.
data OutputStream
  = OutputLogInfo
    -- ^ To the same output stream as other log information.
  | OutputStdout
    -- ^ To the standard output stream.

-- | Type representing output choices for the @stack ide packages@ command.
data ListPackagesCmd
  = ListPackageNames
    -- ^ Package names.
  | ListPackageCabalFiles
    -- ^ Paths to Cabal files.
