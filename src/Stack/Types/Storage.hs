{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Types.Storage
Description : Types used by @Stack.Storage@ modules.
License     : BSD-3-Clause

Types used by @Stack.Storage@ modules.
-}

module Stack.Types.Storage
  ( StoragePrettyException (..)
  , ProjectStorage (..)
  , UserStorage (..)
  ) where

import           Pantry.SQLite ( Storage )
import           Stack.Prelude

-- | Type representing \'pretty\' exceptions thrown by functions exported by
-- modules beginning @Stack.Storage@.
data StoragePrettyException
  = StorageMigrationFailure !Text !(Path Abs File) !SomeException
  deriving (Show, Typeable)

instance Pretty StoragePrettyException where
  pretty (StorageMigrationFailure desc fp ex) =
    "[S-8835]"
    <> line
    <> fillSep
         [ flow "Stack could not migrate the the database"
         , style File (fromString $ show desc)
         , flow "located at"
         , pretty fp
         ]
    <> "."
    <> blankLine
    <> flow "While migrating the database, Stack encountered the error:"
    <> blankLine
    <> string exMsg
    <> blankLine
    <> fillSep
         [ flow "Please report this as an issue at"
         , style Url "https://github.com/commercialhaskell/stack/issues"
         ]
    <> "."
    <> blankLine
    -- See https://github.com/commercialhaskell/stack/issues/5851
    <> if exMsg == winIOGHCRTSMsg
         then
           flow "This error can be caused by a bug that arises if GHC's \
                \'--io-manager=native' RTS option is set using the GHCRTS \
                \environment variable. As a workaround try setting the option \
                \in the project's Cabal file, Stack's configuration file or at \
                \the command line."
         else
           flow "As a workaround you may delete the database. This \
                \will cause the database to be recreated."
   where
    exMsg = displayException ex
    winIOGHCRTSMsg =
      "\\\\.\\NUL: hDuplicateTo: illegal operation (handles are incompatible)"

instance Exception StoragePrettyException

-- | A bit of type safety to ensure we're talking to the right database.
newtype UserStorage = UserStorage
  { userStorage :: Storage
  }

-- | A bit of type safety to ensure we're talking to the right database.
newtype ProjectStorage = ProjectStorage
  { projectStorage :: Storage
  }
