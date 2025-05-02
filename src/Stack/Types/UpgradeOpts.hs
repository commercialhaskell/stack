{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors  #-}

{-|
Module      : Stack.Types.UpgradeOpts
Description : Types for command line options for Stack's @upgrade@ command.
License     : BSD-3-Clause

Types for command line options for Stack's @upgrade@ command.
-}

module Stack.Types.UpgradeOpts
  ( UpgradeOpts (..)
  , BinaryOpts (..)
  , SourceOpts (..)
  ) where

import           Stack.Prelude

-- | Type representing command line options for the @stack upgrade@ command.
data UpgradeOpts = UpgradeOpts
  { binary :: !(Maybe BinaryOpts)
  , source :: !(Maybe SourceOpts)
  }
  deriving Show

-- | Type representing options for upgrading Stack with a binary executable
-- file.
data BinaryOpts = BinaryOpts
  { platform :: !(Maybe String)
  , force :: !Bool
    -- ^ Force a download, even if the downloaded version is older than what we
    -- are.
  , onlyLocalBin :: !Bool
    -- ^ Only download to Stack's local binary directory.
  , version :: !(Maybe String)
    -- ^ Specific version to download
  , gitHubOrg :: !(Maybe String)
  , gitHubRepo :: !(Maybe String)
  }
  deriving Show

-- | Type representing options for upgrading Stack from source code.
newtype SourceOpts
  = SourceOpts (Maybe (String, String)) -- repo and branch
  deriving Show
