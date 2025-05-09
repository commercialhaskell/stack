{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NoFieldSelectors      #-}

{-|
Module      : Stack.Types.GhciOpts
Description : Types related to Stack's @ghci@ and @repl@ commands.
License     : BSD-3-Clause

Types related to Stack's @ghci@ and @repl@ commands.
-}

module Stack.Types.GhciOpts
  ( GhciOpts (..)
  ) where

import           Stack.Prelude
import           Stack.Types.BuildOptsCLI ( ApplyCLIFlag (..) )

-- | Type respresenting command line options for Stack's @ghci@ and @repl@
-- commands.
data GhciOpts = GhciOpts
  { targets            :: ![Text]
  , args               :: ![String]
  , ghcOptions         :: ![String]
  , flags              :: !(Map ApplyCLIFlag (Map FlagName Bool))
  , ghcCommand         :: !(Maybe FilePath)
  , noLoadModules      :: !Bool
  , additionalPackages :: ![String]
  , mainIs             :: !(Maybe Text)
  , loadLocalDeps      :: !Bool
  , hidePackages       :: !(Maybe Bool)
  , noBuild            :: !Bool
  , onlyMain           :: !Bool
  }
  deriving Show
