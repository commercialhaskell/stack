{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NoFieldSelectors      #-}

{-|
Module      : Stack.Types.LsOpts
Description : Types related to Stack's @ls@ command.
License     : BSD-3-Clause

Types related to Stack's @ls@ command.
-}

module Stack.Types.LsOpts
  ( LsCmdOpts (..)
  , LsCmds (..)
  , SnapshotOpts (..)
  , LsView (..)
  , ListDepsOpts (..)
  , ListDepsFormat (..)
  , ListDepsFormatOpts (..)
  , ListDepsTextFilter (..)
  , ListGlobalsOpts (..)
  , ListStylesOpts (..)
  , ListToolsOpts (..)
  ) where

import           Stack.Prelude
import           Stack.Types.DotOpts ( DotOpts (..) )

-- | Type representing command line options for the @stack ls@ command.
newtype LsCmdOpts
  = LsCmdOpts { lsCmds :: LsCmds }

-- | Type representing subcommands for the @stack ls@ command.
data LsCmds
  = LsSnapshot SnapshotOpts
  | LsGlobals ListGlobalsOpts
  | LsDependencies ListDepsOpts
  | LsStyles ListStylesOpts
  | LsTools ListToolsOpts

-- | Type representing command line options for the @stack ls snapshots@
-- command.
data SnapshotOpts = SnapshotOpts
  { viewType :: LsView
  , ltsSnapView :: Bool
  , nightlySnapView :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Type representing subcommands for the @stack ls snapshots@ command.
data LsView
  = Local
  | Remote
  deriving (Eq, Ord, Show)

-- | Type representing command line options for the @stack ls globals@ command.
newtype ListGlobalsOpts = ListGlobalsOpts
  { globalHints :: Bool
    -- ^ Use global hints instead of relying on an actual GHC installation.
  }

-- | Type representing command line options for the @stack ls dependencies@
-- command.
data ListDepsOpts = ListDepsOpts
  { format :: !ListDepsFormat
    -- ^ Format of printing dependencies
  , dotOpts :: !DotOpts
    -- ^ The normal dot options.
  }

-- | Type representing formats for printing dependencies.
data ListDepsFormat
  = ListDepsText ListDepsFormatOpts [ListDepsTextFilter]
  | ListDepsTree ListDepsFormatOpts
  | ListDepsJSON
  | ListDepsConstraints

-- | Type representing command line options for the @stack ls dependencies text@
-- command and similar @cabal@, @tree@ and @json@ commands.
data ListDepsFormatOpts = ListDepsFormatOpts
  { sep :: !Text
    -- ^ Separator between the package name and details.
  , license :: !Bool
    -- ^ Print dependency licenses instead of versions.
  }

-- | Type representing items to filter the results of @stack ls dependencies@.
data ListDepsTextFilter
  = FilterPackage PackageName
    -- ^ Item is a package name.
  | FilterLocals
    -- ^ Item represents all project packages.

-- | Type representing command line options for the @stack ls stack-colors@ and
-- @stack ls stack-colours@ commands.
data ListStylesOpts = ListStylesOpts
  { basic   :: Bool
  , sgr     :: Bool
  , example :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Type representing command line options for the @stack ls tools@ command.
newtype ListToolsOpts
  = ListToolsOpts { filter  :: String }
