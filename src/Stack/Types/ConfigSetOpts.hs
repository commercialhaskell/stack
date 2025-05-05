{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NoFieldSelectors      #-}

{-|
Module      : Stack.Types.ConfigSetOpts
Description : Types related to Stack's @config set@ command.
License     : BSD-3-Clause

Types related to Stack's @config set@ command.
-}

module Stack.Types.ConfigSetOpts
  ( ConfigCmdSet (..)
  , CommandScope (..)
  , configCmdSetScope
  ) where

import           Stack.Prelude
import           Stack.Types.Snapshot ( AbstractSnapshot )

-- | Type representing options for Stack's @config set@ command.
data ConfigCmdSet
  = ConfigCmdSetSnapshot !(Unresolved AbstractSnapshot)
  | ConfigCmdSetResolver !(Unresolved AbstractSnapshot)
  | ConfigCmdSetSystemGhc !CommandScope !Bool
  | ConfigCmdSetInstallGhc !CommandScope !Bool
  | ConfigCmdSetInstallMsys !CommandScope !Bool
  | ConfigCmdSetRecommendStackUpgrade !CommandScope !Bool
  | ConfigCmdSetDownloadPrefix !CommandScope !Text

-- | Type representing scopes for Stack's @config set@ command.
data CommandScope
  = CommandScopeGlobal
    -- ^ Apply changes to Stack's global configuration file.
  | CommandScopeProject
    -- ^ Apply changes to Stack's project-level configuration file.

-- | Yields the command scope for the given config command option.
configCmdSetScope :: ConfigCmdSet -> CommandScope
configCmdSetScope (ConfigCmdSetSnapshot _) = CommandScopeProject
configCmdSetScope (ConfigCmdSetResolver _) = CommandScopeProject
configCmdSetScope (ConfigCmdSetSystemGhc scope _) = scope
configCmdSetScope (ConfigCmdSetInstallGhc scope _) = scope
configCmdSetScope (ConfigCmdSetInstallMsys scope _) = scope
configCmdSetScope (ConfigCmdSetRecommendStackUpgrade scope _) = scope
configCmdSetScope (ConfigCmdSetDownloadPrefix scope _) = scope
