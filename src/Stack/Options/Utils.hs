{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Options.Utils
License     : BSD-3-Clause
-}

module Stack.Options.Utils
  ( GlobalOptsContext (..)
  , hideMods
  ) where

import           Options.Applicative ( Mod, hidden, idm, internal )
import           Stack.Prelude

-- | If argument is True, hides the option from usage and help
hideMods :: Bool -> Mod f a
hideMods hide = if hide then internal <> hidden else idm

-- | Allows adjust global options depending on their context
-- Note: This was being used to remove ambiguity between the local and global
-- implementation of stack init --snapshot option. Now that stack init has no
-- local --snapshot this is not being used anymore but the code is kept for any
-- similar future use cases.
data GlobalOptsContext
  = OuterGlobalOpts -- ^ Global options before subcommand name
  | OtherCmdGlobalOpts -- ^ Global options following any other subcommand
  | BuildCmdGlobalOpts
  | GhciCmdGlobalOpts
  deriving (Eq, Show)
