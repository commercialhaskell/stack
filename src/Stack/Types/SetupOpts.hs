{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NoFieldSelectors      #-}

{-|
Module      : Stack.Types.SetupOpts
Description : Types related to Stack's @setup@ command.
License     : BSD-3-Clause

Types related to Stack's @setup@ command.
-}

module Stack.Types.SetupOpts
  ( SetupCmdOpts (..)
  ) where

import           Stack.Prelude

-- | Type representing command line options for the @stack setup@ command.
data SetupCmdOpts = SetupCmdOpts
  { compilerVersion :: !(Maybe WantedCompiler)
  , forceReinstall  :: !Bool
  , ghcBindistUrl   :: !(Maybe String)
  , ghcjsBootOpts   :: ![String]
  , ghcjsBootClean  :: !Bool
  }
