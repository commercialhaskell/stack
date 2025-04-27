{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors  #-}

{-|
Module      : Stack.Types.DotOpts
License     : BSD-3-Clause

Module exporting the t`DotOpts` type used by Stack's @dot@ and @ls dependencies@
commands.
-}

module Stack.Types.DotOpts
  ( DotOpts (..)
  ) where

import           Stack.Prelude
import           Stack.Types.BuildOptsCLI ( ApplyCLIFlag )

-- | Options record for @stack dot@ and @stack ls dependencies@
data DotOpts = DotOpts
  { includeExternal :: !Bool
    -- ^ Include external dependencies
  , includeBase :: !Bool
    -- ^ Include dependencies on base
  , dependencyDepth :: !(Maybe Int)
    -- ^ Limit the depth of dependency resolution to (Just n) or continue until
    -- fixpoint
  , prune :: !(Set PackageName)
    -- ^ Package names to prune from the graph
  , dotTargets :: [Text]
    -- ^ Stack TARGETs to trace dependencies for
  , flags :: !(Map ApplyCLIFlag (Map FlagName Bool))
    -- ^ Flags to apply when calculating dependencies
  , testTargets :: Bool
    -- ^ Like the "--test" flag for build, affects the meaning of 'dotTargets'.
  , benchTargets :: Bool
    -- ^ Like the "--bench" flag for build, affects the meaning of 'dotTargets'.
  , globalHints :: Bool
    -- ^ Use global hints instead of relying on an actual GHC installation.
  }
