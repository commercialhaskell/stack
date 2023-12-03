{-# LANGUAGE NoImplicitPrelude #-}

-- | Module exporting the `DotOpts` type used by Stack's @dot@ and
-- @ls dependencies@ commands.
module Stack.Types.DotOpts
  ( DotOpts (..)
  ) where

import           Stack.Prelude
import           Stack.Types.BuildOpts ( ApplyCLIFlag )

-- | Options record for @stack dot@ and @stack ls dependencies@
data DotOpts = DotOpts
  { dotIncludeExternal :: !Bool
    -- ^ Include external dependencies
  , dotIncludeBase :: !Bool
    -- ^ Include dependencies on base
  , dotDependencyDepth :: !(Maybe Int)
    -- ^ Limit the depth of dependency resolution to (Just n) or continue until
    -- fixpoint
  , dotPrune :: !(Set PackageName)
    -- ^ Package names to prune from the graph
  , dotTargets :: [Text]
    -- ^ Stack TARGETs to trace dependencies for
  , dotFlags :: !(Map ApplyCLIFlag (Map FlagName Bool))
    -- ^ Flags to apply when calculating dependencies
  , dotTestTargets :: Bool
    -- ^ Like the "--test" flag for build, affects the meaning of 'dotTargets'.
  , dotBenchTargets :: Bool
    -- ^ Like the "--bench" flag for build, affects the meaning of 'dotTargets'.
  , dotGlobalHints :: Bool
    -- ^ Use global hints instead of relying on an actual GHC installation.
  }
