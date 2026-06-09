{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Types.ConfigExtra
License     : BSD-3-Clause
-}

module Stack.Types.ConfigExtra
  ( ConfigExtra (..)
  ) where

import           Stack.Prelude
import           Stack.Types.Config ( Config )
import           Stack.Types.Project ( Project (..) )

-- | A type that represents 'Config' values together with some extra information
--
data ConfigExtra = ConfigExtra
  { config :: !Config
  , mSnapshot :: !(Maybe RawSnapshotLocation)
  , project :: !Project
  , configFile :: !(Either (Path Abs File) (Path Abs File))
    -- ^ Either (Left) the location of the user-specific global configuration
    -- file or, in most cases, (Right) the location of the project-level
    -- coniguration file (stack.yaml, by default).
    --
    -- Note: if the STACK_YAML environment variable is used, the location of the
    -- project-level configuration file may be different from
    -- projectRootL </> "stack.yaml" if a different file name is used.
  }
