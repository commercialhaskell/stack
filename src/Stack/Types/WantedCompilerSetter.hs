{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.WantedCompilerSetter
  ( WantedCompilerSetter (..)
  ) where

import           Stack.Prelude

-- | Type representing ways that a wanted compiler is set.
data WantedCompilerSetter
  = CompilerAtCommandLine
    -- ^ At the command line with --compiler option.
  | SnapshotAtCommandLine
    -- ^ At the command line with --snapshot (or --resolver) option.
  | YamlConfiguration (Maybe (Path Abs File))
    -- ^ Via a YAML configuration file.
  deriving (Show, Typeable)
