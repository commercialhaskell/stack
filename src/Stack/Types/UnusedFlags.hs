{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.UnusedFlags
  ( UnusedFlags (..)
  , FlagSource (..)
  ) where

import           Stack.Prelude

data FlagSource
  = FSCommandLine
  | FSStackYaml
  deriving (Eq, Ord, Show)

data UnusedFlags
  = UFNoPackage FlagSource PackageName
  | UFFlagsNotDefined
      FlagSource
      PackageName
      (Set FlagName) -- defined in package
      (Set FlagName) -- not defined
  | UFSnapshot PackageName
  deriving (Eq, Ord, Show)
