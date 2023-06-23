{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.GlobalOptsMonoid
  ( GlobalOptsMonoid (..)
  ) where

import           Generics.Deriving.Monoid ( mappenddefault, memptydefault )
import           Stack.Prelude
import           Stack.Types.ConfigMonoid ( ConfigMonoid )
import           Stack.Types.DockerEntrypoint ( DockerEntrypoint )
import           Stack.Types.LockFileBehavior ( LockFileBehavior )
import           Stack.Types.Resolver ( AbstractResolver )

-- | Parsed global command-line options monoid.
data GlobalOptsMonoid = GlobalOptsMonoid
  { globalMonoidReExecVersion :: !(First String)
    -- ^ Expected re-exec in container version
  , globalMonoidDockerEntrypoint :: !(First DockerEntrypoint)
    -- ^ Data used when Stack is acting as a Docker entrypoint (internal use
    -- only)
  , globalMonoidLogLevel     :: !(First LogLevel)
    -- ^ Log level
  , globalMonoidTimeInLog    :: !FirstTrue
    -- ^ Whether to include timings in logs.
  , globalMonoidRSLInLog     :: !FirstFalse
    -- ^ Whether to include raw snapshot layer (RSL) in logs.
  , globalMonoidPlanInLog :: !FirstFalse
    -- ^ Whether to include debug information about the construction of the
    -- build plan in logs.
  , globalMonoidConfigMonoid :: !ConfigMonoid
    -- ^ Config monoid, for passing into 'loadConfig'
  , globalMonoidResolver     :: !(First (Unresolved AbstractResolver))
    -- ^ Resolver override
  , globalMonoidResolverRoot :: !(First FilePath)
    -- ^ root directory for resolver relative path
  , globalMonoidCompiler     :: !(First WantedCompiler)
    -- ^ Compiler override
  , globalMonoidTerminal     :: !(First Bool)
    -- ^ We're in a terminal?
  , globalMonoidStyles       :: !StylesUpdate
    -- ^ Stack's output styles
  , globalMonoidTermWidth    :: !(First Int)
    -- ^ Terminal width override
  , globalMonoidStackYaml    :: !(First FilePath)
    -- ^ Override project stack.yaml
  , globalMonoidLockFileBehavior :: !(First LockFileBehavior)
    -- ^ See 'globalLockFileBehavior'
  }
  deriving Generic

instance Semigroup GlobalOptsMonoid where
  (<>) = mappenddefault

instance Monoid GlobalOptsMonoid where
  mempty = memptydefault
  mappend = (<>)
