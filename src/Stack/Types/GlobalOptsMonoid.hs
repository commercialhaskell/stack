{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors  #-}

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
  { reExecVersion :: !(First String)
    -- ^ Expected re-exec in container version
  , dockerEntrypoint :: !(First DockerEntrypoint)
    -- ^ Data used when Stack is acting as a Docker entrypoint (internal use
    -- only)
  , logLevel     :: !(First LogLevel)
    -- ^ Log level
  , timeInLog    :: !FirstTrue
    -- ^ Whether to include timings in logs.
  , rslInLog     :: !FirstFalse
    -- ^ Whether to include raw snapshot layer (RSL) in logs.
  , planInLog :: !FirstFalse
    -- ^ Whether to include debug information about the construction of the
    -- build plan in logs.
  , configMonoid :: !ConfigMonoid
    -- ^ Config monoid, for passing into 'loadConfig'
  , resolver     :: !(First (Unresolved AbstractResolver))
    -- ^ Resolver override
  , resolverRoot :: !(First FilePath)
    -- ^ root directory for resolver relative path
  , compiler     :: !(First WantedCompiler)
    -- ^ Compiler override
  , terminal     :: !(First Bool)
    -- ^ We're in a terminal?
  , styles       :: !StylesUpdate
    -- ^ Stack's output styles
  , termWidth    :: !(First Int)
    -- ^ Terminal width override
  , stackYaml    :: !(First FilePath)
    -- ^ Override project stack.yaml
  , lockFileBehavior :: !(First LockFileBehavior)
    -- ^ See 'globalLockFileBehavior'
  }
  deriving Generic

instance Semigroup GlobalOptsMonoid where
  (<>) = mappenddefault

instance Monoid GlobalOptsMonoid where
  mempty = memptydefault
  mappend = (<>)
