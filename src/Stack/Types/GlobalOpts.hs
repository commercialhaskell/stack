{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.GlobalOpts
  ( GlobalOpts (..)
  , globalOptsBuildOptsMonoidL
  ) where

import          Stack.Prelude
import          Stack.Types.BuildOpts ( BuildOptsMonoid )
import          Stack.Types.ConfigMonoid ( ConfigMonoid (..) )
import          Stack.Types.DockerEntrypoint ( DockerEntrypoint )
import          Stack.Types.LockFileBehavior ( LockFileBehavior )
import          Stack.Types.Resolver ( AbstractResolver )
import          Stack.Types.StackYamlLoc ( StackYamlLoc )

-- | Parsed global command-line options.
data GlobalOpts = GlobalOpts
  { globalReExecVersion :: !(Maybe String)
    -- ^ Expected re-exec in container version
  , globalDockerEntrypoint :: !(Maybe DockerEntrypoint)
    -- ^ Data used when Stack is acting as a Docker entrypoint (internal use
    -- only)
  , globalLogLevel     :: !LogLevel -- ^ Log level
  , globalTimeInLog    :: !Bool -- ^ Whether to include timings in logs.
  , globalRSLInLog     :: !Bool
    -- ^ Whether to include raw snapshot layer (RSL) in logs.
  , globalConfigMonoid :: !ConfigMonoid
    -- ^ Config monoid, for passing into 'loadConfig'
  , globalResolver     :: !(Maybe AbstractResolver) -- ^ Resolver override
  , globalCompiler     :: !(Maybe WantedCompiler) -- ^ Compiler override
  , globalTerminal     :: !Bool -- ^ We're in a terminal?
  , globalStylesUpdate :: !StylesUpdate -- ^ SGR (Ansi) codes for styles
  , globalTermWidth    :: !(Maybe Int) -- ^ Terminal width override
  , globalStackYaml    :: !StackYamlLoc -- ^ Override project stack.yaml
  , globalLockFileBehavior :: !LockFileBehavior
  }
  deriving Show

globalOptsBuildOptsMonoidL :: Lens' GlobalOpts BuildOptsMonoid
globalOptsBuildOptsMonoidL =
  lens
    globalConfigMonoid
    (\x y -> x { globalConfigMonoid = y })
  .
  lens
    configMonoidBuildOpts
    (\x y -> x { configMonoidBuildOpts = y })
