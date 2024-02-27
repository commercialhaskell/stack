{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Stack.Types.GlobalOpts
  ( GlobalOpts (..)
  , globalOptsBuildOptsMonoidL
  ) where

import          Stack.Prelude
import          Stack.Types.BuildOptsMonoid ( BuildOptsMonoid )
import          Stack.Types.ConfigMonoid ( ConfigMonoid (..) )
import          Stack.Types.DockerEntrypoint ( DockerEntrypoint )
import          Stack.Types.LockFileBehavior ( LockFileBehavior )
import          Stack.Types.StackYamlLoc ( StackYamlLoc )
import          Stack.Types.Snapshot ( AbstractSnapshot )

-- | Parsed global command-line options.
data GlobalOpts = GlobalOpts
  { reExecVersion :: !(Maybe String)
    -- ^ Expected re-exec in container version
  , dockerEntrypoint :: !(Maybe DockerEntrypoint)
    -- ^ Data used when Stack is acting as a Docker entrypoint (internal use
    -- only)
  , logLevel     :: !LogLevel -- ^ Log level
  , timeInLog    :: !Bool -- ^ Whether to include timings in logs.
  , rslInLog     :: !Bool
    -- ^ Whether to include raw snapshot layer (RSL) in logs.
  , planInLog :: !Bool
    -- ^ Whether to include debug information about the construction of the
    -- build plan in logs.
  , configMonoid :: !ConfigMonoid
    -- ^ Config monoid, for passing into 'loadConfig'
  , snapshot     :: !(Maybe AbstractSnapshot) -- ^ Snapshot override
  , compiler     :: !(Maybe WantedCompiler) -- ^ Compiler override
  , terminal     :: !Bool -- ^ We're in a terminal?
  , stylesUpdate :: !StylesUpdate -- ^ SGR (Ansi) codes for styles
  , termWidthOpt  :: !(Maybe Int) -- ^ Terminal width override
  , stackYaml    :: !StackYamlLoc -- ^ Override project stack.yaml
  , lockFileBehavior :: !LockFileBehavior
  }

globalOptsBuildOptsMonoidL :: Lens' GlobalOpts BuildOptsMonoid
globalOptsBuildOptsMonoidL =
    lens (.configMonoid) (\x y -> x { configMonoid = y })
  . lens (.buildOpts) (\x y -> x { buildOpts = y })
