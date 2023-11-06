{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.Runner
  ( Runner (..)
  , HasRunner (..)
  , HasDockerEntrypointMVar (..)
  , globalOptsL
  , stackYamlLocL
  , lockFileBehaviorL
  , terminalL
  , reExecL
  , rslInLogL
  ) where

import           RIO.Process ( HasProcessContext (..), ProcessContext )
import           Stack.Prelude
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.LockFileBehavior ( LockFileBehavior )
import           Stack.Types.StackYamlLoc ( StackYamlLoc )

-- | The base environment that almost everything in Stack runs in, based off of
-- parsing command line options in 'GlobalOpts'. Provides logging, process
-- execution, and the MVar used to ensure that the Docker entrypoint is
-- performed exactly once.
data Runner = Runner
  { runnerGlobalOpts :: !GlobalOpts
  , runnerUseColor   :: !Bool
  , runnerLogFunc    :: !LogFunc
  , runnerTermWidth  :: !Int
  , runnerProcessContext :: !ProcessContext
  , runnerDockerEntrypointMVar :: !(MVar Bool)
  }

instance HasLogFunc Runner where
  logFuncL = lens runnerLogFunc (\x y -> x { runnerLogFunc = y })

instance HasProcessContext Runner where
  processContextL =
    lens runnerProcessContext (\x y -> x { runnerProcessContext = y })

instance HasRunner Runner where
  runnerL = id

instance HasStylesUpdate Runner where
  stylesUpdateL = globalOptsL.
                  lens globalStylesUpdate (\x y -> x { globalStylesUpdate = y })
instance HasTerm Runner where
  useColorL = lens runnerUseColor (\x y -> x { runnerUseColor = y })
  termWidthL = lens runnerTermWidth (\x y -> x { runnerTermWidth = y })

instance HasDockerEntrypointMVar Runner where
  dockerEntrypointMVarL =
    lens runnerDockerEntrypointMVar (\x y -> x { runnerDockerEntrypointMVar = y })

-- | Class for environment values which have a 'Runner'.
class (HasProcessContext env, HasLogFunc env) => HasRunner env where
  runnerL :: Lens' env Runner

-- | Class for environment values which have a Docker entrypoint 'MVar'.
class HasRunner env => HasDockerEntrypointMVar env where
  dockerEntrypointMVarL :: Lens' env (MVar Bool)

stackYamlLocL :: HasRunner env => Lens' env StackYamlLoc
stackYamlLocL =
  globalOptsL.lens globalStackYaml (\x y -> x { globalStackYaml = y })

lockFileBehaviorL :: HasRunner env => SimpleGetter env LockFileBehavior
lockFileBehaviorL = globalOptsL.to globalLockFileBehavior

globalOptsL :: HasRunner env => Lens' env GlobalOpts
globalOptsL = runnerL.lens runnerGlobalOpts (\x y -> x { runnerGlobalOpts = y })

-- | See 'globalTerminal'
terminalL :: HasRunner env => Lens' env Bool
terminalL = globalOptsL.lens globalTerminal (\x y -> x { globalTerminal = y })

-- | See 'globalReExecVersion'
reExecL :: HasRunner env => SimpleGetter env Bool
reExecL = globalOptsL.to (isJust . globalReExecVersion)

rslInLogL :: HasRunner env => SimpleGetter env Bool
rslInLogL = globalOptsL.to globalRSLInLog
