{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

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
import           Stack.Prelude hiding ( stylesUpdate )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.LockFileBehavior ( LockFileBehavior )
import           Stack.Types.StackYamlLoc ( StackYamlLoc )

-- | The base environment that almost everything in Stack runs in, based off of
-- parsing command line options in 'GlobalOpts'. Provides logging, process
-- execution, and the MVar used to ensure that the Docker entrypoint is
-- performed exactly once.
data Runner = Runner
  { globalOpts           :: !GlobalOpts
  , useColor             :: !Bool
  , logFunc              :: !LogFunc
  , termWidth            :: !Int
  , processContext       :: !ProcessContext
  , dockerEntrypointMVar :: !(MVar Bool)
  }

instance HasLogFunc Runner where
  logFuncL = lens (.logFunc) (\x y -> x { logFunc = y })

instance HasProcessContext Runner where
  processContextL =
    lens (.processContext) (\x y -> x { processContext = y })

instance HasRunner Runner where
  runnerL = id

instance HasStylesUpdate Runner where
  stylesUpdateL :: Lens' Runner StylesUpdate
  stylesUpdateL = globalOptsL . lens
    (.stylesUpdate)
    (\x y -> x { stylesUpdate = y })

instance HasTerm Runner where
  useColorL = lens (.useColor) (\x y -> x { useColor = y })
  termWidthL = lens (.termWidth) (\x y -> x { termWidth  = y })

instance HasDockerEntrypointMVar Runner where
  dockerEntrypointMVarL =
    lens (.dockerEntrypointMVar) (\x y -> x { dockerEntrypointMVar = y })

-- | Class for environment values which have a 'Runner'.
class (HasProcessContext env, HasLogFunc env) => HasRunner env where
  runnerL :: Lens' env Runner

-- | Class for environment values which have a Docker entrypoint 'MVar'.
class HasRunner env => HasDockerEntrypointMVar env where
  dockerEntrypointMVarL :: Lens' env (MVar Bool)

stackYamlLocL :: HasRunner env => Lens' env StackYamlLoc
stackYamlLocL =
  globalOptsL . lens (.stackYaml) (\x y -> x { stackYaml = y })

lockFileBehaviorL :: HasRunner env => SimpleGetter env LockFileBehavior
lockFileBehaviorL = globalOptsL . to (.lockFileBehavior)

globalOptsL :: HasRunner env => Lens' env GlobalOpts
globalOptsL = runnerL . lens (.globalOpts) (\x y -> x { globalOpts = y })

-- | See 'globalTerminal'
terminalL :: HasRunner env => Lens' env Bool
terminalL =
  globalOptsL . lens (.terminal) (\x y -> x { terminal = y })

-- | See 'globalReExecVersion'
reExecL :: HasRunner env => SimpleGetter env Bool
reExecL = globalOptsL . to (isJust . (.reExecVersion))

rslInLogL :: HasRunner env => SimpleGetter env Bool
rslInLogL = globalOptsL . to (.rslInLog)
