{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

{-|
Module      : Stack.Types.Runner
License     : BSD-3-Clause
-}

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
  , progNameL
  , mExecutablePathL
  , viewExecutablePath
  ) where

import           RIO.Process ( HasProcessContext (..), ProcessContext )
import           Stack.Prelude hiding ( stylesUpdate )
import           Stack.Types.Config.Exception ( ConfigPrettyException (..) )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.LockFileBehavior ( LockFileBehavior )
import           Stack.Types.StackYamlLoc ( StackYamlLoc )

-- | The base environment that almost everything in Stack runs in, based off of
-- parsing command line options in t'GlobalOpts'. Provides logging, process
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

-- | Class for environment values which have a t'Runner'.
class (HasProcessContext env, HasLogFunc env) => HasRunner env where
  runnerL :: Lens' env Runner

-- | Class for environment values which have a Docker entrypoint 'MVar'.
class HasRunner env => HasDockerEntrypointMVar env where
  dockerEntrypointMVarL :: Lens' env (MVar Bool)

-- | See the @stackYaml@ field of the v'GlobalOpts' data constructor.
stackYamlLocL :: HasRunner env => Lens' env StackYamlLoc
stackYamlLocL =
  globalOptsL . lens (.stackYaml) (\x y -> x { stackYaml = y })

-- | See the @lockFileBehavior@ field of the v'GlobalOpts' data constructor.
lockFileBehaviorL :: HasRunner env => SimpleGetter env LockFileBehavior
lockFileBehaviorL = globalOptsL . to (.lockFileBehavior)

-- | See the t'GlobalOpts' type.
globalOptsL :: HasRunner env => Lens' env GlobalOpts
globalOptsL = runnerL . lens (.globalOpts) (\x y -> x { globalOpts = y })

-- | See the @terminal@ field of the v'GlobalOpts' data constructor.
terminalL :: HasRunner env => Lens' env Bool
terminalL =
  globalOptsL . lens (.terminal) (\x y -> x { terminal = y })

-- | See the @reExecVersion@ field of the v'GlobalOpts' data constructor.
reExecL :: HasRunner env => SimpleGetter env Bool
reExecL = globalOptsL . to (isJust . (.reExecVersion))

-- | See the @rslInLog@ field of the v'GlobalOpts' data constructor.
rslInLogL :: HasRunner env => SimpleGetter env Bool
rslInLogL = globalOptsL . to (.rslInLog)

-- | See the @progNameL@ field of the v'GlobalOpts' data constructor.
progNameL :: HasRunner env => SimpleGetter env String
progNameL = globalOptsL . to (.progName)

-- | See the @mExecutablePath@ field of the v'GlobalOpts' data constructor.
mExecutablePathL :: HasRunner env => SimpleGetter env (Maybe (Path Abs File))
mExecutablePathL = globalOptsL . to (.mExecutablePath)

-- | Yield the path to the current Stack executable, if the operating system
-- provides a reliable way to determine it. Otherwise throw
-- 'Stack.Types.Config.Exception.NoExecutablePath'.
viewExecutablePath :: HasRunner env => RIO env (Path Abs File)
viewExecutablePath = view mExecutablePathL >>= \case
    Nothing -> view progNameL >>= prettyThrowM . NoExecutablePath
    Just executablePath -> pure executablePath
