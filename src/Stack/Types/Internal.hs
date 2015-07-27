-- | Internal types to the library.

module Stack.Types.Internal where

import Control.Concurrent.MVar
import Control.Monad.Logger (LogLevel)
import Data.Text (Text)
import Network.HTTP.Client.Conduit (Manager,HasHttpManager(..))
import Stack.Types.Config

-- | Monadic environment.
data Env config =
  Env {envConfig :: !config
      ,envLogLevel :: !LogLevel
      ,envTerminal :: !Bool
      ,envReExec :: !Bool
      ,envManager :: !Manager
      ,envSticky :: !Sticky}

instance HasStackRoot config => HasStackRoot (Env config) where
    getStackRoot = getStackRoot . envConfig
instance HasPlatform config => HasPlatform (Env config) where
    getPlatform = getPlatform . envConfig
instance HasConfig config => HasConfig (Env config) where
    getConfig = getConfig . envConfig
instance HasBuildConfig config => HasBuildConfig (Env config) where
    getBuildConfig = getBuildConfig . envConfig
instance HasEnvConfig config => HasEnvConfig (Env config) where
    getEnvConfig = getEnvConfig . envConfig

instance HasHttpManager (Env config) where
  getHttpManager = envManager

class HasLogLevel r where
  getLogLevel :: r -> LogLevel

instance HasLogLevel (Env config) where
  getLogLevel = envLogLevel

instance HasLogLevel LogLevel where
  getLogLevel = id

class HasTerminal r where
  getTerminal :: r -> Bool

instance HasTerminal (Env config) where
  getTerminal = envTerminal

class HasReExec r where
  getReExec :: r -> Bool

instance HasReExec (Env config) where
  getReExec = envReExec

newtype Sticky = Sticky
    { unSticky :: Maybe (MVar (Maybe Text))
    }

class HasSticky r where
    getSticky :: r -> Sticky

instance HasSticky (Env config) where
  getSticky = envSticky
