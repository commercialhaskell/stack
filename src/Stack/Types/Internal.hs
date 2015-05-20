-- | Internal types to the library.

module Stack.Types.Internal where

import Control.Monad.Logger (LogLevel)
import Network.HTTP.Client.Conduit (Manager,HasHttpManager(..))
import Stack.Types.Config

-- | Monadic environment.
data Env config =
  Env {envConfig :: !config
      ,envLogLevel :: !LogLevel
      ,envManager :: !Manager}

instance HasStackRoot config => HasStackRoot (Env config) where
    getStackRoot = getStackRoot . envConfig
instance HasUrls config => HasUrls (Env config) where
    getUrls = getUrls . envConfig
instance HasConfig config => HasConfig (Env config) where
    getConfig = getConfig . envConfig
instance HasBuildConfig config => HasBuildConfig (Env config) where
    getBuildConfig = getBuildConfig . envConfig

instance HasHttpManager (Env config) where
  getHttpManager = envManager

class HasLogLevel r where
  getLogLevel :: r -> LogLevel

instance HasLogLevel (Env config) where
  getLogLevel = envLogLevel

instance HasLogLevel LogLevel where
  getLogLevel = id
