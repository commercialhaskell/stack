-- | Internal types to the library.

module Stack.Types.Internal where

import           Control.Concurrent.MVar
import           Control.Monad.Logger (LogLevel)
import           Data.ByteString (ByteString)

import           Network.HTTP.Client.Conduit (Manager,HasHttpManager(..))
import           Stack.Types.Config

-- | Monadic environment.
data Env config =
  Env {envConfig :: !config
      ,envLogLevel :: !LogLevel
      ,envManager :: !Manager
      ,envSticky :: !(MVar Sticky)}

instance HasStackRoot config => HasStackRoot (Env config) where
    getStackRoot = getStackRoot . envConfig
instance HasPlatform config => HasPlatform (Env config) where
    getPlatform = getPlatform . envConfig
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

data Sticky = Sticky
    { stickyCurrentLine :: !(Maybe ByteString)
    , stickyMaxColumns :: !Int
    , stickyLastWasSticky :: !Bool
    }

class HasSticky r where
    getSticky :: r -> MVar Sticky

instance HasSticky (Env config) where
  getSticky = envSticky
