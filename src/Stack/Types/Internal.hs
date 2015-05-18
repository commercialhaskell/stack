-- | Internal types to the library.

module Stack.Types.Internal where

import Network.HTTP.Client.Conduit (Manager,HasHttpManager(..))
import Stack.Config (Config,HasConfig(..))

-- | Monadic environment.
data Env =
  Env {envConfig :: Config
      ,envManager :: Manager}

instance HasConfig Env where
  getConfig = envConfig

instance HasHttpManager Env where
  getHttpManager = envManager
