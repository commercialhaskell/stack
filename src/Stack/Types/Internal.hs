{-# LANGUAGE Rank2Types #-}

-- | Internal types to the library.

module Stack.Types.Internal where

import Control.Concurrent.MVar
import Control.Monad.Logger (LogLevel)
import Data.Monoid.Extra
import Data.Text (Text)
import Lens.Micro
import Network.HTTP.Client.Conduit (Manager,HasHttpManager(..))
import Stack.Types.Config

-- | Monadic environment.
data Env config =
  Env {envConfig :: !config
      ,envReExec :: !Bool
      ,envManager :: !Manager
      ,envLogOptions :: !LogOptions
      ,envTerminal :: !Bool
      ,envSticky :: !Sticky
      }

instance HasStackRoot config => HasStackRoot (Env config) where
    getStackRoot = getStackRoot . envConfig
instance HasPlatform config => HasPlatform (Env config) where
    getPlatform = getPlatform . envConfig
    getPlatformVariant = getPlatformVariant . envConfig
instance HasGHCVariant config => HasGHCVariant (Env config) where
    getGHCVariant = getGHCVariant . envConfig
instance HasConfig config => HasConfig (Env config) where
    getConfig = getConfig . envConfig
instance HasBuildConfig config => HasBuildConfig (Env config) where
    getBuildConfig = getBuildConfig . envConfig
instance HasEnvConfig config => HasEnvConfig (Env config) where
    getEnvConfig = getEnvConfig . envConfig

instance HasHttpManager (Env config) where
  getHttpManager = envManager

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

data LogOptions = LogOptions
  { logUseColor :: Bool
  , logUseUnicode :: Bool
  , logUseTime :: Bool
  , logMinLevel :: LogLevel
  , logVerboseFormat :: Bool
  }

class HasLogOptions r where
  getLogOptions :: r -> LogOptions

instance HasLogOptions (Env config) where
  getLogOptions = envLogOptions

envEnvConfig :: Lens' (Env EnvConfig) EnvConfig
envEnvConfig = lens envConfig
                    (\s t -> s {envConfig = t})

buildOptsMonoidHaddock :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidHaddock = lens (getFirst . buildMonoidHaddock)
                            (\buildMonoid t -> buildMonoid {buildMonoidHaddock = First t})

buildOptsMonoidTests :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidTests = lens (getFirst . buildMonoidTests)
                            (\buildMonoid t -> buildMonoid {buildMonoidTests = First t})

buildOptsMonoidBenchmarks :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidBenchmarks = lens (getFirst . buildMonoidBenchmarks)
                            (\buildMonoid t -> buildMonoid {buildMonoidBenchmarks = First t})

buildOptsMonoidInstallExes :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidInstallExes =
  lens (getFirst . buildMonoidInstallExes)
       (\buildMonoid t -> buildMonoid {buildMonoidInstallExes = First t})

buildOptsInstallExes :: Lens' BuildOpts Bool
buildOptsInstallExes =
  lens boptsInstallExes
       (\bopts t -> bopts {boptsInstallExes = t})

buildOptsHaddock :: Lens' BuildOpts Bool
buildOptsHaddock =
  lens boptsHaddock
       (\bopts t -> bopts {boptsHaddock = t})

envConfigBuildOpts :: Lens' EnvConfig BuildOpts
envConfigBuildOpts =
    lens
        (configBuild . bcConfig . envConfigBuildConfig)
        (\envCfg bopts ->
              envCfg
              { envConfigBuildConfig = (envConfigBuildConfig envCfg)
                { bcConfig = (bcConfig (envConfigBuildConfig envCfg))
                  { configBuild = bopts
                  }
                }
              })

globalOptsBuildOptsMonoid :: Lens' GlobalOpts BuildOptsMonoid
globalOptsBuildOptsMonoid =
    lens
        (configMonoidBuildOpts . globalConfigMonoid)
        (\globalOpts boptsMonoid ->
              globalOpts
              { globalConfigMonoid = (globalConfigMonoid globalOpts)
                { configMonoidBuildOpts = boptsMonoid
                }
              })
