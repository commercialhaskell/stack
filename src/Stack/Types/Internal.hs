{-# LANGUAGE Rank2Types #-}

-- | Internal types to the library.

module Stack.Types.Internal where

import Control.Concurrent.MVar
import Control.Monad.Logger (LogLevel)
import Data.Text (Text)
import Lens.Micro
import Network.HTTP.Client.Conduit (Manager,HasHttpManager(..))
import Stack.Types.Config

-- | Monadic environment.
data Env config =
  Env {envConfig :: !config
      ,envLogLevel :: !LogLevel
      ,envTerminal :: !Bool
      ,envReExec :: !Bool
      ,envManager :: !Manager
      ,envSticky :: !Sticky
      ,envSupportsUnicode :: !Bool}

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

class HasSupportsUnicode r where
  getSupportsUnicode :: r -> Bool

instance HasSupportsUnicode (Env config) where
  getSupportsUnicode = envSupportsUnicode

newtype Sticky = Sticky
    { unSticky :: Maybe (MVar (Maybe Text))
    }

class HasSticky r where
    getSticky :: r -> Sticky

instance HasSticky (Env config) where
  getSticky = envSticky

envEnvConfig :: Lens' (Env EnvConfig) EnvConfig
envEnvConfig = lens (envConfig)
                    (\s t -> s {envConfig = t})

buildOptsMonoidHaddock :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidHaddock = lens (buildMonoidHaddock)
                            (\buildMonoid t -> buildMonoid {buildMonoidHaddock = t})

buildOptsMonoidTests :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidTests = lens (buildMonoidTests)
                            (\buildMonoid t -> buildMonoid {buildMonoidTests = t})

buildOptsMonoidBenchmarks :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidBenchmarks = lens (buildMonoidBenchmarks)
                            (\buildMonoid t -> buildMonoid {buildMonoidBenchmarks = t})

buildOptsMonoidInstallExes :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidInstallExes =
  lens (buildMonoidInstallExes)
       (\buildMonoid t -> buildMonoid {buildMonoidInstallExes = t})

buildOptsInstallExes :: Lens' BuildOpts Bool
buildOptsInstallExes =
  lens (boptsInstallExes)
       (\bopts t -> bopts {boptsInstallExes = t})

envConfigBuildOpts :: Lens' EnvConfig BuildOpts
envConfigBuildOpts =
    lens
        (\envCfg -> configBuild (bcConfig (envConfigBuildConfig envCfg)))
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
        (\globalOpts ->
              configMonoidBuildOpts
                  (globalConfigMonoid globalOpts))
        (\globalOpts boptsMonoid ->
              globalOpts
              { globalConfigMonoid = (globalConfigMonoid globalOpts)
                { configMonoidBuildOpts = boptsMonoid
                }
              })
