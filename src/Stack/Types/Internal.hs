{-# LANGUAGE Rank2Types #-}

-- | Internal types to the library.

module Stack.Types.Internal
    ( Env (..)
    , HasTerminal (..)
    , HasReExec (..)
    , Sticky (..)
    , HasSticky (..)
    , LogOptions (..)
    , HasLogOptions (..)
    , view
    ) where

import Control.Concurrent.MVar
import Control.Monad.Logger (LogLevel)
import Data.Text (Text)
import Lens.Micro
import Stack.Types.Config

-- | Monadic environment.
data Env config =
  Env {envConfig :: !config
      ,envReExec :: !Bool
      ,envLogOptions :: !LogOptions
      ,envTerminal :: !Bool
      ,envSticky :: !Sticky
      }

envConfL :: Lens (Env a) (Env b) a b
envConfL = lens envConfig (\x y -> x { envConfig = y })

instance HasPlatform config => HasPlatform (Env config) where
    platformL = envConfL.platformL
    platformVariantL = envConfL.platformVariantL
instance HasGHCVariant config => HasGHCVariant (Env config) where
    ghcVariantL = envConfL.ghcVariantL
instance HasConfig config => HasConfig (Env config) where
    configL = envConfL.configL
instance HasBuildConfigNoLocal config => HasBuildConfigNoLocal (Env config) where
    buildConfigNoLocalL = envConfL.buildConfigNoLocalL
instance HasBuildConfig config => HasBuildConfig (Env config) where
    buildConfigLocalL = envConfL.buildConfigLocalL
instance HasEnvConfigNoLocal config => HasEnvConfigNoLocal (Env config) where
    envConfigNoLocalL = envConfL.envConfigNoLocalL
instance HasEnvConfig config => HasEnvConfig (Env config) where
    envConfigL = envConfL.envConfigL

class HasTerminal env where
  terminalL :: Lens' env Bool

instance HasTerminal (Env config) where
  terminalL = lens envTerminal (\x y -> x { envTerminal = y })

class HasReExec env where
  reExecL :: Lens' env Bool

instance HasReExec (Env config) where
  reExecL = lens envReExec (\x y -> x { envReExec = y })

newtype Sticky = Sticky
  { unSticky :: Maybe (MVar (Maybe Text))
  }

class HasSticky env where
  stickyL :: Lens' env Sticky

instance HasSticky (Env config) where
  stickyL = lens envSticky (\x y -> x { envSticky = y })

data LogOptions = LogOptions
  { logUseColor :: Bool
  , logUseUnicode :: Bool
  , logUseTime :: Bool
  , logMinLevel :: LogLevel
  , logVerboseFormat :: Bool
  }

class HasLogOptions env where
  logOptionsL :: Lens' env LogOptions

instance HasLogOptions (Env config) where
  logOptionsL = lens envLogOptions (\x y -> x { envLogOptions = y })
