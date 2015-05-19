{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The Config type.

module Stack.Types.Config where

import Control.Exception
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable
import Path
import Stack.Types.Docker
import Stack.Types.FlagName
import Stack.Types.PackageName

-- | The top-level Stackage configuration.
data Config =
  Config {configPkgDbLocation    :: !(Path Abs Dir)
         ,configGhcBinLocation   :: !(Path Abs Dir)
         ,configCabalBinLocation :: !(Path Abs Dir)
         ,configStackRoot        :: !(Path Abs Dir)
         ,configBuildIn          :: !Text
         ,configDocker           :: !(Maybe Docker)
         ,configPackages         :: !(Set (Path Abs Dir))
         ,configFlags            :: !(Map FlagName Bool)
         ,configPackageFlags     :: !(Map PackageName (Map FlagName Bool))
         ,configDir              :: !(Path Abs Dir)
         ,configUrls             :: !(Map Text Text)
         ,configGpgVerifyIndex   :: !Bool
         }
  -- ^ Flags for each package's Cabal config.
  deriving (Show)

-- | Class for environment values that can provide a 'Config'.
class HasConfig env where
    getConfig :: env -> Config
instance HasConfig Config where
    getConfig = id
    {-# INLINE getConfig #-}

configInDocker :: Config -> Bool
configInDocker = (== "docker") . configBuildIn

data ConfigException
  = ConfigInvalidYaml String
  | ConfigNoFile
  | ConfigNoDockerConfig
  deriving (Typeable,Show)
instance Exception ConfigException

-- TODO: eliminate occurrences of this exception.
data NotYetImplemented = NotYetImplemented Text
  deriving (Show, Typeable)
instance Exception NotYetImplemented
