{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Docker types.

module Stack.Types.Docker where

import Control.Applicative
import Data.Aeson
import Data.Text (Text)

-- | Docker configuration.
data Docker =
  Docker {dockerEnable :: !Bool
           -- ^ Is using Docker enabled?
         ,dockerRepoOwner :: !String
           -- ^ Docker repository (registry and) owner
         ,dockerRepo :: !String
           -- ^ Docker repository name (e.g. @dev@)
         ,dockerRepoSuffix :: !String
           -- ^ Docker repository name's suffix (e.g. stackage ver)
         ,dockerImageTag :: !(Maybe String)
           -- ^ Optional Docker image tag (e.g. the date)
         ,dockerImage :: !(Maybe String)
           -- ^ Exact Docker image tag or ID.  Overrides docker-repo-*/tag.
         ,dockerRegistryLogin :: !Bool
           -- ^ Does registry require login for pulls?
         ,dockerRegistryUsername :: !(Maybe String)
           -- ^ Optional username for Docker registry.
         ,dockerRegistryPassword :: !(Maybe String)
           -- ^ Optional password for Docker registry.
         ,dockerAutoPull :: !Bool
           -- ^ Automatically pull new images.
         ,dockerDetach :: !Bool
           -- ^ Whether to run a detached container
         ,dockerPersist :: !Bool
           -- ^ Create a persistent container (don't remove it when finished).  Implied by
           -- `dockerDetach`.
         ,dockerContainerName :: !(Maybe String)
           -- ^ Container name to use, only makes sense from command-line with `dockerPersist`
           -- or `dockerDetach`.
         ,dockerRunArgsDefault :: ![String]
           -- ^ Arguments to pass directly to @docker run@ (from @stackage-build.config@).
         ,dockerRunArgsExtra :: ![[String]]
           -- ^ Arguments to pass directly to @docker run@ (passed on stackage-build command-line).
         ,dockerMountDefault :: ![Mount]
           -- ^ Volumes to mount in the container (from @stackage-build.config@).
         ,dockerMountExtra :: ![Mount]
           -- ^ Volumes to mount in the container (from stackage-build command-line).
         ,dockerPassHost :: !Bool
           -- ^ Pass Docker daemon connection information into container.
         ,dockerExtra :: ![String]
           -- ^ This is a placeholder for command-line argument parsing.
         }
  deriving (Show)

-- | For YAML.
instance FromJSON Docker where
  parseJSON = withObject "Docker" $ \o ->
            Docker <$> o .:? dockerEnableArgName .!= True
                   <*> o .:? dockerRepoOwnerArgName .!= dockerRepoOwner defaultDocker
                   <*> o .:? dockerRepoArgName .!= dockerRepo defaultDocker
                   <*> o .:? dockerRepoSuffixArgName .!= dockerRepoSuffix defaultDocker
                   <*> o .:? dockerImageTagArgName .!= dockerImageTag defaultDocker
                   <*> o .:? dockerImageArgName
                   <*> o .:? dockerRegistryLoginArgName .!= dockerRegistryLogin defaultDocker
                   <*> o .:? dockerRegistryUsernameArgName .!= dockerRegistryUsername defaultDocker
                   <*> o .:? dockerRegistryPasswordArgName .!= dockerRegistryPassword defaultDocker
                   <*> o .:? dockerAutoPullArgName .!= dockerAutoPull defaultDocker
                   <*> o .:? dockerDetachArgName .!= dockerDetach defaultDocker
                   <*> o .:? dockerPersistArgName .!= dockerPersist defaultDocker
                   <*> o .:? dockerContainerNameArgName .!= dockerContainerName defaultDocker
                   <*> o .:? dockerRunArgsArgName .!= dockerRunArgsDefault defaultDocker
                   <*> pure (dockerRunArgsExtra defaultDocker)
                   <*> o .:? dockerMountArgName .!= dockerMountDefault defaultDocker
                   <*> pure (dockerMountExtra defaultDocker)
                   <*> o .:? dockerPassHostArgName .!= dockerPassHost defaultDocker
                   <*> pure (dockerExtra defaultDocker)

-- | Default values for Docker configuration.
defaultDocker :: Docker
defaultDocker =
        Docker {dockerEnable = False
               ,dockerRepoOwner = "fpco"
               ,dockerRepo = "dev"
               ,dockerRepoSuffix = ""
               ,dockerImageTag = Nothing
               ,dockerImage = Nothing
               ,dockerRegistryLogin = False
               ,dockerRegistryUsername = Nothing
               ,dockerRegistryPassword = Nothing
               ,dockerAutoPull = True
               ,dockerDetach = False
               ,dockerPersist = False
               ,dockerContainerName = Nothing
               ,dockerRunArgsDefault = []
               ,dockerRunArgsExtra = []
               ,dockerMountDefault = []
               ,dockerMountExtra = []
               ,dockerPassHost = False
               ,dockerExtra = []
               }

dockerEnableArgName :: Text
dockerEnableArgName = "enable"

dockerRepoOwnerArgName :: Text
dockerRepoOwnerArgName = "repo-owner"

dockerRepoArgName :: Text
dockerRepoArgName = "repo"

dockerRepoSuffixArgName :: Text
dockerRepoSuffixArgName = "repo-suffix"

dockerImageTagArgName :: Text
dockerImageTagArgName = "image-tag"

dockerImageArgName :: Text
dockerImageArgName = "image"

dockerRegistryLoginArgName :: Text
dockerRegistryLoginArgName = "registry-login"

dockerRegistryUsernameArgName :: Text
dockerRegistryUsernameArgName = "registry-username"

dockerRegistryPasswordArgName :: Text
dockerRegistryPasswordArgName = "registry-password"

dockerAutoPullArgName :: Text
dockerAutoPullArgName = "auto-pull"

dockerDetachArgName :: Text
dockerDetachArgName = "detach"

dockerRunArgsArgName :: Text
dockerRunArgsArgName = "run-args"

dockerMountArgName :: Text
dockerMountArgName = "mount"

dockerContainerNameArgName :: Text
dockerContainerNameArgName = "container-name"

dockerPersistArgName :: Text
dockerPersistArgName = "persist"

dockerPassHostArgName :: Text
dockerPassHostArgName = "pass-host"

-- | Docker volume mount.
data Mount = Mount String String

-- | For optparse-applicative.
instance Read Mount where
  readsPrec _ s = case break (== ':') s of
                    (a,(':':b)) -> [(Mount a b,"")]
                    (a,[]) -> [(Mount a a,"")]
                    _ -> fail "Invalid value for mount"

-- | Show instance.
instance Show Mount where
  show (Mount a b) = if a == b
                        then a
                        else concat [a,":",b]

-- | For YAML.
instance FromJSON Mount where
  parseJSON v = fmap read (parseJSON v)
