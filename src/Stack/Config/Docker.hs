{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Docker configuration
module Stack.Config.Docker where

import           Stack.Prelude
import           Data.List (find)
import qualified Data.Text as T
import           Distribution.Version (simplifyVersionRange)
import           Stack.Types.Version
import           Stack.Types.Config
import           Stack.Types.Docker
import           Stack.Types.Resolver

-- | Add a default Docker tag name to a given base image.
addDefaultTag
  :: MonadThrow m
  => String -- ^ base
  -> Maybe Project
  -> Maybe AbstractResolver
  -> m String
addDefaultTag base mproject maresolver = do
  let exc = throwM $ ResolverNotSupportedException mproject maresolver
  lts <- case maresolver of
    Just (ARResolver (RSLSynonym lts@(LTS _ _))) -> pure lts
    Just _aresolver -> exc
    Nothing ->
      case projectResolver <$> mproject of
        Just (RSLSynonym lts@(LTS _ _)) -> pure lts
        _ -> exc
  pure $ base ++ ":" ++ show lts

-- | Interprets DockerOptsMonoid options.
dockerOptsFromMonoid
    :: MonadThrow m
    => Maybe Project
    -> Maybe AbstractResolver
    -> DockerOptsMonoid
    -> m DockerOpts
dockerOptsFromMonoid mproject maresolver DockerOptsMonoid{..} = do
    let dockerImage =
          case getFirst dockerMonoidRepoOrImage of
            Nothing -> addDefaultTag "fpco/stack-build" mproject maresolver
            Just (DockerMonoidImage image) -> pure image
            Just (DockerMonoidRepo repo) ->
              case find (`elem` (":@" :: String)) repo of
                Nothing -> addDefaultTag repo mproject maresolver
                -- Repo already specified a tag or digest, so don't append default
                Just _ -> pure repo
    let dockerEnable =
            fromFirst (getAny dockerMonoidDefaultEnable) dockerMonoidEnable
        dockerRegistryLogin =
            fromFirst
                (isJust (emptyToNothing (getFirst dockerMonoidRegistryUsername)))
                dockerMonoidRegistryLogin
        dockerRegistryUsername = emptyToNothing (getFirst dockerMonoidRegistryUsername)
        dockerRegistryPassword = emptyToNothing (getFirst dockerMonoidRegistryPassword)
        dockerAutoPull = fromFirstTrue dockerMonoidAutoPull
        dockerDetach = fromFirstFalse dockerMonoidDetach
        dockerPersist = fromFirstFalse dockerMonoidPersist
        dockerContainerName = emptyToNothing (getFirst dockerMonoidContainerName)
        dockerNetwork = emptyToNothing (getFirst dockerMonoidNetwork)
        dockerRunArgs = dockerMonoidRunArgs
        dockerMount = dockerMonoidMount
        dockerMountMode = emptyToNothing (getFirst dockerMonoidMountMode)
        dockerEnv = dockerMonoidEnv
        dockerSetUser = getFirst dockerMonoidSetUser
        dockerRequireDockerVersion =
            simplifyVersionRange (getIntersectingVersionRange dockerMonoidRequireDockerVersion)
        dockerStackExe = getFirst dockerMonoidStackExe

    pure DockerOpts{..}
  where emptyToNothing Nothing = Nothing
        emptyToNothing (Just s) | null s = Nothing
                                | otherwise = Just s

-- | Exceptions thrown by Stack.Docker.Config.
data StackDockerConfigException
    = ResolverNotSupportedException !(Maybe Project) !(Maybe AbstractResolver)
    -- ^ Only LTS resolvers are supported for default image tag.
    deriving (Typeable)

-- | Exception instance for StackDockerConfigException.
instance Exception StackDockerConfigException

-- | Show instance for StackDockerConfigException.
instance Show StackDockerConfigException where
    show (ResolverNotSupportedException mproject maresolver) =
        concat
            [ "Resolver not supported for Docker images:\n    "
            , case (mproject, maresolver) of
                (Nothing, Nothing) -> "no resolver specified"
                (_, Just aresolver) -> T.unpack $ utf8BuilderToText $ display aresolver
                (Just project, Nothing) -> T.unpack $ utf8BuilderToText $ display $ projectResolver project
            , "\nUse an LTS resolver, or set the '"
            , T.unpack dockerImageArgName
            , "' explicitly, in your configuration file."]
