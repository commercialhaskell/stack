{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Docker configuration
module Stack.Config.Docker
  ( ConfigDockerException (..)
  , addDefaultTag
  , dockerOptsFromMonoid
  ) where

import           Data.List ( find )
import qualified Data.Text as T
import           Distribution.Version ( simplifyVersionRange )
import           Stack.Prelude
import           Stack.Types.Project ( Project (..) )
import           Stack.Types.Docker
                   ( DockerOpts (..), DockerMonoidRepoOrImage (..)
                   , DockerOptsMonoid (..), dockerImageArgName
                   )
import           Stack.Types.Resolver ( AbstractResolver (..) )
import           Stack.Types.Version ( getIntersectingVersionRange )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Config.Docker" module.
data ConfigDockerException
  = ResolverNotSupportedException !(Maybe Project) !(Maybe AbstractResolver)
  -- ^ Only LTS resolvers are supported for default image tag.
  deriving (Show, Typeable)

instance Exception ConfigDockerException where
  displayException (ResolverNotSupportedException mproject maresolver) =
    concat
      [ "Error: [S-8575]\n"
      , "Resolver not supported for Docker images:\n    "
      , case (mproject, maresolver) of
          (Nothing, Nothing) -> "no resolver specified"
          (_, Just aresolver) ->
            T.unpack $ utf8BuilderToText $ display aresolver
          (Just project, Nothing) ->
            T.unpack $ utf8BuilderToText $ display $ projectResolver project
      , "\nUse an LTS resolver, or set the '"
      , T.unpack dockerImageArgName
      , "' explicitly, in your configuration file."]

-- | Add a default Docker tag name to a given base image.
addDefaultTag ::
     MonadThrow m
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
dockerOptsFromMonoid ::
     MonadThrow m
  => Maybe Project
  -> Maybe AbstractResolver
  -> DockerOptsMonoid
  -> m DockerOpts
dockerOptsFromMonoid mproject maresolver dockerMonoid = do
  let dockerImage =
        case getFirst dockerMonoid.dockerMonoidRepoOrImage of
          Nothing -> addDefaultTag "fpco/stack-build" mproject maresolver
          Just (DockerMonoidImage image) -> pure image
          Just (DockerMonoidRepo repo) ->
            case find (`elem` (":@" :: String)) repo of
              Nothing -> addDefaultTag repo mproject maresolver
              -- Repo already specified a tag or digest, so don't append default
              Just _ -> pure repo
  let dockerEnable =
        fromFirst
          (getAny dockerMonoid.dockerMonoidDefaultEnable)
          dockerMonoid.dockerMonoidEnable
      dockerRegistryLogin =
        fromFirst
          (isJust (emptyToNothing (getFirst dockerMonoid.dockerMonoidRegistryUsername)))
          dockerMonoid.dockerMonoidRegistryLogin
      dockerRegistryUsername =
        emptyToNothing (getFirst dockerMonoid.dockerMonoidRegistryUsername)
      dockerRegistryPassword =
        emptyToNothing (getFirst dockerMonoid.dockerMonoidRegistryPassword)
      dockerAutoPull = fromFirstTrue dockerMonoid.dockerMonoidAutoPull
      dockerDetach = fromFirstFalse dockerMonoid.dockerMonoidDetach
      dockerPersist = fromFirstFalse dockerMonoid.dockerMonoidPersist
      dockerContainerName =
        emptyToNothing (getFirst dockerMonoid.dockerMonoidContainerName)
      dockerNetwork = emptyToNothing (getFirst dockerMonoid.dockerMonoidNetwork)
      dockerRunArgs = dockerMonoid.dockerMonoidRunArgs
      dockerMount = dockerMonoid.dockerMonoidMount
      dockerMountMode =
        emptyToNothing (getFirst dockerMonoid.dockerMonoidMountMode)
      dockerEnv = dockerMonoid.dockerMonoidEnv
      dockerSetUser = getFirst dockerMonoid.dockerMonoidSetUser
      dockerRequireDockerVersion =
        simplifyVersionRange (getIntersectingVersionRange dockerMonoid.dockerMonoidRequireDockerVersion)
      dockerStackExe = getFirst dockerMonoid.dockerMonoidStackExe
  pure $ DockerOpts
    { dockerEnable
    , dockerImage
    , dockerRegistryLogin
    , dockerRegistryUsername
    , dockerRegistryPassword
    , dockerAutoPull
    , dockerDetach
    , dockerPersist
    , dockerContainerName
    , dockerNetwork
    , dockerRunArgs
    , dockerMount
    , dockerMountMode
    , dockerEnv
    , dockerStackExe
    , dockerSetUser
    , dockerRequireDockerVersion
    }
 where
  emptyToNothing Nothing = Nothing
  emptyToNothing (Just s)
    | null s = Nothing
    | otherwise = Just s
