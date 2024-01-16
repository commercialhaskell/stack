{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

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
            T.unpack $ utf8BuilderToText $ display project.resolver
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
      case (.resolver) <$> mproject of
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
  let image =
        case getFirst dockerMonoid.repoOrImage of
          Nothing -> addDefaultTag "fpco/stack-build" mproject maresolver
          Just (DockerMonoidImage image') -> pure image'
          Just (DockerMonoidRepo repo) ->
            case find (`elem` (":@" :: String)) repo of
              Nothing -> addDefaultTag repo mproject maresolver
              -- Repo already specified a tag or digest, so don't append default
              Just _ -> pure repo
  let enable =
        fromFirst
          (getAny dockerMonoid.defaultEnable)
          dockerMonoid.enable
      registryLogin =
        fromFirst
          (isJust (emptyToNothing (getFirst dockerMonoid.registryUsername)))
          dockerMonoid.registryLogin
      registryUsername =
        emptyToNothing (getFirst dockerMonoid.registryUsername)
      registryPassword =
        emptyToNothing (getFirst dockerMonoid.registryPassword)
      autoPull = fromFirstTrue dockerMonoid.autoPull
      detach = fromFirstFalse dockerMonoid.detach
      persist = fromFirstFalse dockerMonoid.persist
      containerName =
        emptyToNothing (getFirst dockerMonoid.containerName)
      network = emptyToNothing (getFirst dockerMonoid.network)
      runArgs = dockerMonoid.runArgs
      mount = dockerMonoid.mount
      mountMode =
        emptyToNothing (getFirst dockerMonoid.mountMode)
      env = dockerMonoid.env
      setUser = getFirst dockerMonoid.setUser
      requireDockerVersion =
        simplifyVersionRange
          dockerMonoid.requireDockerVersion.getIntersectingVersionRange
      stackExe = getFirst dockerMonoid.stackExe
  pure $ DockerOpts
    { enable
    , image
    , registryLogin
    , registryUsername
    , registryPassword
    , autoPull
    , detach
    , persist
    , containerName
    , network
    , runArgs
    , mount
    , mountMode
    , env
    , stackExe
    , setUser
    , requireDockerVersion
    }
 where
  emptyToNothing Nothing = Nothing
  emptyToNothing (Just s)
    | null s = Nothing
    | otherwise = Just s
