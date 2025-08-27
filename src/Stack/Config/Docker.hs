{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Stack.Config.Docker
Description : Docker configuration.
License     : BSD-3-Clause

Docker configuration.
-}

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
import           Stack.Types.Snapshot ( AbstractSnapshot (..) )
import           Stack.Types.Version ( IntersectingVersionRange (..) )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Config.Docker" module.
data ConfigDockerException
  = SnapshotNotSupportedException !(Maybe Project) !(Maybe AbstractSnapshot)
  -- ^ Only LTS snapshots are supported for default image tag.
  deriving Show

instance Exception ConfigDockerException where
  displayException (SnapshotNotSupportedException mproject mASnapshot) =
    concat
      [ "Error: [S-8575]\n"
      , "Snapshot resolver not supported for Docker images:\n    "
      , case (mproject, mASnapshot) of
          (Nothing, Nothing) -> "no snapshot specified"
          (_, Just aSnapshot) ->
            T.unpack $ utf8BuilderToText $ display aSnapshot
          (Just project, Nothing) ->
            T.unpack $ utf8BuilderToText $ display project.snapshot
      , "\nUse an LTS snapshot, or set the '"
      , T.unpack dockerImageArgName
      , "' explicitly, in your configuration file."]

-- | Add a default Docker tag name to a given base image.
addDefaultTag ::
     MonadThrow m
  => String -- ^ base
  -> Maybe Project
  -> Maybe AbstractSnapshot
  -> m String
addDefaultTag base mproject mASnapshot = do
  let exc = throwM $ SnapshotNotSupportedException mproject mASnapshot
  lts <- case mASnapshot of
    Just (ASSnapshot (RSLSynonym lts@(LTS _ _))) -> pure lts
    Just _aSnapshot -> exc
    Nothing ->
      case (.snapshot) <$> mproject of
        Just (RSLSynonym lts@(LTS _ _)) -> pure lts
        _ -> exc
  pure $ base ++ ":" ++ show lts

-- | Interprets DockerOptsMonoid options.
dockerOptsFromMonoid ::
     MonadThrow m
  => Maybe Project
  -> Maybe AbstractSnapshot
  -> DockerOptsMonoid
  -> m DockerOpts
dockerOptsFromMonoid mproject mASnapshot dockerMonoid = do
  let image =
        case getFirst dockerMonoid.repoOrImage of
          Nothing -> addDefaultTag "fpco/stack-build" mproject mASnapshot
          Just (DockerMonoidImage image') -> pure image'
          Just (DockerMonoidRepo repo) ->
            case find (`elem` (":@" :: String)) repo of
              Nothing -> addDefaultTag repo mproject mASnapshot
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
          dockerMonoid.requireDockerVersion.intersectingVersionRange
      stackExe = getFirst dockerMonoid.stackExe
  pure DockerOpts
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
