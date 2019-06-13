{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Freeze
    ( freeze
    , FreezeOpts (..)
    , FreezeMode (..)
    ) where

import           Data.Aeson ((.=), object)
import qualified Data.Yaml as Yaml
import           RIO.Process
import qualified RIO.ByteString as B
import           Stack.Prelude
import           Stack.Types.Config

data FreezeMode = FreezeProject | FreezeSnapshot

newtype FreezeOpts = FreezeOpts
    { freezeMode :: FreezeMode
    }

freeze :: HasEnvConfig env => FreezeOpts -> RIO env ()
freeze (FreezeOpts mode) = do
  mproject <- view $ configL.to configProject
  let warn = logWarn "No project was found: nothing to freeze"
  case mproject of
    PCProject (p, _) -> doFreeze p mode
    PCGlobalProject -> warn
    PCNoProject _ -> warn

doFreeze ::
       (HasProcessContext env, HasLogFunc env, HasPantryConfig env)
    => Project
    -> FreezeMode
    -> RIO env ()
doFreeze p FreezeProject = do
  let deps = projectDependencies p
      resolver = projectResolver p
      completePackageLocation' pl =
        case pl of
          RPLImmutable pli -> PLImmutable <$> completePackageLocation pli
          RPLMutable m -> pure $ PLMutable m
  resolver' <- completeSnapshotLocation resolver
  deps' <- mapM completePackageLocation' deps
  let rawCompleted = map toRawPL deps'
      rawResolver = toRawSL resolver'
  if rawCompleted == deps && rawResolver == resolver
  then
    logInfo "No freezing is required for this project"
  else do
    logInfo "# Fields not mentioned below do not need to be updated"

    if rawResolver == resolver
      then logInfo "# No update to resolver is needed"
      else do
        logInfo "# Frozen version of resolver"
        B.putStr $ Yaml.encode $ object ["resolver" .= rawResolver]

    if rawCompleted == deps
      then logInfo "# No update to extra-deps is needed"
      else do
        logInfo "# Frozen version of extra-deps"
        B.putStr $ Yaml.encode $ object ["extra-deps" .= rawCompleted]

doFreeze p FreezeSnapshot = do
  resolver <- completeSnapshotLocation $ projectResolver p
  result <- loadSnapshotLayer resolver
  case result of
    Left _wc ->
      logInfo "No freezing is required for compiler resolver"
    Right snap -> do
      snap' <- completeSnapshotLayer snap
      let rawCompleted = toRawSnapshotLayer snap'
      if rawCompleted == snap
        then
        logInfo "No freezing is required for the snapshot of this project"
        else
        liftIO $ B.putStr $ Yaml.encode snap'
