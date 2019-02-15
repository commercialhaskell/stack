{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stack.Freeze
    ( freeze
    , FreezeOpts(..)
    , FreezeMode(..)
    ) where

import Data.Aeson ((.=), object)
import qualified Data.List.NonEmpty as NE
import qualified Data.Yaml as Yaml
import Path (addFileExtension, fromAbsFile, parent, toFilePath)
import Path.IO (doesFileExist, getModificationTime)
import qualified RIO.ByteString as B
import RIO.Process
import Stack.Config (loadConfigYaml)
import Stack.Prelude
import Stack.Types.Config

data FreezeMode
    = FreezeProject
    | FreezeSnapshot

newtype FreezeOpts = FreezeOpts
    { freezeMode :: FreezeMode
    }

freeze :: HasEnvConfig env => FreezeOpts -> RIO env ()
freeze (FreezeOpts mode) = do
    mproject <- view $ configL . to configMaybeProject
    case mproject of
        Just (p, _) -> doFreeze p mode
        Nothing -> logWarn "No project was found: nothing to freeze"

doFreeze ::
       ( HasProcessContext env
       , HasLogFunc env
       , HasPantryConfig env
       , HasEnvConfig env
       )
    => Project
    -> FreezeMode
    -> RIO env ()
doFreeze p FreezeProject = do
    let deps :: [RawPackageLocation] = projectDependencies p
        resolver :: RawSnapshotLocation = projectResolver p
    resolver' :: SnapshotLocation <- completeSnapshotLocation resolver
    deps' :: [PackageLocation] <- mapM undefined deps
    let rawCompleted = map toRawPL deps'
        rawResolver = toRawSL resolver'
    if rawCompleted == deps && rawResolver == resolver
        then logInfo "No freezing is required for this project"
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
                    B.putStr $
                        Yaml.encode $ object ["extra-deps" .= rawCompleted]
doFreeze p FreezeSnapshot = do
    resolver <- completeSnapshotLocation $ projectResolver p
    result <- loadSnapshotLayer resolver
    case result of
        Left _wc -> logInfo "No freezing is required for compiler resolver"
        Right (snap, _) -> do
            snap' <- completeSnapshotLayer snap
            let rawCompleted = toRawSnapshotLayer snap'
            if rawCompleted == snap
                then logInfo
                         "No freezing is required for the snapshot of this project"
                else liftIO $ B.putStr $ Yaml.encode snap'
