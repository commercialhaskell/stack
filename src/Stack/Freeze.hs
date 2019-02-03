{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stack.Freeze
    ( freeze
    , FreezeOpts (..)
    , FreezeMode (..)
    , hasLockFile
    , isLockFileOutdated
    ) where

import qualified Prelude as Prelude
import           Data.Aeson ((.=), object)
import qualified Data.Yaml as Yaml
import           RIO.Process
import qualified RIO.ByteString as B
import           Stack.Prelude
import           Stack.Types.Config
import Stack.Config (loadConfigYaml)
import Path (addFileExtension, parent, fromAbsFile, toFilePath)
import Path.IO (doesFileExist)
import qualified Data.List.NonEmpty as NE

data FreezeMode = FreezeProject | FreezeSnapshot

newtype FreezeOpts = FreezeOpts
    { freezeMode :: FreezeMode
    }

freeze :: HasEnvConfig env => FreezeOpts -> RIO env ()
freeze (FreezeOpts mode) = do
  mproject <- view $ configL.to configMaybeProject
  case mproject of
    Just (p, _) -> doFreeze p mode
    Nothing -> logWarn "No project was found: nothing to freeze"

completePackageLocation' :: (HasProcessContext env, HasLogFunc env, HasPantryConfig env, HasEnvConfig env) => RawPackageLocation -> RIO env PackageLocation
completePackageLocation' pl =
    case pl of
      RPLImmutable pli -> PLImmutable <$> completePackageLocation pli
      RPLMutable m -> pure $ PLMutable m

doFreeze ::
       (HasProcessContext env, HasLogFunc env, HasPantryConfig env, HasEnvConfig env)
    => Project
    -> FreezeMode
    -> RIO env ()
doFreeze p FreezeProject = do
  envConfig <- view envConfigL
  let bconfig = envConfigBuildConfig envConfig
  generateLockFile bconfig
  isLockFileOutdated bconfig -- todo: remove this in future (just for testing)
  let deps :: [RawPackageLocation] = projectDependencies p
      resolver :: RawSnapshotLocation = projectResolver p
  resolver' :: SnapshotLocation <- completeSnapshotLocation resolver
  deps' :: [PackageLocation] <- mapM completePackageLocation' deps
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
    Right (snap, _) -> do
      snap' <- completeSnapshotLayer snap
      let rawCompleted = toRawSnapshotLayer snap'
      if rawCompleted == snap
        then
        logInfo "No freezing is required for the snapshot of this project"
        else
        liftIO $ B.putStr $ Yaml.encode snap'

-- BuildConfig is in Types/Config.hs
generateLockFile :: HasEnvConfig env => BuildConfig -> RIO env ()
generateLockFile bconfig = do
  let stackFile = bcStackYaml bconfig
  lockFile <- liftIO $ addFileExtension "lock" stackFile
  iosc <- loadConfigYaml (parseStackYamlConfig (parent stackFile)) stackFile
  StackYamlConfig deps resolver <- liftIO iosc
  resolver' :: SnapshotLocation <- completeSnapshotLocation resolver
  deps' :: [PackageLocation] <- mapM completePackageLocation' deps
  let deps'' = map toRawPL deps'
  let depsObject = Yaml.object [
                            ("resolver",
                                       Yaml.array
                                           [
                                            object [("original", Yaml.toJSON resolver)],
                                            object [("complete", Yaml.toJSON resolver')]
                                           ]
                            ),
                            ("dependencies",
                                           Yaml.array
                                                   [
                                                    object [("original", Yaml.toJSON deps)],
                                                    object [("complete", Yaml.toJSON deps'')]
                                                   ]
                            )]
  B.writeFile (fromAbsFile lockFile) (Yaml.encode depsObject)


hasLockFile :: HasEnvConfig env => BuildConfig -> RIO env Bool
hasLockFile bconfig = do
  let stackFile = bcStackYaml bconfig
  lockFile <- liftIO $ addFileExtension "lock" stackFile
  liftIO $ doesFileExist lockFile

parsePLI :: HasEnvConfig env => BuildConfig -> RIO env [PackageLocation]
parsePLI bconfig = do
  let stackFile = bcStackYaml bconfig
      rootDir = parent stackFile
  lockFile <- liftIO $ addFileExtension "lock" stackFile
  (pli :: Yaml.Value) <- Yaml.decodeFileThrow (toFilePath lockFile)
  plis <- Yaml.parseMonad (parseLockFile rootDir) pli
  plis' <- liftIO $ plis
  pure $ NE.toList plis'

-- Parse the orignial stack file and then parse the lock file and then compare it with  the original file content
isLockFileOutdated :: HasEnvConfig env => BuildConfig -> RIO env Bool
isLockFileOutdated bconfig = do
  let stackFile = bcStackYaml bconfig
  plis <- parsePLI bconfig
  lockFile <- liftIO $ addFileExtension "lock" stackFile
  iosc <- loadConfigYaml (parseStackYamlConfig (parent stackFile)) stackFile
  StackYamlConfig deps resolver <- liftIO iosc
  resolver' :: SnapshotLocation <- completeSnapshotLocation resolver
  deps' :: [PackageLocation] <- mapM completePackageLocation' deps
  let deps'' = map toRawPL deps'
      rawResolver = toRawSL resolver'
      isUpdated = (deps'' == deps) && (resolver == rawResolver)
  liftIO $ Prelude.print "raw PLI"
  liftIO $ Prelude.print deps
  liftIO $ Prelude.print "raw PLI from complete"
  liftIO $ Prelude.print plis
  pure $ not isUpdated


-- Use loadProjectConfig and parseLockfile to see if lock file has been outdated
