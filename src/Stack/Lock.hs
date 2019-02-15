{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stack.Lock where

import Data.Aeson ((.=), object)
import qualified Data.List.NonEmpty as NE
import qualified Data.Yaml as Yaml
import Pantry (completePackageLocation)
import Path (addFileExtension, fromAbsFile, parent, toFilePath)
import Path.IO (doesFileExist, getModificationTime)
import qualified RIO.ByteString as B
import RIO.Process
import Stack.Prelude
import Stack.Types.Config

-- BuildConfig is in Types/Config.hs
generateLockFile :: Path Abs File -> RIO Config ()
generateLockFile stackFile = do
    logDebug "Gennerating lock file"
    mproject <- view $ configL . to configMaybeProject
    p <-
        case mproject of
            Just (p, _) -> return p
            Nothing -> error "No project was found: nothing to freeze" -- todo
    let deps :: [RawPackageLocation] = projectDependencies p
        resolver :: RawSnapshotLocation = projectResolver p
    lockFile <- liftIO $ addFileExtension "lock" stackFile
    resolver' :: SnapshotLocation <- completeSnapshotLocation resolver
    deps' :: [PackageLocation] <- mapM completePackageLocation' deps
    let deps'' = zip deps deps'
    let depsObject =
            Yaml.object
                [ ( "resolver"
                  , object
                        [ ("original", Yaml.toJSON resolver)
                        , ("complete", Yaml.toJSON resolver')
                        ])
                , ( "dependencies"
                  , Yaml.array
                        (map (\(raw, comp) ->
                                  object
                                      [ ("original", Yaml.toJSON raw)
                                      , ("complete", Yaml.toJSON comp)
                                      ])
                             deps''))
                ]
    B.writeFile (fromAbsFile lockFile) (Yaml.encode depsObject)

hasLockFile :: HasEnvConfig env => BuildConfig -> RIO env Bool
hasLockFile bconfig = do
    let stackFile = bcStackYaml bconfig
    lockFile <- liftIO $ addFileExtension "lock" stackFile
    liftIO $ doesFileExist lockFile

-- parsePLI :: HasEnvConfig env => BuildConfig -> RIO env [PackageLocation]
-- parsePLI bconfig = do
--     let stackFile = bcStackYaml bconfig
--         rootDir = parent stackFile
--     lockFile <- liftIO $ addFileExtension "lock" stackFile
--     (pli :: Yaml.Value) <- Yaml.decodeFileThrow (toFilePath lockFile)
--     plis <- Yaml.parseMonad (parseLockFile rootDir) pli
--     plis' <- liftIO $ plis
--     pure $ NE.toList plis'
isLockFileOutdated :: Path Abs File -> RIO Config Bool
isLockFileOutdated stackFile = do
    lockFile <- liftIO $ addFileExtension "lock" stackFile
    smt <- liftIO $ getModificationTime stackFile
    lmt <-
        liftIO $ do
            exists <- doesFileExist lockFile
            if exists
                then do
                    mt <- getModificationTime lockFile
                    pure $ Just mt
                else pure Nothing
    case lmt of
        Nothing -> return True
        Just mt -> return $ smt > mt
-- lockfile modificaton time < stackfile modification time
-- Use loadProjectConfig and parseLockfile to see if lock file has been outdated
