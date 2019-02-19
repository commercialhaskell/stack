{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stack.Lock where

import qualified Data.Yaml as Yaml
import Data.Yaml (object)
import Pantry (resolveSnapshotFile)
import Path (addFileExtension, fromAbsFile, parent)
import Path.IO (doesFileExist, getModificationTime)
import qualified RIO.ByteString as B
import Stack.Prelude
import Stack.Types.Config

data LockException
    = LockNoProject
    | LockCannotGenerate SnapshotLocation
    deriving (Typeable)

instance Exception LockException

instance Show LockException where
    show LockNoProject = "No project found for locking."
    show (LockCannotGenerate e) =
        "Lock file cannot be generated for snapshot: " <> (show e)

generateLockFile :: Path Abs File -> RIO Config ()
generateLockFile stackFile = do
    logDebug "Gennerating lock file"
    mproject <- view $ configL . to configMaybeProject
    p <-
        case mproject of
            Just (p, _) -> return p
            Nothing -> throwM LockNoProject
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

loadSnapshotFile :: Path Abs File -> Path Abs Dir -> IO [RawPackageLocation]
loadSnapshotFile path rootDir = do
    val <- Yaml.decodeFileThrow (toFilePath path)
    case Yaml.parseEither (resolveSnapshotFile rootDir) val of
        Left str -> fail $ "Cannot parse snapshot file: Got error " <> str
        Right rplio -> rplio

loadSnapshotLockFile ::
       Path Abs File -- ^ Snapshot lock file
    -> IO [(RawPackageLocation, PackageLocation)]
loadSnapshotLockFile = undefined

generateSnapshotLockFile ::
       Path Abs File -- ^ Snapshot file
    -> [RawPackageLocation]
    -> RIO Config ()
generateSnapshotLockFile path rpl = do
    logInfo "Generating Lock file for snapshot"
    deps :: [PackageLocation] <- mapM completePackageLocation' rpl
    lockFile <- liftIO $ addFileExtension "lock" path
    let depPairs :: [(PackageLocation, RawPackageLocation)] = zip deps rpl
        depsObject =
            Yaml.object
                [ ( "dependencies"
                  , Yaml.array
                        (map (\(raw, comp) ->
                                  object
                                      [ ("original", Yaml.toJSON raw)
                                      , ("complete", Yaml.toJSON comp)
                                      ])
                             depPairs))
                ]
    B.writeFile (fromAbsFile lockFile) (Yaml.encode depsObject)

-- Things to do
-- 1. Creae function to write custom snapshot lock file. something like fn :: [RawPackageLocation] -> Path Abs File (snapshot file) -> IO ()
-- 2. Create function to load data from custom snapshot file. Something like fn :: Path Abs File -> IO [(PackageLocation, RawPackageLocation)]
-- 3. Try to use the loaded data in the loadBuildconfig function
generateLockFileForCustomSnapshot ::
       SnapshotLocation -> Path Abs File -> RIO Config ()
generateLockFileForCustomSnapshot (SLFilePath path) stackFile
    -- todo: see if there is existing and outdated file
 = do
    let snapshotPath = resolvedAbsolute path
    rpl <- liftIO $ loadSnapshotFile snapshotPath (parent stackFile)
    generateSnapshotLockFile snapshotPath rpl
generateLockFileForCustomSnapshot xs _ = throwM (LockCannotGenerate xs)

-- hasLockFile :: HasEnvConfig env => RIO env Bool
-- hasLockFile = do
--     bconfig <- view $ envConfigL . to envConfigBuildConfig
--     let stackFile = bcStackYaml bconfig
--     lockFile <- liftIO $ addFileExtension "lock" stackFile
--     liftIO $ doesFileExist lockFile
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
