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
                        (map (\(comp, raw) ->
                                  object
                                      [ ("original", Yaml.toJSON raw)
                                      , ("complete", Yaml.toJSON comp)
                                      ])
                             depPairs))
                ]
    B.writeFile (fromAbsFile lockFile) (Yaml.encode depsObject)

generateLockFileForCustomSnapshot ::
       SnapshotLocation -> Path Abs File -> RIO Config ()
generateLockFileForCustomSnapshot (SLFilePath path) stackFile = do
    logInfo "Generating lock file for custom snapshot"
    let snapshotPath = resolvedAbsolute path
    rpl <- liftIO $ loadSnapshotFile snapshotPath (parent stackFile)
    generateSnapshotLockFile snapshotPath rpl
generateLockFileForCustomSnapshot xs _ = throwM (LockCannotGenerate xs)

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
