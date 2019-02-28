{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stack.Lock where

import Data.List ((\\), intersect)
import qualified Data.Yaml as Yaml
import Data.Yaml (object)
import Pantry (loadLockFile, resolveSnapshotFile)
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

-- You need to keep track of the following things
-- Has resolver changed.
--  * If yes, then to what value it has changed. Both from and to has to be printed.
-- Has extra-deps changed
--  * Can be (added/changed/removed). You need to indicate them.
--  * Keep track of lockfile package and current stack.yaml [RawPackageLocation]
data Change = Change
    { chAdded :: [RawPackageLocation]
    , chRemoved :: [RawPackageLocation]
    , chUnchanged :: [(PackageLocation, RawPackageLocation)]
    }

findChange ::
       [(PackageLocation, RawPackageLocation)] -- ^ Lock file
    -> [RawPackageLocation] -- ^ stack.yaml file
    -> Change
findChange lrpl srpl =
    let lr = map snd lrpl
        unchangedOnes = intersect lr srpl
        unchangedFull = filter (\(pl, rpl) -> rpl `elem` srpl) lrpl
     in Change
            { chAdded = srpl \\ unchangedOnes
            , chRemoved = lr \\ unchangedOnes
            , chUnchanged = unchangedFull
            }

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
    lockFileExists <- liftIO $ doesFileExist lockFile
    lockInfo <-
        case lockFileExists of
            True ->
                liftIO $ do
                    lfio <- loadLockFile lockFile
                    let lfpl = lfPackageLocation lfio
                        lfor = lfoResolver lfio
                        lfcr = lfcResolver lfio
                    pl <- lfpl
                    or <- lfor
                    cr <- lfcr
                    pure $ Just (pl, or, cr)
            False -> pure Nothing
    (deps', resolver') <-
        case lockInfo of
            Just (pl, or, cr) -> do
                let change = findChange pl deps
                    unchangedRes = map fst (chUnchanged change)
                    addedStr =
                        concat $
                        map
                            (\x ->
                                 "Adding " <> (show x) <>
                                 " package to the lock file.\n")
                            (chAdded change)
                    deletedstr =
                        concat $
                        map
                            (\x ->
                                 "Removing " <> (show x) <>
                                 " package from the lock file.\n")
                            (chRemoved change)
                logInfo (displayShow $ addedStr <> deletedstr)
                deps <- mapM completePackageLocation' (chAdded change)
                let allDeps = unchangedRes <> deps
                res <-
                    if or == resolver
                        then pure cr
                        else completeSnapshotLocation resolver
                pure (allDeps, res)
            Nothing -> do
                resolver' :: SnapshotLocation <-
                    completeSnapshotLocation resolver
                deps' :: [PackageLocation] <- mapM completePackageLocation' deps
                pure (deps', resolver')
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

loadSnapshotFile ::
       Path Abs File
    -> Path Abs Dir
    -> IO ([RawPackageLocationImmutable], RawSnapshotLocation)
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
    -> [RawPackageLocationImmutable]
    -> RawSnapshotLocation
    -> RIO Config ()
generateSnapshotLockFile path rpli rpl = do
    logInfo "Generating Lock file for snapshot"
    let rpli' :: [RawPackageLocation] = map RPLImmutable rpli
    deps :: [PackageLocation] <- mapM completePackageLocation' rpli'
    rpl' :: SnapshotLocation <- completeSnapshotLocation rpl
    lockFile <- liftIO $ addFileExtension "lock" path
    let depPairs :: [(PackageLocation, RawPackageLocation)] = zip deps rpli'
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
                , ( "resolver"
                  , object
                        [ ("original", Yaml.toJSON rpl)
                        , ("complete", Yaml.toJSON rpl')
                        ])
                ]
    B.writeFile (fromAbsFile lockFile) (Yaml.encode depsObject)

generateLockFileForCustomSnapshot ::
       SnapshotLocation -> Path Abs File -> RIO Config ()
generateLockFileForCustomSnapshot (SLFilePath path) stackFile = do
    logInfo "Generating lock file for custom snapshot"
    let snapshotPath = resolvedAbsolute path
    (rpli, rpl) <- liftIO $ loadSnapshotFile snapshotPath (parent stackFile)
    generateSnapshotLockFile snapshotPath rpli rpl
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
