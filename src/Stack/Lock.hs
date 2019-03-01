{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Stack.Lock where

import Data.Aeson.Extended (unWarningParser)
import Data.List ((\\), intersect)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Data.Yaml as Yaml
import Data.Yaml
import Pantry
    ( GitHubRepo(..)
    , OptionalSubdirs(..)
    , Unresolved(..)
    , completePackageLocation
    , osToRpms
    , parseArchiveLocationObject
    , parseArchiveLocationText
    , parsePackageIdentifierRevision
    , parseRawSnapshotLocation
    , parseRawSnapshotLocationPath
    , parseWantedCompiler
    , rpmEmpty
    )
import Path (addFileExtension, fromAbsFile, parent)
import Path.IO (doesFileExist, getModificationTime, resolveDir, resolveFile)
import qualified RIO.ByteString as B
import qualified RIO.HashMap as HM
import RIO.Process
import qualified RIO.Text as T
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
    { chAdded :: ![RawPackageLocation]
    , chRemoved :: ![RawPackageLocation]
    , chUnchanged :: ![(RawPackageLocation, PackageLocation)]
    }

completeFullPackageLocation ::
       (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
    => RawPackageLocation
    -> RIO env PackageLocation
completeFullPackageLocation (RPLImmutable rpli) = do
    pl <- completePackageLocation rpli
    pure $ PLImmutable pl
completeFullPackageLocation (RPLMutable rplm) = pure $ PLMutable rplm

findChange ::
       [(RawPackageLocation, PackageLocation)] -- ^ Lock file
    -> [RawPackageLocation] -- ^ stack.yaml file
    -> Change
findChange lrpl srpl =
    let lr = map fst lrpl
        unchangedOnes = intersect lr srpl
        unchangedFull = filter (\(rpl, pl) -> rpl `elem` srpl) lrpl
     in Change
            { chAdded = srpl \\ unchangedOnes
            , chRemoved = lr \\ unchangedOnes
            , chUnchanged = unchangedFull
            }

generatePackageLockFile :: Path Abs File -> RIO Config ()
generatePackageLockFile stackFile = do
    logDebug "Generating lock file"
    mproject <- view $ configL . to configMaybeProject
    p <-
        case mproject of
            Just (p, _) -> return p
            Nothing -> throwM LockNoProject
    let deps :: [RawPackageLocation] = projectDependencies p
        resolver :: RawSnapshotLocation = projectResolver p
    packageLockFile <- liftIO $ addFileExtension "lock" stackFile
    packageLockFileExists <- liftIO $ doesFileExist packageLockFile
    lockInfo :: Maybe LockFile <-
        case packageLockFileExists of
            True ->
                liftIO $ do
                    lfio <- loadPackageLockFile packageLockFile
                    pure $ Just lfio
            False -> pure Nothing
    (deps', resolver') <-
        case lockInfo of
            Just lockData -> do
                let change =
                        findChange
                            (Map.toList $ lfPackageLocations lockData)
                            deps
                    unchangedRes = map snd (chUnchanged change)
                    addedStr :: [Utf8Builder] =
                        map
                            (\x -> "Lock file package added: " (display x))
                            (chAdded change)
                    deletedStr :: [Utf8Builder] =
                        map
                            (\x -> "Lock file package removed: " (display x))
                            (chRemoved change)
                mapM_ logDebug addedStr
                mapM_ logDebug deletedStr
                deps <- mapM completeFullPackageLocation (chAdded change)
                let allDeps = unchangedRes <> deps
                res <-
                    if (lfoResolver lockData) == resolver
                        then pure (lfcResolver lockData)
                        else completeSnapshotLocation resolver
                pure (allDeps, res)
            Nothing -> do
                resolver' :: SnapshotLocation <-
                    completeSnapshotLocation resolver
                deps' :: [PackageLocation] <-
                    mapM completeFullPackageLocation deps
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
    B.writeFile (fromAbsFile packageLockFile) (Yaml.encode depsObject)

loadSnapshotFile ::
       Path Abs File
    -> Path Abs Dir
    -> IO ([RawPackageLocationImmutable], RawSnapshotLocation)
loadSnapshotFile path rootDir = do
    val <- Yaml.decodeFileThrow (toFilePath path)
    case Yaml.parseEither (resolveSnapshotFile rootDir) val of
        Left str -> fail $ "Cannot parse snapshot file: Got error " <> str
        Right rplio -> rplio

createSnapshotLayerLockFile ::
       Path Abs File -- ^ Snapshot file
    -> [RawPackageLocationImmutable]
    -> RawSnapshotLocation
    -> RIO Config ()
createSnapshotLayerLockFile path rpli rpl = do
    let rpli' :: [RawPackageLocation] = map RPLImmutable rpli
    deps :: [PackageLocation] <- mapM completeFullPackageLocation rpli'
    rpl' :: SnapshotLocation <- completeSnapshotLocation rpl
    snapshotLockFile <- liftIO $ addFileExtension "lock" path
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
    B.writeFile (fromAbsFile snapshotLockFile) (Yaml.encode depsObject)

generateSnapshotLayerLockFile ::
       SnapshotLocation -> Path Abs File -> RIO Config ()
generateSnapshotLayerLockFile (SLFilePath path) stackFile = do
    logInfo "Generating Lock file for custom snapshot"
    let snapshotPath = resolvedAbsolute path
    (rpli, rpl) <- liftIO $ loadSnapshotFile snapshotPath (parent stackFile)
    createSnapshotLayerLockFile snapshotPath rpli rpl
generateSnapshotLayerLockFile xs _ = throwM (LockCannotGenerate xs)

isLockFileOutdated :: Path Abs File -> RIO Config Bool
isLockFileOutdated stackFile = do
    lockFile <- liftIO $ addFileExtension "lock" stackFile
    smt <- liftIO $ getModificationTime stackFile
    liftIO $ do
        exists <- doesFileExist lockFile
        if exists
            then do
                mt <- getModificationTime lockFile
                pure $ smt > mt
            else pure True

parsePackageLockFile :: Path Abs Dir -> Value -> Parser (IO LockFile)
parsePackageLockFile rootDir value =
    withObject
        "LockFile"
        (\obj -> do
             vals :: Value <- obj .: "dependencies"
             xs :: Vector (Unresolved (RawPackageLocation, PackageLocation)) <-
                 withArray
                     "LockFileArray"
                     (\vec -> sequence $ Vector.map parseSingleObject vec)
                     vals
             resolver <- obj .: "resolver"
             roriginal <- resolver .: "original"
             rcomplete <- resolver .: "complete"
             ro <- parseRSL roriginal
             rc <- parseSL rcomplete
             let rpaths = resolvePaths (Just rootDir)
             pure $ do
                 lfpls <- rpaths $ sequence $ Vector.toList xs
                 lfor <- rpaths ro
                 lfcr <- rpaths rc
                 pure $
                     LockFile
                         { lfPackageLocations = Map.fromList lfpls
                         , lfoResolver = lfor
                         , lfcResolver = lfcr
                         })
        value

loadPackageLockFile :: Path Abs File -> IO LockFile
loadPackageLockFile lockFile = do
    val <- Yaml.decodeFileThrow (toFilePath lockFile)
    case Yaml.parseEither (parsePackageLockFile (parent lockFile)) val of
        Left str -> fail $ "Cannot parse package lock file: Got error " <> str
        Right lockFileIO -> lockFileIO

data LockFile = LockFile
    { lfPackageLocations :: !(Map RawPackageLocation PackageLocation)
    , lfoResolver :: !RawSnapshotLocation
    , lfcResolver :: !SnapshotLocation
    }

combineUnresolved :: Unresolved a -> Unresolved b -> Unresolved (a, b)
combineUnresolved a b = do
    ua <- a
    ub <- b
    pure (ua, ub)

parseRPLImmutable :: Value -> Parser (Unresolved RawPackageLocation)
parseRPLImmutable v = do
    xs :: Unresolved RawPackageLocationImmutable <- parseRPLI v
    pure $ RPLImmutable <$> xs

parseResolvedPath :: Value -> Parser (Unresolved RawPackageLocation)
parseResolvedPath value = mkMutable <$> parseJSON value
  where
    mkMutable :: Text -> Unresolved RawPackageLocation
    mkMutable t =
        Unresolved $ \mdir -> do
            case mdir of
                Nothing -> throwIO $ MutablePackageLocationFromUrl t
                Just dir -> do
                    abs' <- resolveDir dir $ T.unpack t
                    pure $ RPLMutable $ ResolvedPath (RelFilePath t) abs'

parseRPL :: Value -> Parser (Unresolved RawPackageLocation)
parseRPL v = parseRPLImmutable v <|> parseResolvedPath v

parsePImmutable :: Value -> Parser (Unresolved PackageLocation)
parsePImmutable v = do
    xs :: Unresolved PackageLocationImmutable <- parseJSON v
    pure $ PLImmutable <$> xs

parseSingleObject ::
       Value -> Parser (Unresolved (RawPackageLocation, PackageLocation))
parseSingleObject value =
    withObject
        "LockFile"
        (\obj -> do
             original <- obj .: "original"
             complete <- obj .: "complete"
             orig <- parseRPL original
             comp <- parsePImmutable complete
             pure $ combineUnresolved orig comp)
        value

parseSnapshotLocationPath :: Text -> Unresolved SnapshotLocation
parseSnapshotLocationPath t =
    Unresolved $ \mdir ->
        case mdir of
            Nothing -> throwIO $ InvalidFilePathSnapshot t
            Just dir -> do
                abs' <-
                    resolveFile dir (T.unpack t) `catchAny` \_ ->
                        throwIO (InvalidSnapshotLocation dir t)
                pure $ SLFilePath $ ResolvedPath (RelFilePath t) abs'

parseSLObject :: Value -> Parser (Unresolved SnapshotLocation)
parseSLObject =
    withObject "UnresolvedSnapshotLocation (Object)" $ \o ->
        ((pure . SLCompiler) <$> o .: "compiler") <|>
        ((\x y -> pure $ SLUrl x y) <$> o .: "url" <*> parseJSON (Object o)) <|>
        (parseSnapshotLocationPath <$> o .: "filepath")

parseSnapshotLocation :: Value -> Parser (Unresolved SnapshotLocation)
parseSnapshotLocation =
    withObject
        "UnresolvedSnapshotLocation"
        (\o -> do
             url <- o .: "url"
             bkey <- parseJSON (Object o)
             pure $ pure $ SLUrl url bkey)

parseSL :: Value -> Parser (Unresolved SnapshotLocation)
parseSL v = txtParser v <|> parseSLObject v
  where
    txt :: Text -> Maybe (Unresolved SnapshotLocation)
    txt t =
        either
            (const Nothing)
            (Just . pure . SLCompiler)
            (parseWantedCompiler t)
    txtParser =
        withText
            ("UnresolvedSnapshotLocation (Text)")
            (\t -> pure $ fromMaybe (parseSnapshotLocationPath t) (txt t))

parseBlobKey :: Object -> Parser (Maybe BlobKey)
parseBlobKey o = do
    msha <- o .:? "sha256"
    msize <- o .:? "size"
    case (msha, msize) of
        (Nothing, Nothing) -> pure Nothing
        (Just sha, Just size') -> pure $ Just $ BlobKey sha size'
        (Just _sha, Nothing) -> fail "You must also specify the file size"
        (Nothing, Just _) -> fail "You must also specify the file's SHA256"

parseRSLObject :: Value -> Parser (Unresolved RawSnapshotLocation)
parseRSLObject =
    withObject "UnresolvedRawSnapshotLocation (Object)" $ \o ->
        ((pure . RSLCompiler) <$> o .: "compiler") <|>
        ((\x y -> pure $ RSLUrl x y) <$> o .: "url" <*> parseBlobKey o) <|>
        (parseRawSnapshotLocationPath <$> o .: "filepath")

parseRSL :: Value -> Parser (Unresolved RawSnapshotLocation)
parseRSL v = txtParser v <|> parseRSLObject v
  where
    txtParser =
        withText
            "UnresolvedSnapshotLocation (Text)"
            (pure . parseRawSnapshotLocation)

parseSnapshotFile ::
       Value
    -> Parser (Unresolved ([RawPackageLocationImmutable], RawSnapshotLocation))
parseSnapshotFile (Object obj) = do
    packages <- obj .: "packages"
    resolver <- obj .: "resolver"
    xs <-
        withArray
            "SnapshotFileArray"
            (\vec -> sequence $ Vector.map parseRPLI vec)
            packages
    resolver <- parseRSL resolver
    pure $ combineUnresolved (sequence $ Vector.toList xs) resolver
parseSnapshotFile val = fail $ "Expected Object, but got: " <> (show val)

resolveSnapshotFile ::
       Path Abs Dir
    -> Value
    -> Parser (IO ([RawPackageLocationImmutable], RawSnapshotLocation))
resolveSnapshotFile rootDir val = do
    unrpl <- parseSnapshotFile val
    let pkgLoc = resolvePaths (Just rootDir) unrpl
    pure pkgLoc

parseRPLHttpText :: Value -> Parser (Unresolved RawPackageLocationImmutable)
parseRPLHttpText =
    withText "UnresolvedPackageLocationImmutable.UPLIArchive (Text)" $ \t ->
        case parseArchiveLocationText t of
            Nothing -> fail $ "Invalid archive location: " ++ T.unpack t
            Just (Unresolved mkArchiveLocation) ->
                pure $
                Unresolved $ \mdir -> do
                    raLocation <- mkArchiveLocation mdir
                    let raHash = Nothing
                        raSize = Nothing
                        raSubdir = T.empty
                    pure $ RPLIArchive RawArchive {..} rpmEmpty

parseRPLHackageText :: Value -> Parser (Unresolved RawPackageLocationImmutable)
parseRPLHackageText =
    withText "UnresolvedPackageLocationImmutable.UPLIHackage (Text)" $ \t ->
        case parsePackageIdentifierRevision t of
            Left e -> fail $ show e
            Right pir -> pure $ pure $ RPLIHackage pir Nothing

parseRPLHackageObject ::
       Value -> Parser (Unresolved RawPackageLocationImmutable)
parseRPLHackageObject =
    withObject "UnresolvedPackageLocationImmutable.UPLIHackage" $ \o ->
        (pure) <$> (RPLIHackage <$> o .: "hackage" <*> o .:? "pantry-tree")

optionalSubdirs' :: Object -> Parser OptionalSubdirs
optionalSubdirs' o =
    case HM.lookup "subdirs" o -- if subdirs exists, it needs to be valid
          of
        Just v' -> do
            subdirs <- parseJSON v'
            case NE.nonEmpty subdirs of
                Nothing -> fail "Invalid empty subdirs"
                Just x -> pure $ OSSubdirs x
        Nothing ->
            OSPackageMetadata <$> o .:? "subdir" .!= T.empty <*>
            (RawPackageMetadata <$> (fmap unCabalString <$> (o .:? "name")) <*>
             (fmap unCabalString <$> (o .:? "version")) <*>
             o .:? "pantry-tree" <*>
             o .:? "cabal-file")

parseRPLRepo :: Value -> Parser (Unresolved RawPackageLocationImmutable)
parseRPLRepo =
    withObject "UnresolvedPackageLocationImmutable.UPLIRepo" $ \o -> do
        (repoType, repoUrl) <-
            ((RepoGit, ) <$> o .: "git") <|> ((RepoHg, ) <$> o .: "hg")
        repoCommit <- o .: "commit"
        os <- optionalSubdirs' o
        pure $
            pure $
            NE.head $
            NE.map (\(repoSubdir, pm) -> RPLIRepo Repo {..} pm) (osToRpms os)

parseArchiveRPLObject ::
       Value -> Parser (Unresolved RawPackageLocationImmutable)
parseArchiveRPLObject =
    withObject "UnresolvedPackageLocationImmutable.UPLIArchive" $ \o -> do
        Unresolved mkArchiveLocation <-
            unWarningParser $ parseArchiveLocationObject o
        raHash <- o .:? "sha256"
        raSize <- o .:? "size"
        os <- optionalSubdirs' o
        pure $
            Unresolved $ \mdir -> do
                raLocation <- mkArchiveLocation mdir
                pure $
                    NE.head $
                    NE.map
                        (\(raSubdir, pm) -> RPLIArchive RawArchive {..} pm)
                        (osToRpms os)

parseGithubRPLObject :: Value -> Parser (Unresolved RawPackageLocationImmutable)
parseGithubRPLObject =
    withObject "PLArchive:github" $ \o -> do
        GitHubRepo ghRepo <- o .: "github"
        commit <- o .: "commit"
        let raLocation =
                ALUrl $
                T.concat
                    [ "https://github.com/"
                    , ghRepo
                    , "/archive/"
                    , commit
                    , ".tar.gz"
                    ]
        raHash <- o .:? "sha256"
        raSize <- o .:? "size"
        os <- optionalSubdirs' o
        pure $
            pure $
            NE.head $
            NE.map
                (\(raSubdir, pm) -> RPLIArchive RawArchive {..} pm)
                (osToRpms os)

parseRPLI :: Value -> Parser (Unresolved RawPackageLocationImmutable)
parseRPLI v =
    parseRPLHttpText v <|> parseRPLHackageText v <|> parseRPLHackageObject v <|>
    parseRPLRepo v <|>
    parseArchiveRPLObject v <|>
    parseGithubRPLObject v

parsePLI :: Value -> Parser (Unresolved PackageLocationImmutable)
parsePLI v = do
    x <- parseJSON v
    pure x

parseImmutableObject ::
       Value
    -> Parser (Unresolved ( RawPackageLocationImmutable
                          , PackageLocationImmutable))
parseImmutableObject value =
    withObject
        "LockFile"
        (\obj -> do
             original <- obj .: "original"
             complete <- obj .: "complete"
             orig <- parseRPLI original
             comp <- parsePLI complete
             pure $ combineUnresolved orig comp)
        value

parseSnapshotLayerLockFile ::
       Value
    -> Parser (Unresolved [( RawPackageLocationImmutable
                           , PackageLocationImmutable)])
parseSnapshotLayerLockFile =
    withObject
        "SnapshotLayerLockFile"
        (\obj -> do
             vals <- obj .: "dependencies"
             xs <-
                 withArray
                     "SnapshotLayerLockArray"
                     (\vec -> sequence $ Vector.map parseImmutableObject vec)
                     vals
             pure $ sequence $ Vector.toList xs)

resolveSnapshotLayerLockFile ::
       Path Abs Dir
    -> Value
    -> Parser (IO (Map RawPackageLocationImmutable PackageLocationImmutable))
resolveSnapshotLayerLockFile rootDir val = do
    pkgs <- parseSnapshotLayerLockFile val
    let pkgsLoc = resolvePaths (Just rootDir) pkgs
    pure $ Map.fromList <$> pkgsLoc

loadSnapshotLayerLockFile ::
       Path Abs File
    -> Path Abs Dir
    -> IO (Map RawPackageLocationImmutable PackageLocationImmutable)
loadSnapshotLayerLockFile lockFile rootDir = do
    val <- Yaml.decodeFileThrow (toFilePath lockFile)
    case Yaml.parseEither (resolveSnapshotLayerLockFile rootDir) val of
        Left str ->
            fail $
            "Cannot parse snapshot lock file: Got error " <> str <> (show val)
        Right lockFileIO -> lockFileIO
