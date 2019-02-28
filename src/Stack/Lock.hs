{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Stack.Lock where

import Data.Aeson.Extended (unWarningParser)
import Data.List ((\\), intersect)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as Vector
import qualified Data.Yaml as Yaml
import Data.Yaml
import Pantry
    ( GitHubRepo(..)
    , OptionalSubdirs(..)
    , Unresolved(..)
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

loadLockFile :: Path Abs File -> IO (LockFile IO)
loadLockFile lockFile = do
    val <- Yaml.decodeFileThrow (toFilePath lockFile)
    case Yaml.parseEither (resolveLockFile (parent lockFile)) val of
        Left str -> fail $ "Cannot parse lock file: Got error " <> str
        Right lockFileIO -> pure lockFileIO

data LockFile a = LockFile
    { lfPackageLocation :: a [(PackageLocation, RawPackageLocation)]
    , lfoResolver :: a RawSnapshotLocation
    , lfcResolver :: a SnapshotLocation
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
       Value -> Parser (Unresolved (PackageLocation, RawPackageLocation))
parseSingleObject value =
    withObject
        "LockFile"
        (\obj -> do
             original <- obj .: "original"
             complete <- obj .: "complete"
             orig <- parseRPL original
             comp <- parsePImmutable complete
             pure $ combineUnresolved comp orig)
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

parseLockFile :: Value -> Parser (LockFile Unresolved)
parseLockFile value =
    withObject
        "LockFile"
        (\obj -> do
             vals :: Value <- obj .: "dependencies"
             xs <-
                 withArray
                     "LockFileArray"
                     (\vec -> sequence $ Vector.map parseSingleObject vec)
                     vals
             resolver <- obj .: "resolver"
             roriginal <- resolver .: "original"
             rcomplete <- resolver .: "complete"
             ro <- parseRSL roriginal
             rc <- parseSL rcomplete
             pure $
                 LockFile
                     { lfPackageLocation = sequence (Vector.toList xs)
                     , lfoResolver = ro
                     , lfcResolver = rc
                     })
        value

resolveLockFile :: Path Abs Dir -> Value -> Parser (LockFile IO)
resolveLockFile rootDir v = do
    lockFile <- parseLockFile v
    let pkgLoc = (lfPackageLocation lockFile)
        origRes = lfoResolver lockFile
        compRes = lfcResolver lockFile
        pkgLoc' = resolvePaths (Just rootDir) pkgLoc
    pure $
        LockFile
            { lfPackageLocation = pkgLoc'
            , lfoResolver = resolvePaths (Just rootDir) origRes
            , lfcResolver = resolvePaths (Just rootDir) compRes
            }

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
    -> Parser (Unresolved ( PackageLocationImmutable
                          , RawPackageLocationImmutable))
parseImmutableObject value =
    withObject
        "LockFile"
        (\obj -> do
             original <- obj .: "original"
             complete <- obj .: "complete"
             orig <- parseRPLI original
             comp <- parsePLI complete
             pure $ combineUnresolved comp orig)
        value

parseSnapshotLockFile ::
       Value
    -> Parser (Unresolved [( PackageLocationImmutable
                           , RawPackageLocationImmutable)])
parseSnapshotLockFile =
    withObject
        "SnapshotLockFile"
        (\obj -> do
             vals <- obj .: "dependencies"
             xs <-
                 withArray
                     "SnapshotLockArray"
                     (\vec -> sequence $ Vector.map parseImmutableObject vec)
                     vals
             pure $ sequence $ Vector.toList xs)

resolveSnapshotLockFile ::
       Path Abs Dir
    -> Value
    -> Parser (IO [(PackageLocationImmutable, RawPackageLocationImmutable)])
resolveSnapshotLockFile rootDir val = do
    pkgs <- parseSnapshotLockFile val
    let pkgsLoc = resolvePaths (Just rootDir) pkgs
    pure pkgsLoc

loadSnapshotLockFile ::
       Path Abs File
    -> Path Abs Dir
    -> IO [(PackageLocationImmutable, RawPackageLocationImmutable)]
loadSnapshotLockFile lockFile rootDir = do
    val <- Yaml.decodeFileThrow (toFilePath lockFile)
    case Yaml.parseEither (resolveSnapshotLockFile rootDir) val of
        Left str ->
            fail $
            "Cannot parse snapshot lock file: Got error " <> str <> (show val)
        Right lockFileIO -> lockFileIO
