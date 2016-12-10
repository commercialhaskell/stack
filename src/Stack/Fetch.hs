{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Functionality for downloading packages securely for cabal's usage.

module Stack.Fetch
    ( unpackPackages
    , unpackPackageIdents
    , fetchPackages
    , untar
    , resolvePackages
    , resolvePackagesAllowMissing
    , ResolvedPackage (..)
    , withCabalFiles
    , withCabalLoader
    ) where

import qualified    Codec.Archive.Tar as Tar
import qualified    Codec.Archive.Tar.Check as Tar
import qualified    Codec.Archive.Tar.Entry as Tar
import              Codec.Compression.GZip (decompress)
import              Control.Applicative
import              Control.Concurrent.Async (Concurrently (..))
import              Control.Concurrent.MVar.Lifted (modifyMVar, newMVar)
import              Control.Concurrent.STM
import              Control.Exception (assert)
import              Control.Exception.Safe (tryIO)
import              Control.Monad (join, liftM, unless, void, when)
import              Control.Monad.Catch
import              Control.Monad.IO.Class
import              Control.Monad.Logger
import              Control.Monad.Reader (ask, runReaderT)
import              Control.Monad.Trans.Control
import              Control.Monad.Trans.Unlift (MonadBaseUnlift, askRunBase)
import "cryptohash" Crypto.Hash (SHA512 (..))
import              Data.ByteString (ByteString)
import qualified    Data.ByteString as S
import qualified    Data.ByteString.Lazy as L
import              Data.Either (partitionEithers)
import qualified    Data.Foldable as F
import              Data.Function (fix)
import qualified    Data.Git as Git
import qualified    Data.Git.Ref as Git
import qualified    Data.Git.Storage as Git
import qualified    Data.Git.Storage.Object as Git
import qualified    Data.HashMap.Strict as HashMap
import              Data.List (intercalate)
import              Data.List.NonEmpty (NonEmpty)
import qualified    Data.List.NonEmpty as NE
import              Data.Map (Map)
import qualified    Data.Map as Map
import              Data.Maybe (maybeToList, catMaybes)
import              Data.Monoid
import              Data.Set (Set)
import qualified    Data.Set as Set
import              Data.String (fromString)
import qualified    Data.Text as T
import              Data.Text.Encoding (decodeUtf8)
import              Data.Text.Metrics
import              Data.Typeable (Typeable)
import              Data.Word (Word64)
import              Network.HTTP.Download
import              Path
import              Path.Extra (toFilePathNoTrailingSep)
import              Path.IO
import              Prelude -- Fix AMP warning
import              Stack.GhcPkg
import              Stack.PackageIndex
import              Stack.Types.BuildPlan
import              Stack.Types.Config
import              Stack.Types.PackageIdentifier
import              Stack.Types.PackageIndex
import              Stack.Types.PackageName
import              Stack.Types.Version
import              System.FilePath ((<.>))
import qualified    System.FilePath as FP
import              System.IO
import              System.PosixCompat (setFileMode)

type PackageCaches = Map PackageIdentifier (PackageIndex, PackageCache)

data FetchException
    = Couldn'tReadIndexTarball FilePath Tar.FormatError
    | Couldn'tReadPackageTarball FilePath SomeException
    | UnpackDirectoryAlreadyExists (Set FilePath)
    | CouldNotParsePackageSelectors [String]
    | UnknownPackageNames (Set PackageName)
    | UnknownPackageIdentifiers (Set PackageIdentifier) String
    deriving Typeable
instance Exception FetchException

instance Show FetchException where
    show (Couldn'tReadIndexTarball fp err) = concat
        [ "There was an error reading the index tarball "
        , fp
        , ": "
        , show err
        ]
    show (Couldn'tReadPackageTarball fp err) = concat
        [ "There was an error reading the package tarball "
        , fp
        , ": "
        , show err
        ]
    show (UnpackDirectoryAlreadyExists dirs) = unlines
        $ "Unable to unpack due to already present directories:"
        : map ("    " ++) (Set.toList dirs)
    show (CouldNotParsePackageSelectors strs) =
        "The following package selectors are not valid package names or identifiers: " ++
        intercalate ", " strs
    show (UnknownPackageNames names) =
        "The following packages were not found in your indices: " ++
        intercalate ", " (map packageNameString $ Set.toList names)
    show (UnknownPackageIdentifiers idents suggestions) =
        "The following package identifiers were not found in your indices: " ++
        intercalate ", " (map packageIdentifierString $ Set.toList idents) ++
        (if null suggestions then "" else "\n" ++ suggestions)

-- | Fetch packages into the cache without unpacking
fetchPackages :: (StackMiniM env m, HasConfig env)
              => EnvOverride
              -> Set PackageIdentifier
              -> m ()
fetchPackages menv idents' = do
    resolved <- resolvePackages menv idents Set.empty
    ToFetchResult toFetch alreadyUnpacked <- getToFetch Nothing resolved
    assert (Map.null alreadyUnpacked) (return ())
    nowUnpacked <- fetchPackages' Nothing toFetch
    assert (Map.null nowUnpacked) (return ())
  where
    -- Since we're just fetching tarballs and not unpacking cabal files, we can
    -- always provide a Nothing Git SHA
    idents = Map.fromList $ map (, Nothing) $ Set.toList idents'

-- | Intended to work for the command line command.
unpackPackages :: (StackMiniM env m, HasConfig env)
               => EnvOverride
               -> FilePath -- ^ destination
               -> [String] -- ^ names or identifiers
               -> m ()
unpackPackages menv dest input = do
    dest' <- resolveDir' dest
    (names, idents) <- case partitionEithers $ map parse input of
        ([], x) -> return $ partitionEithers x
        (errs, _) -> throwM $ CouldNotParsePackageSelectors errs
    resolved <- resolvePackages menv
        (Map.fromList $ map (, Nothing) idents)
        (Set.fromList names)
    ToFetchResult toFetch alreadyUnpacked <- getToFetch (Just dest') resolved
    unless (Map.null alreadyUnpacked) $
        throwM $ UnpackDirectoryAlreadyExists $ Set.fromList $ map toFilePath $ Map.elems alreadyUnpacked
    unpacked <- fetchPackages' Nothing toFetch
    F.forM_ (Map.toList unpacked) $ \(ident, dest'') -> $logInfo $ T.pack $ concat
        [ "Unpacked "
        , packageIdentifierString ident
        , " to "
        , toFilePath dest''
        ]
  where
    -- Possible future enhancement: parse names as name + version range
    parse s =
        case parsePackageNameFromString s of
            Right x -> Right $ Left x
            Left _ ->
                case parsePackageIdentifierFromString s of
                    Left _ -> Left s
                    Right x -> Right $ Right x

-- | Ensure that all of the given package idents are unpacked into the build
-- unpack directory, and return the paths to all of the subdirectories.
unpackPackageIdents
    :: (StackMiniM env m, HasConfig env)
    => EnvOverride
    -> Path Abs Dir -- ^ unpack directory
    -> Maybe (Path Rel Dir) -- ^ the dist rename directory, see: https://github.com/fpco/stack/issues/157
    -> Map PackageIdentifier (Maybe GitSHA1)
    -> m (Map PackageIdentifier (Path Abs Dir))
unpackPackageIdents menv unpackDir mdistDir idents = do
    resolved <- resolvePackages menv idents Set.empty
    ToFetchResult toFetch alreadyUnpacked <- getToFetch (Just unpackDir) resolved
    nowUnpacked <- fetchPackages' mdistDir toFetch
    return $ alreadyUnpacked <> nowUnpacked

data ResolvedPackage = ResolvedPackage
    { rpCache :: !PackageCache
    , rpIndex :: !PackageIndex
    , rpGitSHA1 :: !(Maybe GitSHA1)
    }

-- | Resolve a set of package names and identifiers into @FetchPackage@ values.
resolvePackages :: (StackMiniM env m, HasConfig env)
                => EnvOverride
                -> Map PackageIdentifier (Maybe GitSHA1)
                -> Set PackageName
                -> m (Map PackageIdentifier ResolvedPackage)
resolvePackages menv idents0 names0 = do
    eres <- go
    case eres of
        Left _ -> do
            updateAllIndices menv
            go >>= either throwM return
        Right x -> return x
  where
    go = r <$> resolvePackagesAllowMissing idents0 names0
    r (missingNames, missingIdents, idents)
      | not $ Set.null missingNames  = Left $ UnknownPackageNames       missingNames
      | not $ Set.null missingIdents = Left $ UnknownPackageIdentifiers missingIdents ""
      | otherwise                    = Right idents

resolvePackagesAllowMissing
    :: (StackMiniM env m, HasConfig env)
    => Map PackageIdentifier (Maybe GitSHA1)
    -> Set PackageName
    -> m (Set PackageName, Set PackageIdentifier, Map PackageIdentifier ResolvedPackage)
resolvePackagesAllowMissing idents0 names0 = do
    (caches, shaCaches) <- getPackageCaches
    let versions = Map.fromListWith max $ map toTuple $ Map.keys caches
        (missingNames, idents1) = partitionEithers $ map
            (\name -> maybe (Left name ) (Right . PackageIdentifier name)
                (Map.lookup name versions))
            (Set.toList names0)
    let (missingIdents, resolved) = partitionEithers $ map (goIdent caches shaCaches)
                                  $ Map.toList
                                  $ idents0 <> Map.fromList (map (, Nothing) idents1)
    return (Set.fromList missingNames, Set.fromList missingIdents, Map.fromList resolved)
  where
    goIdent caches shaCaches (ident, mgitsha) =
        case Map.lookup ident caches of
            Nothing -> Left ident
            Just (index, cache) ->
                let (index', cache', mgitsha') =
                      case mgitsha of
                        Nothing -> (index, cache, mgitsha)
                        Just gitsha ->
                            case HashMap.lookup gitsha shaCaches of
                                Just (index'', offsetSize) ->
                                        ( index''
                                        , cache { pcOffsetSize = offsetSize }
                                        -- we already got the info
                                        -- about this SHA, don't do
                                        -- any lookups later
                                        , Nothing
                                        )
                                Nothing -> (index, cache, mgitsha)
                 in Right (ident, ResolvedPackage
                    { rpCache = cache'
                    , rpIndex = index'
                    , rpGitSHA1 = mgitsha'
                    })

data ToFetch = ToFetch
    { tfTarball :: !(Path Abs File)
    , tfDestDir :: !(Maybe (Path Abs Dir))
    , tfUrl     :: !T.Text
    , tfSize    :: !(Maybe Word64)
    , tfSHA512  :: !(Maybe ByteString)
    , tfCabal   :: !ByteString
    -- ^ Contents of the .cabal file
    }

data ToFetchResult = ToFetchResult
    { tfrToFetch         :: !(Map PackageIdentifier ToFetch)
    , tfrAlreadyUnpacked :: !(Map PackageIdentifier (Path Abs Dir))
    }

-- | Add the cabal files to a list of idents with their caches.
withCabalFiles
    :: (StackMiniM env m, HasConfig env)
    => IndexName
    -> [(PackageIdentifier, PackageCache, Maybe GitSHA1, a)]
    -> (PackageIdentifier -> a -> ByteString -> IO b)
    -> m [b]
withCabalFiles name pkgs f = do
    indexPath <- configPackageIndex name
    mgitRepo <- configPackageIndexRepo name
    bracket
        (liftIO $ openBinaryFile (toFilePath indexPath) ReadMode)
        (liftIO . hClose) $ \h ->
            let inner mgit = mapM (goPkg h mgit) pkgs
             in case mgitRepo of
                    Nothing -> inner Nothing
                    Just repo -> bracket
                        (liftIO $ Git.openRepo
                                $ fromString
                                $ toFilePath repo FP.</> ".git")
                        (liftIO . Git.closeRepo)
                        (inner . Just)
  where
    goPkg h (Just git) (ident, pc, Just (GitSHA1 sha), tf) = do
        let ref = Git.fromHex sha
        mobj <- liftIO $ tryIO $ Git.getObject git ref True
        case mobj of
            Right (Just (Git.ObjBlob (Git.Blob bs))) -> liftIO $ f ident tf (L.toStrict bs)
            -- fallback when the appropriate SHA isn't found
            e -> do
                $logWarn $ mconcat
                    [ "Did not find .cabal file for "
                    , T.pack $ packageIdentifierString ident
                    , " with Git SHA of "
                    , decodeUtf8 sha
                    ]
                $logDebug (T.pack (show e))
                goPkg h Nothing (ident, pc, Nothing, tf)
    goPkg h _mgit (ident, pc, mgitsha, tf) = do
        -- We have a Just as the git SHA, so looking up previously
        -- failed, and we're not in a Git repo. So warn the user and
        -- move on.
        case mgitsha of
            Nothing -> return ()
            Just (GitSHA1 sha) -> $logWarn $ mconcat
                [ "Did not find .cabal file for "
                , T.pack $ packageIdentifierString ident
                , " with Git SHA of "
                , decodeUtf8 sha
                , " in tarball-based cache"
                ]
        let OffsetSize offset size = pcOffsetSize pc
        liftIO $ do
            hSeek h AbsoluteSeek $ fromIntegral offset
            cabalBS <- S.hGet h $ fromIntegral size
            f ident tf cabalBS

-- | Provide a function which will load up a cabal @ByteString@ from the
-- package indices.
withCabalLoader
    :: (StackMiniM env m, HasConfig env, MonadBaseUnlift IO m)
    => EnvOverride
    -> ((PackageIdentifier -> IO ByteString) -> m a)
    -> m a
withCabalLoader menv inner = do
    env <- ask

    -- Want to try updating the index once during a single run for missing
    -- package identifiers. We also want to ensure we only update once at a
    -- time
    --
    -- TODO: probably makes sense to move this concern into getPackageCaches
    updateRef <- liftIO $ newMVar True

    loadCaches <- getPackageCachesIO
    runInBase <- liftBaseWith $ \run -> return (void . run)
    unlift <- askRunBase

    -- TODO in the future, keep all of the necessary @Handle@s open
    let doLookup :: PackageIdentifier
                 -> IO ByteString
        doLookup ident = do
            (caches, _gitSHACaches) <- loadCaches
            eres <- unlift $ lookupPackageIdentifierExact ident env caches
            case eres of
                Just bs -> return bs
                -- Update the cache and try again
                Nothing -> do
                    let fuzzy = fuzzyLookupCandidates ident caches
                        suggestions = case fuzzy of
                            Nothing ->
                              case typoCorrectionCandidates ident caches of
                                  Nothing -> ""
                                  Just cs -> "Perhaps you meant " <>
                                    orSeparated cs <> "?"
                            Just cs -> "Possible candidates: " <>
                              commaSeparated (NE.map packageIdentifierText cs)
                              <> "."
                    join $ modifyMVar updateRef $ \toUpdate ->
                        if toUpdate then do
                            runInBase $ do
                                $logInfo $ T.concat
                                    [ "Didn't see "
                                    , T.pack $ packageIdentifierString ident
                                    , " in your package indices.\n"
                                    , "Updating and trying again."
                                    ]
                                updateAllIndices menv
                                _ <- getPackageCaches
                                return ()
                            return (False, doLookup ident)
                        else return (toUpdate,
                                     throwM $ UnknownPackageIdentifiers
                                       (Set.singleton ident) (T.unpack suggestions))
    inner doLookup

lookupPackageIdentifierExact
  :: (StackMiniM env m, HasConfig env)
  => PackageIdentifier
  -> env
  -> PackageCaches
  -> m (Maybe ByteString)
lookupPackageIdentifierExact ident env caches =
    case Map.lookup ident caches of
        Nothing -> return Nothing
        Just (index, cache) -> do
            [bs] <- flip runReaderT env
                  $ withCabalFiles (indexName index) [(ident, cache, Nothing, ())]
                  $ \_ _ bs -> return bs
            return $ Just bs

-- | Given package identifier and package caches, return list of packages
-- with the same name and the same two first version number components found
-- in the caches.
fuzzyLookupCandidates
  :: PackageIdentifier
  -> PackageCaches
  -> Maybe (NonEmpty PackageIdentifier)
fuzzyLookupCandidates (PackageIdentifier name ver) caches =
  let (_, zero, bigger) = Map.splitLookup zeroIdent caches
      zeroIdent         = PackageIdentifier name $(mkVersion "0.0")
      sameName  (PackageIdentifier n _) = n == name
      sameMajor (PackageIdentifier _ v) = toMajorVersion v == toMajorVersion ver
  in NE.nonEmpty . filter sameMajor $ maybe [] (pure . const zeroIdent) zero
         <> takeWhile sameName (Map.keys bigger)

-- | Try to come up with typo corrections for given package identifier using
-- package caches. This should be called before giving up, i.e. when
-- 'fuzzyLookupCandidates' cannot return anything.
typoCorrectionCandidates
  :: PackageIdentifier
  -> PackageCaches
  -> Maybe (NonEmpty T.Text)
typoCorrectionCandidates ident =
  let getName = packageNameText . packageIdentifierName
      name    = getName ident
  in  NE.nonEmpty
    . Map.keys
    . Map.filterWithKey (const . (== 1) . damerauLevenshtein name)
    . Map.mapKeys getName

-- | Figure out where to fetch from.
getToFetch :: (StackMiniM env m, HasConfig env)
           => Maybe (Path Abs Dir) -- ^ directory to unpack into, @Nothing@ means no unpack
           -> Map PackageIdentifier ResolvedPackage
           -> m ToFetchResult
getToFetch mdest resolvedAll = do
    (toFetch0, unpacked) <- liftM partitionEithers $ mapM checkUnpacked $ Map.toList resolvedAll
    toFetch1 <- mapM goIndex $ Map.toList $ Map.fromListWith (++) toFetch0
    return ToFetchResult
        { tfrToFetch = Map.unions toFetch1
        , tfrAlreadyUnpacked = Map.fromList unpacked
        }
  where
    checkUnpacked (ident, resolved) = do
        dirRel <- parseRelDir $ packageIdentifierString ident
        let mdestDir = (</> dirRel) <$> mdest
        mexists <-
            case mdestDir of
                Nothing -> return Nothing
                Just destDir -> do
                    exists <- doesDirExist destDir
                    return $ if exists then Just destDir else Nothing
        case mexists of
            Just destDir -> return $ Right (ident, destDir)
            Nothing -> do
                let index = rpIndex resolved
                    d = pcDownload $ rpCache resolved
                    targz = T.pack $ packageIdentifierString ident ++ ".tar.gz"
                tarball <- configPackageTarball (indexName index) ident
                return $ Left (indexName index, [(ident, rpCache resolved, rpGitSHA1 resolved, ToFetch
                    { tfTarball = tarball
                    , tfDestDir = mdestDir
                    , tfUrl = case d of
                        Just d' -> decodeUtf8 $ pdUrl d'
                        Nothing -> indexDownloadPrefix index <> targz
                    , tfSize = fmap pdSize d
                    , tfSHA512 = fmap pdSHA512 d
                    , tfCabal = S.empty -- filled in by goIndex
                    })])

    goIndex (name, pkgs) =
        liftM Map.fromList $
        withCabalFiles name pkgs $ \ident tf cabalBS ->
        return (ident, tf { tfCabal = cabalBS })

-- | Download the given name,version pairs into the directory expected by cabal.
--
-- For each package it downloads, it will optionally unpack it to the given
-- @Path@ (if present). Note that unpacking is not simply a matter of
-- untarring, but also of grabbing the cabal file from the package index. The
-- destinations should not include package identifiers.
--
-- Returns the list of paths unpacked, including package identifiers. E.g.:
--
-- @
-- fetchPackages [("foo-1.2.3", Just "/some/dest")] ==> ["/some/dest/foo-1.2.3"]
-- @
--
-- Since 0.1.0.0
fetchPackages' :: (StackMiniM env m, HasConfig env)
               => Maybe (Path Rel Dir) -- ^ the dist rename directory, see: https://github.com/fpco/stack/issues/157
               -> Map PackageIdentifier ToFetch
               -> m (Map PackageIdentifier (Path Abs Dir))
fetchPackages' mdistDir toFetchAll = do
    connCount <- view $ configL.to configConnectionCount
    outputVar <- liftIO $ newTVarIO Map.empty

    runInBase <- liftBaseWith $ \run -> return (void . run)
    parMapM_
        connCount
        (go outputVar runInBase)
        (Map.toList toFetchAll)

    liftIO $ readTVarIO outputVar
  where
    go :: (MonadIO m,MonadThrow m,MonadLogger m)
       => TVar (Map PackageIdentifier (Path Abs Dir))
       -> (m () -> IO ())
       -> (PackageIdentifier, ToFetch)
       -> m ()
    go outputVar runInBase (ident, toFetch) = do
        req <- parseUrlThrow $ T.unpack $ tfUrl toFetch
        let destpath = tfTarball toFetch

        let toHashCheck bs = HashCheck SHA512 (CheckHexDigestByteString bs)
        let downloadReq = DownloadRequest
                { drRequest = req
                , drHashChecks = map toHashCheck $ maybeToList (tfSHA512 toFetch)
                , drLengthCheck = fromIntegral <$> tfSize toFetch
                , drRetryPolicy = drRetryPolicyDefault
                }
        let progressSink _ =
                liftIO $ runInBase $ $logInfo $ packageIdentifierText ident <> ": download"
        _ <- verifiedDownload downloadReq destpath progressSink

        identStrP <- parseRelDir $ packageIdentifierString ident

        F.forM_ (tfDestDir toFetch) $ \destDir -> do
            let innerDest = toFilePath destDir

            unexpectedEntries <- liftIO $ untar destpath identStrP (parent destDir)

            liftIO $ do
                case mdistDir of
                    Nothing -> return ()
                    -- See: https://github.com/fpco/stack/issues/157
                    Just distDir -> do
                        let inner = parent destDir </> identStrP
                            oldDist = inner </> $(mkRelDir "dist")
                            newDist = inner </> distDir
                        exists <- doesDirExist oldDist
                        when exists $ do
                            -- Previously used takeDirectory, but that got confused
                            -- by trailing slashes, see:
                            -- https://github.com/commercialhaskell/stack/issues/216
                            --
                            -- Instead, use Path which is a bit more resilient
                            ensureDir $ parent newDist
                            renameDir oldDist newDist

                let cabalFP =
                        innerDest FP.</>
                        packageNameString (packageIdentifierName ident)
                        <.> "cabal"
                S.writeFile cabalFP $ tfCabal toFetch

                atomically $ modifyTVar outputVar $ Map.insert ident destDir

            F.forM_ unexpectedEntries $ \(path, entryType) ->
                $logWarn $ "Unexpected entry type " <> entryType <> " for entry " <> T.pack path

-- | Internal function used to unpack tarball.
--
-- Takes a path to a .tar.gz file, the name of the directory it should contain,
-- and a destination folder to extract the tarball into. Returns unexpected
-- entries, as pairs of paths and descriptions.
untar :: forall b1 b2. Path b1 File -> Path Rel Dir -> Path b2 Dir -> IO [(FilePath, T.Text)]
untar tarPath expectedTarFolder destDirParent = do
  ensureDir destDirParent
  withBinaryFile (toFilePath tarPath) ReadMode $ \h -> do
                -- Avoid using L.readFile, which is more likely to leak
                -- resources
                lbs <- L.hGetContents h
                let rawEntries = fmap (either wrap wrap)
                            $ Tar.checkTarbomb (toFilePathNoTrailingSep expectedTarFolder)
                            $ Tar.read $ decompress lbs

                    filterEntries
                      :: Monoid w => (Tar.Entry -> (Bool, w))
                         -> Tar.Entries b -> (Tar.Entries b, w)
                    -- Allow collecting warnings, Writer-monad style.
                    filterEntries f =
                      Tar.foldEntries
                        (\e -> let (res, w) = f e in
                            \(rest, wOld) -> ((if res then Tar.Next e else id) rest, wOld <> w))
                        (Tar.Done, mempty)
                        (\err -> (Tar.Fail err, mempty))

                    extractableEntry e =
                      case Tar.entryContent e of
                        Tar.NormalFile _ _ -> (True, [])
                        Tar.Directory -> (True, [])
                        Tar.SymbolicLink _ -> (True, [])
                        Tar.HardLink _ -> (True, [])
                        Tar.OtherEntryType 'g' _ _ -> (False, [])
                        Tar.OtherEntryType 'x' _ _ -> (False, [])
                        Tar.CharacterDevice _ _ -> (False, [(path, "character device")])
                        Tar.BlockDevice _ _ -> (False, [(path, "block device")])
                        Tar.NamedPipe -> (False, [(path, "named pipe")])
                        Tar.OtherEntryType code _ _ -> (False, [(path, "other entry type with code " <> T.pack (show code))])
                        where
                          path = Tar.fromTarPath $ Tar.entryTarPath e
                    (entries, unexpectedEntries) = filterEntries extractableEntry rawEntries

                    wrap :: Exception e => e -> FetchException
                    wrap = Couldn'tReadPackageTarball (toFilePath tarPath) . toException

                    getPerms :: Tar.Entry -> (FilePath, Tar.Permissions)
                    getPerms e = (toFilePath destDirParent FP.</> Tar.fromTarPath (Tar.entryTarPath e),
                                  Tar.entryPermissions e)

                    filePerms :: [(FilePath, Tar.Permissions)]
                    filePerms = catMaybes $ Tar.foldEntries (\e -> (:) (Just $ getPerms e))
                                                            [] (const []) entries
                Tar.unpack (toFilePath destDirParent) entries
                -- Reset file permissions as they were in the tarball, but only
                -- for extracted entries (whence filterEntries extractableEntry above).
                -- See https://github.com/commercialhaskell/stack/issues/2361
                mapM_ (\(fp, perm) -> setFileMode
                    (FP.dropTrailingPathSeparator fp)
                    perm) filePerms
                return unexpectedEntries

parMapM_ :: (F.Foldable f,MonadIO m,MonadBaseControl IO m)
         => Int
         -> (a -> m ())
         -> f a
         -> m ()
parMapM_ (max 1 -> 1) f xs = F.mapM_ f xs
parMapM_ cnt f xs0 = do
    var <- liftIO (newTVarIO $ F.toList xs0)

    -- See comment on similar line in Stack.Build
    runInBase <- liftBaseWith $ \run -> return (void . run)

    let worker = fix $ \loop -> join $ atomically $ do
            xs <- readTVar var
            case xs of
                [] -> return $ return ()
                x:xs' -> do
                    writeTVar var xs'
                    return $ do
                        runInBase $ f x
                        loop
        workers 1 = Concurrently worker
        workers i = Concurrently worker *> workers (i - 1)
    liftIO $ runConcurrently $ workers cnt

orSeparated :: NonEmpty T.Text -> T.Text
orSeparated xs
  | NE.length xs == 1 = NE.head xs
  | NE.length xs == 2 = NE.head xs <> " or " <> NE.last xs
  | otherwise = T.intercalate ", " (NE.init xs) <> ", or " <> NE.last xs

commaSeparated :: NonEmpty T.Text -> T.Text
commaSeparated = F.fold . NE.intersperse ", "
