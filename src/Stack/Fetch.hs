{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE RecordWildCards       #-}

-- | Functionality for downloading packages securely for cabal's usage.

module Stack.Fetch
    ( unpackPackages
    , unpackPackageIdent
    , unpackPackageIdents
    , fetchPackages
    , untar
    , resolvePackages
    , resolvePackagesAllowMissing
    , ResolvedPackage (..)
    , withCabalFiles
    , loadFromIndex
    ) where

import qualified    Codec.Archive.Tar as Tar
import qualified    Codec.Archive.Tar.Check as Tar
import qualified    Codec.Archive.Tar.Entry as Tar
import              Codec.Compression.GZip (decompress)
import              Stack.Prelude
import              Crypto.Hash (SHA256 (..))
import qualified    Data.ByteString as S
import qualified    Data.Foldable as F
import qualified    Data.HashMap.Strict as HashMap
import qualified    Data.HashSet as HashSet
import              Data.List (intercalate, maximum)
import              Data.List.NonEmpty (NonEmpty)
import qualified    Data.List.NonEmpty as NE
import qualified    Data.Map as Map
import qualified    Data.Set as Set
import qualified    Data.Text as T
import              Data.Text.Encoding (decodeUtf8)
import              Data.Text.Metrics
import              Lens.Micro (to)
import              Network.HTTP.Download
import              Path
import              Path.Extra (toFilePathNoTrailingSep)
import              Path.IO
import              Stack.PackageIndex
import              Stack.Types.BuildPlan
import              Stack.Types.PackageIdentifier
import              Stack.Types.PackageIndex
import              Stack.Types.PackageName
import              Stack.Types.Version
import qualified    System.FilePath as FP
import              System.IO (SeekMode (AbsoluteSeek))
import              System.PosixCompat (setFileMode)

data FetchException
    = Couldn'tReadIndexTarball FilePath Tar.FormatError
    | Couldn'tReadPackageTarball FilePath SomeException
    | UnpackDirectoryAlreadyExists (Set FilePath)
    | CouldNotParsePackageSelectors [String]
    | UnknownPackageNames (Set PackageName)
    | UnknownPackageIdentifiers (HashSet PackageIdentifierRevision) String
        Bool -- Do we use any 00-index.tar.gz indices? Just used for more informative error messages
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
    show (UnknownPackageIdentifiers idents suggestions uses00Index) =
        "The following package identifiers were not found in your indices: " ++
        intercalate ", " (map packageIdentifierRevisionString $ HashSet.toList idents) ++
        (if null suggestions then "" else "\n" ++ suggestions) ++
        (if uses00Index then "\n\nYou seem to be using a legacy 00-index.tar.gz tarball.\nConsider changing your configuration to use a 01-index.tar.gz file.\nAlternatively, you can set the ignore-revision-mismatch setting to true.\nFor more information, see: https://github.com/commercialhaskell/stack/issues/3520" else "")

-- | Fetch packages into the cache without unpacking
fetchPackages :: HasCabalLoader env => Set PackageIdentifier -> RIO env ()
fetchPackages idents' = do
    resolved <- resolvePackages Nothing idents Set.empty
    ToFetchResult toFetch alreadyUnpacked <- getToFetch Nothing resolved
    assert (Map.null alreadyUnpacked) (return ())
    nowUnpacked <- fetchPackages' Nothing toFetch
    assert (Map.null nowUnpacked) (return ())
  where
    -- Since we're just fetching tarballs and not unpacking cabal files, we can
    -- always provide a CFILatest cabal file info
    idents = map (flip PackageIdentifierRevision CFILatest) $ Set.toList idents'

-- | Intended to work for the command line command.
unpackPackages :: HasCabalLoader env
               => Maybe SnapshotDef -- ^ when looking up by name, take from this build plan
               -> FilePath -- ^ destination
               -> [String] -- ^ names or identifiers
               -> RIO env ()
unpackPackages mSnapshotDef dest input = do
    dest' <- resolveDir' dest
    (names, idents) <- case partitionEithers $ map parse input of
        ([], x) -> return $ partitionEithers x
        (errs, _) -> throwM $ CouldNotParsePackageSelectors errs
    resolved <- resolvePackages mSnapshotDef idents (Set.fromList names)
    ToFetchResult toFetch alreadyUnpacked <- getToFetch (Just dest') resolved
    unless (Map.null alreadyUnpacked) $
        throwM $ UnpackDirectoryAlreadyExists $ Set.fromList $ map toFilePath $ Map.elems alreadyUnpacked
    unpacked <- fetchPackages' Nothing toFetch
    F.forM_ (Map.toList unpacked) $ \(ident, dest'') -> logInfo $
        "Unpacked " <>
        fromString (packageIdentifierString ident) <>
        " to " <>
        fromString (toFilePath dest'')
  where
    -- Possible future enhancement: parse names as name + version range
    parse s =
        case parsePackageName t of
            Right x -> Right $ Left x
            Left _ ->
                case parsePackageIdentifierRevision t of
                    Right x -> Right $ Right x
                    Left _ -> Left s
      where
        t = T.pack s

-- | Same as 'unpackPackageIdents', but for a single package.
unpackPackageIdent
    :: HasCabalLoader env
    => Path Abs Dir -- ^ unpack directory
    -> Path Rel Dir -- ^ the dist rename directory, see: https://github.com/fpco/stack/issues/157
    -> PackageIdentifierRevision
    -> RIO env (Path Abs Dir)
unpackPackageIdent unpackDir distDir (PackageIdentifierRevision ident mcfi) = do
  -- FIXME make this more direct in the future
  m <- unpackPackageIdents unpackDir (Just distDir) [PackageIdentifierRevision ident mcfi]
  case Map.toList m of
    [(ident', dir)]
      | ident /= ident' -> error "unpackPackageIdent: ident mismatch"
      | otherwise       -> return dir
    [] -> error "unpackPackageIdent: empty list"
    _  -> error "unpackPackageIdent: multiple results"

-- | Ensure that all of the given package idents are unpacked into the build
-- unpack directory, and return the paths to all of the subdirectories.
unpackPackageIdents
    :: HasCabalLoader env
    => Path Abs Dir -- ^ unpack directory
    -> Maybe (Path Rel Dir) -- ^ the dist rename directory, see: https://github.com/fpco/stack/issues/157
    -> [PackageIdentifierRevision]
    -> RIO env (Map PackageIdentifier (Path Abs Dir))
unpackPackageIdents unpackDir mdistDir idents = do
    resolved <- resolvePackages Nothing idents Set.empty
    ToFetchResult toFetch alreadyUnpacked <- getToFetch (Just unpackDir) resolved
    nowUnpacked <- fetchPackages' mdistDir toFetch
    return $ alreadyUnpacked <> nowUnpacked

data ResolvedPackage = ResolvedPackage
    { rpIdent :: !PackageIdentifier
    , rpDownload :: !(Maybe PackageDownload)
    , rpOffsetSize :: !OffsetSize
    , rpIndex :: !PackageIndex
    }
    deriving Show

-- | Resolve a set of package names and identifiers into @FetchPackage@ values.
resolvePackages :: HasCabalLoader env
                => Maybe SnapshotDef -- ^ when looking up by name, take from this build plan
                -> [PackageIdentifierRevision]
                -> Set PackageName
                -> RIO env [ResolvedPackage]
resolvePackages mSnapshotDef idents0 names0 = do
    eres <- go
    case eres of
        Left _ -> do
            updateAllIndices
            go >>= either throwM return
        Right x -> return x
  where
    go = r <$> getUses00Index <*> resolvePackagesAllowMissing mSnapshotDef idents0 names0
    r uses00Index (missingNames, missingIdents, idents)
      | not $ Set.null missingNames  = Left $ UnknownPackageNames       missingNames
      | not $ HashSet.null missingIdents = Left $ UnknownPackageIdentifiers missingIdents "" uses00Index
      | otherwise                    = Right idents

-- | Does the configuration use a 00-index.tar.gz file for indices?
-- See <https://github.com/commercialhaskell/stack/issues/3520>
getUses00Index :: HasCabalLoader env => RIO env Bool
getUses00Index =
    any is00 <$> view (cabalLoaderL.to clIndices)
  where
    is00 :: PackageIndex -> Bool
    is00 index = "00-index.tar.gz" `T.isInfixOf` indexLocation index

-- | Turn package identifiers and package names into a list of
-- @ResolvedPackage@s. Returns any unresolved names and
-- identifier. These are considered unresolved even if the only
-- mismatch is in the cabal file info (MSS 2017-07-17: old versions of
-- this code had special handling to treat missing cabal file info as
-- a warning, that's no longer necessary or desirable since all info
-- should be present and checked).
resolvePackagesAllowMissing
    :: forall env. HasCabalLoader env
    => Maybe SnapshotDef -- ^ when looking up by name, take from this build plan
    -> [PackageIdentifierRevision]
    -> Set PackageName
    -> RIO env (Set PackageName, HashSet PackageIdentifierRevision, [ResolvedPackage])
resolvePackagesAllowMissing mSnapshotDef idents0 names0 = do
  cache@(PackageCache cache') <- getPackageCaches

      -- Find out the latest versions of all packages in the cache
  let versions = fmap (maximum . HashMap.keys) cache'

      -- Determines the identifier for a given name, either from
      -- snapshot information or by taking the latest version
      -- available
      getNamed :: PackageName -> Maybe PackageIdentifierRevision
      getNamed =
          case mSnapshotDef of
              Nothing -> getNamedFromIndex
              Just sd -> getNamedFromSnapshotDef sd

      -- Use whatever is specified in the snapshot. TODO this does not
      -- handle the case where a snapshot defines a package outside of
      -- the index, we'd need a LoadedSnapshot for that.
      getNamedFromSnapshotDef sd name = do
          loop $ sdLocations sd
        where
          loop [] = Nothing
          loop (PLIndex ident@(PackageIdentifierRevision (PackageIdentifier name' _) _):rest)
            | name == name' = Just ident
            | otherwise = loop rest
          loop (_:rest) = loop rest

      -- Take latest version available, including latest cabal file information
      getNamedFromIndex name = fmap
          (\ver -> PackageIdentifierRevision (PackageIdentifier name ver) CFILatest)
          (HashMap.lookup name versions)

      (missingNames, idents1) = partitionEithers $ map
          (\name -> maybe (Left name) Right (getNamed name))
          (Set.toList names0)
  cl <- view cabalLoaderL
  let (missingIdents, resolved) =
        partitionEithers
          $ map (\pir -> maybe (Left pir) Right (lookupResolvedPackage cl pir cache))
          $ idents0 <> idents1
  return (Set.fromList missingNames, HashSet.fromList missingIdents, resolved)

lookupResolvedPackage
  :: CabalLoader
  -> PackageIdentifierRevision
  -> PackageCache PackageIndex
  -> Maybe ResolvedPackage
lookupResolvedPackage cl (PackageIdentifierRevision ident@(PackageIdentifier name version) cfi) (PackageCache cache) = do
  (index, mdownload, files) <- HashMap.lookup name cache >>= HashMap.lookup version
  let moffsetSize =
        case cfi of
          CFILatest -> Just $ snd $ NE.last files
          CFIHash _msize hash' -> -- TODO check size?
              lookup hash'
            $ concatMap (\(hashes, x) -> map (, x) hashes)
            $ NE.toList files
          CFIRevision rev -> fmap snd $ listToMaybe $ drop (fromIntegral rev) $ NE.toList files
  offsetSize <-
    case moffsetSize of
      Just x -> Just x
      Nothing
        | clIgnoreRevisionMismatch cl -> Just $ snd $ NE.last files
        | otherwise -> Nothing
  Just ResolvedPackage
    { rpIdent = ident
    , rpDownload = mdownload
    , rpOffsetSize = offsetSize
    , rpIndex = index
    }

data ToFetch = ToFetch
    { tfTarball :: !(Path Abs File)
    , tfDestDir :: !(Maybe (Path Abs Dir))
    , tfUrl     :: !T.Text
    , tfSize    :: !(Maybe Word64)
    , tfSHA256  :: !(Maybe StaticSHA256)
    , tfCabal   :: !ByteString
    -- ^ Contents of the .cabal file
    }

data ToFetchResult = ToFetchResult
    { tfrToFetch         :: !(Map PackageIdentifier ToFetch)
    , tfrAlreadyUnpacked :: !(Map PackageIdentifier (Path Abs Dir))
    }

-- | Add the cabal files to a list of idents with their caches.
withCabalFiles
    :: HasCabalLoader env
    => IndexName
    -> [(ResolvedPackage, a)]
    -> (PackageIdentifier -> a -> ByteString -> IO b)
    -> RIO env [b]
withCabalFiles name pkgs f = do
    indexPath <- configPackageIndex name
    withBinaryFile (toFilePath indexPath) ReadMode
      $ \h -> mapM (goPkg h) pkgs
  where
    goPkg h (ResolvedPackage { rpIdent = ident, rpOffsetSize = OffsetSize offset size }, tf) = do
        -- Did not find warning for tarballs is handled above
        liftIO $ do
            hSeek h AbsoluteSeek $ fromIntegral offset
            cabalBS <- S.hGet h $ fromIntegral size
            f ident tf cabalBS

loadFromIndex :: HasCabalLoader env => PackageIdentifierRevision -> RIO env ByteString
loadFromIndex ident = do
  -- TODO in the future, keep all of the necessary @Handle@s open
  bothCaches <- getPackageCaches
  mres <- lookupPackageIdentifierExact ident bothCaches
  case mres of
      Just bs -> return bs
      -- Update the cache and try again
      Nothing -> do
          let fuzzy = fuzzyLookupCandidates ident bothCaches
              suggestions = case fuzzy of
                  FRNameNotFound Nothing -> ""
                  FRNameNotFound (Just cs) ->
                        "Perhaps you meant " <> orSeparated cs <> "?"
                  FRVersionNotFound cs -> "Possible candidates: " <>
                    commaSeparated (NE.map packageIdentifierText cs)
                    <> "."
                  FRRevisionNotFound cs ->
                    "The specified revision was not found.\nPossible candidates: " <>
                    commaSeparated (NE.map (T.pack . packageIdentifierRevisionString) cs)
                    <> "."
          cl <- view cabalLoaderL
          join $ modifyMVar (clUpdateRef cl) $ \toUpdate ->
              if toUpdate then do
                  logInfo $
                      "Didn't see " <>
                      fromString (packageIdentifierRevisionString ident) <>
                      " in your package indices.\n" <>
                      "Updating and trying again."
                  updateAllIndices
                  _ <- getPackageCaches
                  return (False, loadFromIndex ident)
              else do
                uses00Index <- getUses00Index
                return (toUpdate, throwIO $ UnknownPackageIdentifiers
                             (HashSet.singleton ident) (T.unpack suggestions) uses00Index)

lookupPackageIdentifierExact
  :: HasCabalLoader env
  => PackageIdentifierRevision
  -> PackageCache PackageIndex
  -> RIO env (Maybe ByteString)
lookupPackageIdentifierExact identRev cache = do
  cl <- view cabalLoaderL
  forM (lookupResolvedPackage cl identRev cache) $ \rp -> do
    [bs] <- withCabalFiles (indexName (rpIndex rp)) [(rp, ())] $ \_ _ bs -> return bs
    return bs

data FuzzyResults
  = FRNameNotFound !(Maybe (NonEmpty T.Text))
  | FRVersionNotFound !(NonEmpty PackageIdentifier)
  | FRRevisionNotFound !(NonEmpty PackageIdentifierRevision)

-- | Given package identifier and package caches, return list of packages
-- with the same name and the same two first version number components found
-- in the caches.
fuzzyLookupCandidates
  :: PackageIdentifierRevision
  -> PackageCache index
  -> FuzzyResults
fuzzyLookupCandidates (PackageIdentifierRevision (PackageIdentifier name ver) _rev) (PackageCache caches) =
  case HashMap.lookup name caches of
    Nothing -> FRNameNotFound $ typoCorrectionCandidates name (PackageCache caches)
    Just m ->
      case HashMap.lookup ver m of
        Nothing ->
          case NE.nonEmpty $ filter sameMajor $ HashMap.keys m of
            Just vers -> FRVersionNotFound $ NE.map (PackageIdentifier name) vers
            Nothing ->
              case NE.nonEmpty $ HashMap.keys m of
                Nothing -> error "fuzzyLookupCandidates: no versions"
                Just vers -> FRVersionNotFound $ NE.map (PackageIdentifier name) vers
        Just (_index, _mpd, revisions) ->
          let hashes = concatMap fst $ NE.toList revisions
              pirs = map (PackageIdentifierRevision (PackageIdentifier name ver) . CFIHash Nothing) hashes
           in case NE.nonEmpty pirs of
                Nothing -> error "fuzzyLookupCandidates: no revisions"
                Just pirs' -> FRRevisionNotFound pirs'
  where
    sameMajor v = toMajorVersion v == toMajorVersion ver

-- | Try to come up with typo corrections for given package identifier using
-- package caches. This should be called before giving up, i.e. when
-- 'fuzzyLookupCandidates' cannot return anything.
typoCorrectionCandidates
  :: PackageName
  -> PackageCache index
  -> Maybe (NonEmpty T.Text)
typoCorrectionCandidates name' (PackageCache cache) =
  let name = packageNameText name'
  in  NE.nonEmpty
    . take 10
    . map snd
    . filter (\(distance, _) -> distance < 4)
    . map (\k -> (damerauLevenshtein name (packageNameText k), packageNameText k))
    . HashMap.keys
    $ cache

-- | Figure out where to fetch from.
getToFetch :: HasCabalLoader env
           => Maybe (Path Abs Dir) -- ^ directory to unpack into, @Nothing@ means no unpack
           -> [ResolvedPackage]
           -> RIO env ToFetchResult
getToFetch mdest resolvedAll = do
    (toFetch0, unpacked) <- liftM partitionEithers $ mapM checkUnpacked resolvedAll
    toFetch1 <- mapM goIndex $ Map.toList $ Map.fromListWith (++) toFetch0
    return ToFetchResult
        { tfrToFetch = Map.unions toFetch1
        , tfrAlreadyUnpacked = Map.fromList unpacked
        }
  where
    checkUnpacked resolved = do
        let ident = rpIdent resolved
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
                    d = rpDownload resolved
                    targz = T.pack $ packageIdentifierString ident ++ ".tar.gz"
                tarball <- configPackageTarball (indexName index) ident
                return $ Left (indexName index, [(resolved, ToFetch
                    { tfTarball = tarball
                    , tfDestDir = mdestDir
                    , tfUrl = case fmap pdUrl d of
                        Just url | not (S.null url) -> decodeUtf8 url
                        _ -> indexDownloadPrefix index <> targz
                    , tfSize = fmap pdSize d
                    , tfSHA256 = fmap pdSHA256 d
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
fetchPackages' :: forall env. HasCabalLoader env
               => Maybe (Path Rel Dir) -- ^ the dist rename directory, see: https://github.com/fpco/stack/issues/157
               -> Map PackageIdentifier ToFetch
               -> RIO env (Map PackageIdentifier (Path Abs Dir))
fetchPackages' mdistDir toFetchAll = do
    connCount <- view $ cabalLoaderL.to clConnectionCount
    outputVar <- newTVarIO Map.empty

    parMapM_
        connCount
        (go outputVar)
        (Map.toList toFetchAll)

    readTVarIO outputVar
  where
    go :: TVar (Map PackageIdentifier (Path Abs Dir))
       -> (PackageIdentifier, ToFetch)
       -> RIO env ()
    go outputVar (ident, toFetch) = do
        req <- parseUrlThrow $ T.unpack $ tfUrl toFetch
        let destpath = tfTarball toFetch

        let toHashCheck bs = HashCheck SHA256 (CheckHexDigestByteString bs)
        let downloadReq = DownloadRequest
                { drRequest = req
                , drHashChecks = map (toHashCheck . staticSHA256ToBase16) $ maybeToList (tfSHA256 toFetch)
                , drLengthCheck = fromIntegral <$> tfSize toFetch
                , drRetryPolicy = drRetryPolicyDefault
                }
        let progressSink _ =
                logInfo $ display ident <> ": download"
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
                        FP.<.> "cabal"
                S.writeFile cabalFP $ tfCabal toFetch

                atomically $ modifyTVar outputVar $ Map.insert ident destDir

            F.forM_ unexpectedEntries $ \(path, entryType) ->
                logWarn $ "Unexpected entry type " <> display entryType <> " for entry " <> fromString path

-- | Internal function used to unpack tarball.
--
-- Takes a path to a .tar.gz file, the name of the directory it should contain,
-- and a destination folder to extract the tarball into. Returns unexpected
-- entries, as pairs of paths and descriptions.
untar :: forall b1 b2. Path b1 File -> Path Rel Dir -> Path b2 Dir -> IO [(FilePath, T.Text)]
untar tarPath expectedTarFolder destDirParent = do
  ensureDir destDirParent
  withLazyFile (toFilePath tarPath) $ \lbs -> do
                let rawEntries = fmap (either wrap wrap)
                            $ Tar.checkTarbomb (toFilePathNoTrailingSep expectedTarFolder)
                            $ Tar.read $ decompress lbs

                    filterEntries
                      :: (Semigroup w, Monoid w) => (Tar.Entry -> (Bool, w))
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

parMapM_ :: (F.Foldable f,MonadUnliftIO m)
         => Int
         -> (a -> m ())
         -> f a
         -> m ()
parMapM_ (max 1 -> 1) f xs = F.mapM_ f xs
parMapM_ cnt f xs0 = withRunInIO $ \run -> do
    var <- newTVarIO $ F.toList xs0

    replicateConcurrently_ cnt $ fix $ \loop -> join $ atomically $ do
      xs <- readTVar var
      case xs of
          [] -> return $ return ()
          x:xs' -> do
              writeTVar var xs'
              return $ do
                  run $ f x
                  loop

orSeparated :: NonEmpty T.Text -> T.Text
orSeparated xs
  | NE.length xs == 1 = NE.head xs
  | NE.length xs == 2 = NE.head xs <> " or " <> NE.last xs
  | otherwise = T.intercalate ", " (NE.init xs) <> ", or " <> NE.last xs

commaSeparated :: NonEmpty T.Text -> T.Text
commaSeparated = F.fold . NE.intersperse ", "

-- | Location of a package tarball
configPackageTarball :: HasCabalLoader env => IndexName -> PackageIdentifier -> RIO env (Path Abs File)
configPackageTarball iname ident = do
    root <- configPackageIndexRoot iname
    name <- parseRelDir $ packageNameString $ packageIdentifierName ident
    ver <- parseRelDir $ versionString $ packageIdentifierVersion ident
    base <- parseRelFile $ packageIdentifierString ident ++ ".tar.gz"
    return (root </> $(mkRelDir "packages") </> name </> ver </> base)
