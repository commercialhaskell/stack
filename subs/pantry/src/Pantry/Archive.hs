{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Logic for loading up trees from HTTPS archives.
module Pantry.Archive
  ( getArchive
  , getArchiveKey
  , fetchArchivesRaw
  , fetchArchives
  , withArchiveLoc
  ) where

import RIO
import qualified Pantry.SHA256 as SHA256
import Pantry.Storage
import Pantry.Tree
import Pantry.Types
import Pantry.Internal (normalizeParents, makeTarRelative)
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T
import qualified RIO.List as List
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Data.Bits ((.&.), shiftR)
import Path (toFilePath)
import qualified Codec.Archive.Zip as Zip
import qualified Data.Digest.CRC32 as CRC32
import Distribution.PackageDescription (packageDescription, package)

import Conduit
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit.Tar as Tar
import Pantry.HTTP

fetchArchivesRaw
  :: (HasPantryConfig env, HasLogFunc env)
  => [(RawArchive, RawPackageMetadata)]
  -> RIO env ()
fetchArchivesRaw pairs =
  for_ pairs $ \(ra, rpm) ->
    getArchive (RPLIArchive ra rpm) ra rpm

fetchArchives
  :: (HasPantryConfig env, HasLogFunc env)
  => [(Archive, PackageMetadata)]
  -> RIO env ()
fetchArchives pairs =
  -- TODO be more efficient, group together shared archives
  fetchArchivesRaw [
    let PackageIdentifier nm ver = pmIdent pm
        rpm = RawPackageMetadata (Just nm) (Just ver) (Just $ pmTreeKey pm) (Just $ pmCabal pm)
        ra = RawArchive (archiveLocation a) (Just $ archiveHash a) (Just $ archiveSize a) (archiveSubdir a)
    in (ra, rpm)
   | (a, pm) <- pairs]

getArchiveKey
  :: forall env. (HasPantryConfig env, HasLogFunc env)
  => RawPackageLocationImmutable -- ^ for exceptions
  -> RawArchive
  -> RawPackageMetadata
  -> RIO env TreeKey
getArchiveKey rpli archive rpm = packageTreeKey <$> getArchive rpli archive rpm -- potential optimization

getArchive
  :: forall env. (HasPantryConfig env, HasLogFunc env)
  => RawPackageLocationImmutable -- ^ for exceptions
  -> RawArchive
  -> RawPackageMetadata
  -> RIO env Package
getArchive rpli archive rpm = do
  -- Check if the value is in the archive, and use it if possible
  mpa <- loadCache archive
  pa <-
    case mpa of
      Just pa -> pure pa
      -- Not in the archive. Load the archive. Completely ignore the
      -- PackageMetadata for now, we'll check that the Package
      -- info matches next.
      Nothing -> withArchiveLoc archive $ \fp sha size -> do
        pa <- parseArchive rpli archive fp
        -- Storing in the cache exclusively uses information we have
        -- about the archive itself, not metadata from the user.
        storeCache archive sha size pa
        pure pa

  either throwIO pure $ checkPackageMetadata rpli rpm pa

storeCache
  :: forall env. (HasPantryConfig env, HasLogFunc env)
  => RawArchive
  -> SHA256
  -> FileSize
  -> Package
  -> RIO env ()
storeCache archive sha size pa =
  case raLocation archive of
    ALUrl url -> withStorage $ storeArchiveCache url (raSubdir archive) sha size (packageTreeKey pa)
    ALFilePath _ -> pure () -- TODO cache local as well

loadCache
  :: forall env. (HasPantryConfig env, HasLogFunc env)
  => RawArchive
  -> RIO env (Maybe Package)
loadCache archive =
  case loc of
    ALFilePath _ -> pure Nothing -- TODO can we do something intelligent here?
    ALUrl url -> withStorage (loadArchiveCache url (raSubdir archive)) >>= loop
  where
    loc = raLocation archive
    msha = raHash archive
    msize = raSize archive

    loadFromCache :: TreeId -> RIO env (Maybe Package)
    loadFromCache tid = fmap Just $ withStorage $ loadPackageById tid

    loop [] = pure Nothing
    loop ((sha, size, tid):rest) =
      case msha of
        Nothing -> do
          case msize of
            Just size' | size /= size' -> loop rest
            _ -> do
              case loc of
                ALUrl url -> do
                  logWarn $ "Using archive from " <> display url <> " without a specified cryptographic hash"
                  logWarn $ "Cached hash is " <> display sha <> ", file size " <> display size
                  logWarn "For security and reproducibility, please add a hash and file size to your configuration"
                ALFilePath _ -> pure ()
              loadFromCache tid
        Just sha'
          | sha == sha' ->
              case msize of
                Nothing -> do
                  case loc of
                    ALUrl url -> do
                      logWarn $ "Archive from " <> display url <> " does not specify a size"
                      logWarn $ "To avoid an overflow attack, please add the file size to your configuration: " <> display size
                    ALFilePath _ -> pure ()
                  loadFromCache tid
                Just size'
                  | size == size' -> loadFromCache tid
                  | otherwise -> do

                      logWarn $ "Archive from " <> display loc <> " has a matching hash but mismatched size"
                      logWarn "Please verify that your configuration provides the correct size"
                      loop rest
          | otherwise -> loop rest

-- ensure name, version, etc are correct
checkPackageMetadata
  :: RawPackageLocationImmutable
  -> RawPackageMetadata
  -> Package
  -> Either PantryException Package
checkPackageMetadata rpli rpm pa = do
  let err = MismatchedPackageMetadata
              rpli
              rpm
              (Just (packageTreeKey pa))
              (teBlob $ packageCabalEntry pa)
              (packageIdent pa)
      test (Just x) y = x == y
      test Nothing _ = True

      tests =
        [ test (rpmTreeKey rpm) (packageTreeKey pa)
        , test (rpmName rpm) (pkgName $ packageIdent pa)
        , test (rpmVersion rpm) (pkgVersion $ packageIdent pa)
        , test (rpmCabal rpm) (teBlob $ packageCabalEntry pa)
        ]

   in if and tests then Right pa else Left err

-- | Provide a local file with the contents of the archive, regardless
-- of where it comes from. Perform SHA256 and file size validation if
-- downloading.
withArchiveLoc
  :: HasLogFunc env
  => RawArchive
  -> (FilePath -> SHA256 -> FileSize -> RIO env a)
  -> RIO env a
withArchiveLoc (RawArchive (ALFilePath resolved) msha msize _subdir) f = do
  let abs' = resolvedAbsolute resolved
      fp = toFilePath abs'
  (sha, size) <- withBinaryFile fp ReadMode $ \h -> do
    size <- FileSize . fromIntegral <$> hFileSize h
    for_ msize $ \size' -> when (size /= size') $ throwIO $ LocalInvalidSize abs' Mismatch
      { mismatchExpected = size'
      , mismatchActual = size
      }

    sha <- runConduit (sourceHandle h .| SHA256.sinkHash)
    for_ msha $ \sha' -> when (sha /= sha') $ throwIO $ LocalInvalidSHA256 abs' Mismatch
      { mismatchExpected = sha'
      , mismatchActual = sha
      }

    pure (sha, size)
  f fp sha size
withArchiveLoc (RawArchive (ALUrl url) msha msize _subdir) f =
  withSystemTempFile "archive" $ \fp hout -> do
    logDebug $ "Downloading archive from " <> display url
    (sha, size, ()) <- httpSinkChecked url msha msize (sinkHandle hout)
    hClose hout
    f fp sha size

data ArchiveType = ATTarGz | ATTar | ATZip
  deriving (Enum, Bounded)

instance Display ArchiveType where
  display ATTarGz = "GZIP-ed tar file"
  display ATTar = "Uncompressed tar file"
  display ATZip = "Zip file"

data METype
  = METNormal
  | METExecutable
  | METLink !FilePath
  deriving Show

data MetaEntry = MetaEntry
  { mePath :: !FilePath
  , meType :: !METype
  }
  deriving Show

foldArchive
  :: (HasPantryConfig env, HasLogFunc env)
  => ArchiveLocation -- ^ for error reporting
  -> FilePath
  -> ArchiveType
  -> a
  -> (a -> MetaEntry -> ConduitT ByteString Void (RIO env) a)
  -> RIO env a
foldArchive loc fp ATTarGz accum f =
  withSourceFile fp $ \src -> runConduit $ src .| ungzip .| foldTar loc accum f
foldArchive loc fp ATTar accum f =
  withSourceFile fp $ \src -> runConduit $ src .| foldTar loc accum f
foldArchive loc fp ATZip accum0 f = withBinaryFile fp ReadMode $ \h -> do
  let go accum entry = do
        let me = MetaEntry (Zip.eRelativePath entry) met
            met = fromMaybe METNormal $ do
              let modes = shiftR (Zip.eExternalFileAttributes entry) 16
              guard $ Zip.eVersionMadeBy entry .&. 0xFF00 == 0x0300
              guard $ modes /= 0
              Just $
                if (modes .&. 0o100) == 0
                  then METNormal
                  else METExecutable
            lbs = Zip.fromEntry entry
        let crcExpected = Zip.eCRC32 entry
            crcActual = CRC32.crc32 lbs
        when (crcExpected /= crcActual)
          $ throwIO $ CRC32Mismatch loc (Zip.eRelativePath entry) Mismatch
              { mismatchExpected = crcExpected
              , mismatchActual = crcActual
              }
        runConduit $ sourceLazy lbs .| f accum me
      isDir entry =
        case reverse $ Zip.eRelativePath entry of
          '/':_ -> True
          _ -> False
  -- We're entering lazy I/O land thanks to zip-archive.
  lbs <- BL.hGetContents h
  foldM go accum0 (filter (not . isDir) $ Zip.zEntries $ Zip.toArchive lbs)

foldTar
  :: (HasPantryConfig env, HasLogFunc env)
  => ArchiveLocation -- ^ for exceptions
  -> a
  -> (a -> MetaEntry -> ConduitT ByteString o (RIO env) a)
  -> ConduitT ByteString o (RIO env) a
foldTar loc accum0 f = do
  ref <- newIORef accum0
  Tar.untar $ \fi -> toME fi >>= traverse_ (\me -> do
    accum <- readIORef ref
    accum' <- f accum me
    writeIORef ref $! accum')
  readIORef ref
  where
    toME :: MonadIO m => Tar.FileInfo -> m (Maybe MetaEntry)
    toME fi = do
      let exc = InvalidTarFileType loc (Tar.getFileInfoPath fi) (Tar.fileType fi)
      mmet <-
        case Tar.fileType fi of
          Tar.FTSymbolicLink bs ->
            case decodeUtf8' bs of
              Left _ -> throwIO exc
              Right text -> pure $ Just $ METLink $ T.unpack text
          Tar.FTNormal -> pure $ Just $
            if Tar.fileMode fi .&. 0o100 /= 0
              then METExecutable
              else METNormal
          Tar.FTDirectory -> pure Nothing
          _ -> throwIO exc
      pure $
        (\met -> MetaEntry
          { mePath = Tar.getFileInfoPath fi
          , meType = met
          })
        <$> mmet

data SimpleEntry = SimpleEntry
  { seSource :: !FilePath
  , seType :: !FileType
  }
  deriving Show

-- | Attempt to parse the contents of the given archive in the given
-- subdir into a 'Tree'. This will not consult any caches. It will
-- ensure that:
--
-- * The cabal file exists
--
-- * The cabal file can be parsed
--
-- * The name inside the cabal file matches the name of the cabal file itself
parseArchive
  :: (HasPantryConfig env, HasLogFunc env)
  => RawPackageLocationImmutable
  -> RawArchive
  -> FilePath -- ^ file holding the archive
  -> RIO env Package
parseArchive rpli archive fp = do
  let loc = raLocation archive
      getFiles [] = throwIO $ UnknownArchiveType loc
      getFiles (at:ats) = do
        eres <- tryAny $ foldArchive loc fp at id $ \m me -> pure $ m . (me:)
        case eres of
          Left e -> do
            logDebug $ "parseArchive of " <> display at <> ": " <> displayShow e
            getFiles ats
          Right files -> pure (at, Map.fromList $ map (mePath &&& id) $ files [])
  (at, files) <- getFiles [minBound..maxBound]

  let toSimple :: MetaEntry -> Either String SimpleEntry
      toSimple me =
        case meType me of
          METNormal -> Right $ SimpleEntry (mePath me) FTNormal
          METExecutable -> Right $ SimpleEntry (mePath me) FTExecutable
          METLink relDest -> do
            case relDest of
              '/':_ -> Left $ "Cannot have an absolute relative dest: " ++ relDest
              _ -> Right ()
            dest0 <-
              case makeTarRelative (mePath me) relDest of
                Left e -> Left $ concat
                  [ "Error resolving relative path "
                  , relDest
                  , " from symlink at "
                  , mePath me
                  , ": "
                  , e
                  ]
                Right x -> Right x
            dest <-
              case normalizeParents dest0 of
                Left e -> Left $ concat
                  [ "Invalid symbolic link from "
                  , mePath me
                  , " to "
                  , relDest
                  , ", tried parsing "
                  , dest0
                  , ": "
                  , e
                  ]
                Right x -> Right x
            case Map.lookup dest files of
              Nothing -> Left $ "Symbolic link dest not found from " ++ mePath me ++ " to " ++ relDest ++ ", looking for " ++ dest
              Just me' ->
                case meType me' of
                  METNormal -> Right $ SimpleEntry dest FTNormal
                  METExecutable -> Right $ SimpleEntry dest FTExecutable
                  METLink _ -> Left $ "Symbolic link dest cannot be a symbolic link, from " ++ mePath me ++ " to " ++ relDest

  case traverse toSimple files of
    Left e -> throwIO $ UnsupportedTarball loc $ T.pack e
    Right files1 -> do
      let files2 = stripCommonPrefix $ Map.toList files1
          files3 = takeSubdir (raSubdir archive) files2
          toSafe (fp', a) =
            case mkSafeFilePath fp' of
              Nothing -> Left $ "Not a safe file path: " ++ show fp'
              Just sfp -> Right (sfp, a)
      case traverse toSafe files3 of
        Left e -> throwIO $ UnsupportedTarball loc $ T.pack e
        Right safeFiles -> do
          let toSave = Set.fromList $ map (seSource . snd) safeFiles
          blobs <-
            foldArchive loc fp at mempty $ \m me ->
              if mePath me `Set.member` toSave
                then do
                  bs <- mconcat <$> sinkList
                  (_, blobKey) <- lift $ withStorage $ storeBlob bs
                  pure $ Map.insert (mePath me) blobKey m
                else pure m
          tree <- fmap (TreeMap . Map.fromList) $ for safeFiles $ \(sfp, se) ->
            case Map.lookup (seSource se) blobs of
              Nothing -> error $ "Impossible: blob not found for: " ++ seSource se
              Just blobKey -> pure (sfp, TreeEntry blobKey (seType se))

          -- parse the cabal file and ensure it has the right name
          (cabalPath, cabalEntry@(TreeEntry cabalBlobKey _)) <- findCabalFile rpli tree
          mbs <- withStorage $ loadBlob cabalBlobKey
          bs <-
            case mbs of
              Nothing -> throwIO $ TreeReferencesMissingBlob rpli cabalPath cabalBlobKey
              Just bs -> pure bs
          (_warnings, gpd) <- rawParseGPD (Left rpli) bs
          let ident@(PackageIdentifier name _) = package $ packageDescription gpd
          when (cabalPath /= cabalFileName name) $
            throwIO $ WrongCabalFileName rpli cabalPath name

          -- It's good! Store the tree, let's bounce
          (_tid, treeKey) <- withStorage $ storeTree ident tree cabalEntry
          pure Package
            { packageTreeKey = treeKey
            , packageTree = tree
            , packageCabalEntry = cabalEntry
            , packageIdent = ident
            }

findCabalFile
  :: MonadThrow m
  => RawPackageLocationImmutable -- ^ for exceptions
  -> Tree
  -> m (SafeFilePath, TreeEntry)
findCabalFile loc (TreeMap m) = do
  let isCabalFile (sfp, _) =
        let txt = unSafeFilePath sfp
         in not ("/" `T.isInfixOf` txt) && ".cabal" `T.isSuffixOf` txt
  case filter isCabalFile $ Map.toList m of
    [] -> throwM $ TreeWithoutCabalFile loc
    [(key, te)] -> pure (key, te)
    xs -> throwM $ TreeWithMultipleCabalFiles loc $ map fst xs

-- | If all files have a shared prefix, strip it off
stripCommonPrefix :: [(FilePath, a)] -> [(FilePath, a)]
stripCommonPrefix [] = []
stripCommonPrefix pairs@((firstFP, _):_) = fromMaybe pairs $ do
  let firstDir = takeWhile (/= '/') firstFP
  guard $ not $ null firstDir
  let strip (fp, a) = (, a) <$> List.stripPrefix (firstDir ++ "/") fp
  stripCommonPrefix <$> traverse strip pairs

-- | Take us down to the specified subdirectory
takeSubdir
  :: Text -- ^ subdir
  -> [(FilePath, a)] -- ^ files after stripping common prefix
  -> [(Text, a)]
takeSubdir subdir = mapMaybe $ \(fp, a) -> do
  stripped <- List.stripPrefix subdirs $ splitDirs $ T.pack fp
  Just (T.intercalate "/" stripped, a)
  where
    splitDirs = List.dropWhile (== ".") . filter (/= "") . T.splitOn "/"
    subdirs = splitDirs subdir
