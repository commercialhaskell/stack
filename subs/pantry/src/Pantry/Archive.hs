{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Logic for loading up trees from HTTPS archives.
module Pantry.Archive
  ( getArchive
  , getArchiveKey
  , fetchArchives
  , withArchiveLoc
  ) where

import RIO
import RIO.FilePath (normalise, takeDirectory, (</>))
import Pantry.StaticSHA256
import Pantry.Storage
import Pantry.Tree
import Pantry.Types
import qualified RIO.Text as T
import qualified RIO.List as List
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Data.Bits ((.&.))
import Path (toFilePath)
import qualified Codec.Archive.Zip as Zip

import Conduit
import Crypto.Hash.Conduit
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit.Tar as Tar
import Network.HTTP.Client (parseUrlThrow)
import Network.HTTP.Simple (httpSink)

fetchArchives
  :: (HasPantryConfig env, HasLogFunc env)
  => [(Archive, PackageMetadata)]
  -> RIO env ()
fetchArchives pairs = do
  -- FIXME be more efficient, group together shared archives
  for_ pairs $ uncurry getArchive

getArchiveKey
  :: forall env. (HasPantryConfig env, HasLogFunc env)
  => Archive
  -> PackageMetadata
  -> RIO env TreeKey
getArchiveKey archive pm = fst <$> getArchive archive pm -- potential optimization

getArchive
  :: forall env. (HasPantryConfig env, HasLogFunc env)
  => Archive
  -> PackageMetadata
  -> RIO env (TreeKey, Tree)
getArchive archive pm =
  checkPackageMetadata (PLArchive archive pm) pm $
  withCache $
  withArchiveLoc archive $ \fp sha size -> do
    (tid, key, tree) <- parseArchive loc fp subdir
    pure (tid, sha, size, key, tree)
  where
    msha = archiveHash archive
    msize = archiveSize archive
    subdir = pmSubdir pm
    loc = archiveLocation archive

    withCache
      :: RIO env (TreeSId, StaticSHA256, FileSize, TreeKey, Tree)
      -> RIO env (TreeKey, Tree)
    withCache inner =
      let loop [] = do
            (tid, sha, size, treeKey, tree) <- inner
            case loc of
              ALUrl url -> withStorage $ storeArchiveCache url subdir sha size tid
              ALFilePath _ -> pure ()
            pure (treeKey, tree)
          loop ((sha, size, tid):rest) =
            case msha of
              Nothing -> do
                case msize of
                  Just size' | size /= size' -> loop rest
                  _ -> do
                    case loc of
                      ALUrl url -> do
                        logWarn $ "Using archive from " <> display url <> "without a specified cryptographic hash"
                        logWarn $ "Cached hash is " <> display sha <> ", file size " <> display size
                        logWarn "For security and reproducibility, please add a hash and file size to your configuration"
                      ALFilePath _ -> pure ()
                    withStorage $ loadTreeById tid
              Just sha'
                | sha == sha' ->
                    case msize of
                      Nothing -> do
                        case loc of
                          ALUrl url -> do
                            logWarn $ "Archive from " <> display url <> " does not specify a size"
                            logWarn $ "To avoid an overflow attack, please add the file size to your configuration: " <> display size
                          ALFilePath _ -> pure ()
                        withStorage $ loadTreeById tid
                      Just size'
                        | size == size' -> withStorage $ loadTreeById tid
                        | otherwise -> do

                            logWarn $ "Archive from " <> display loc <> " has a matching hash but mismatched size"
                            logWarn "Please verify that your configuration provides the correct size"
                            loop rest
                | otherwise -> loop rest
       in case loc of
            ALUrl url -> withStorage (loadArchiveCache url subdir) >>= loop
            ALFilePath _ -> loop []

withArchiveLoc
  :: HasLogFunc env
  => Archive
  -> (FilePath -> StaticSHA256 -> FileSize -> RIO env a)
  -> RIO env a
withArchiveLoc (Archive (ALFilePath resolved) msha msize) f = do
  let fp = toFilePath $ resolvedAbsolute resolved
  (sha, size) <- withBinaryFile fp ReadMode $ \h -> do
    size <- FileSize . fromIntegral <$> hFileSize h
    for_ msize $ \size' -> when (size /= size') $ error $ "Mismatched local archive size: " ++ show (resolved, size, size')

    sha <- mkStaticSHA256FromDigest <$> runConduit (sourceHandle h .| sinkHash)
    for_ msha $ \sha' -> when (sha /= sha') $ error $ "Mismatched local archive sha: " ++ show (resolved, sha, sha')

    pure (sha, size)
  f fp sha size
withArchiveLoc (Archive (ALUrl url) msha msize) f =
  withSystemTempFile "archive" $ \fp hout -> do
    req <- parseUrlThrow $ T.unpack url
    logDebug $ "Downloading archive from " <> display url
    (sha, size, ()) <- httpSink req $ const $ getZipSink $ (,,)
      <$> ZipSink (checkSha msha)
      <*> ZipSink (checkSize $ (\(FileSize w) -> w) <$> msize)
      <*> ZipSink (sinkHandle hout)
    hClose hout
    f fp sha (FileSize size)
  where
    checkSha mexpected = do
      actual <- mkStaticSHA256FromDigest <$> sinkHash
      for_ mexpected $ \expected -> unless (actual == expected) $ error $ concat
        [ "Invalid SHA256 downloading from "
        , T.unpack url
        , ". Expected: "
        , show expected
        , ". Actual: "
        , show actual
        ]
      pure actual
    checkSize mexpected =
      loop 0
      where
        loop accum = do
          mbs <- await
          case mbs of
            Nothing ->
              case mexpected of
                Just expected | expected /= accum -> error $ concat
                    [ "Invalid file size downloading from "
                    , T.unpack url
                    , ". Expected: "
                    , show expected
                    , ". Actual: "
                    , show accum
                    ]
                _ -> pure accum
            Just bs -> do
              let accum' = accum + fromIntegral (B.length bs)
              case mexpected of
                Just expected
                  | accum' > expected -> error $ concat
                    [ "Invalid file size downloading from "
                    , T.unpack url
                    , ". Expected: "
                    , show expected
                    , ", but file is at least: "
                    , show accum'
                    ]
                _ -> loop accum'

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
  => FilePath
  -> ArchiveType
  -> a
  -> (a -> MetaEntry -> ConduitT ByteString Void (RIO env) a)
  -> RIO env a
foldArchive fp ATTarGz accum f =
  withSourceFile fp $ \src -> runConduit $ src .| ungzip .| foldTar accum f
foldArchive fp ATTar accum f =
  withSourceFile fp $ \src -> runConduit $ src .| foldTar accum f
foldArchive fp ATZip accum0 f = withBinaryFile fp ReadMode $ \h -> do
  -- We're entering lazy I/O land thanks to zip-archive.
  lbs <- BL.hGetContents h
  let go accum entry = do
        let me = MetaEntry (Zip.eRelativePath entry) met
            met = METNormal -- FIXME determine this correctly
        -- FIXME check crc32
        runConduit $ sourceLazy (Zip.fromEntry entry) .| f accum me
  foldM go accum0 (Zip.zEntries $ Zip.toArchive lbs)

foldTar
  :: (HasPantryConfig env, HasLogFunc env)
  => a
  -> (a -> MetaEntry -> ConduitT ByteString o (RIO env) a)
  -> ConduitT ByteString o (RIO env) a
foldTar accum0 f = do
  ref <- newIORef accum0
  Tar.untar $ \fi -> for_ (toME fi) $ \me -> do
    accum <- readIORef ref
    accum' <- f accum me
    writeIORef ref $! accum'
  readIORef ref
  where
    toME :: Tar.FileInfo -> Maybe MetaEntry
    toME fi = do
      met <-
        case Tar.fileType fi of
          Tar.FTSymbolicLink bs ->
            case decodeUtf8' bs of
              Left e -> error $ "Need to handle this case better! " ++ show e
              Right text -> Just $ METLink $ T.unpack text
          Tar.FTNormal -> Just $
            if Tar.fileMode fi .&. 0o100 /= 0
              then METExecutable
              else METNormal
          Tar.FTDirectory -> Nothing
          _ -> Nothing
      Just MetaEntry
        { mePath = Tar.getFileInfoPath fi
        , meType = met
        }

data SimpleEntry = SimpleEntry
  { seSource :: !FilePath
  , seType :: !FileType
  }
  deriving Show

parseArchive
  :: (HasPantryConfig env, HasLogFunc env)
  => ArchiveLocation
  -> FilePath -- ^ file holding the archive
  -> Text -- ^ subdir, besides the single-dir stripping logic
  -> RIO env (TreeSId, TreeKey, Tree)
parseArchive loc fp subdir = do
  let getFiles [] = error $ T.unpack $ utf8BuilderToText $ "Unable to determine archive type of: " <> display loc
      getFiles (at:ats) = do
        eres <- tryAny $ foldArchive fp at id $ \m me -> pure $ m . (me:)
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
          METLink relDest ->
            let dest = map toSlash $ normalise $ takeDirectory (mePath me) </> relDest
                toSlash '\\' = '/'
                toSlash c = c
             in case Map.lookup dest files of
                  Nothing -> Left $ "Symbolic link dest not found from " ++ mePath me ++ " to " ++ relDest
                  Just me' ->
                    case meType me' of
                      METNormal -> Right $ SimpleEntry dest FTNormal
                      METExecutable -> Right $ SimpleEntry dest FTExecutable
                      METLink _ -> Left $ "Symbolic link dest cannot be a symbolic link, from " ++ mePath me ++ " to " ++ relDest

  case traverse toSimple files of
    Left e ->
      error $ T.unpack $ utf8BuilderToText $ "Unsupported tarball from " <> display loc <> ": " <> fromString e
    Right files1 -> do
      let files2 = stripCommonPrefix $ Map.toList files1
          files3 = takeSubdir subdir files2
          toSafe (fp', a) =
            case mkSafeFilePath fp' of
              Nothing -> Left $ "Not a safe file path: " ++ T.unpack fp'
              Just sfp -> Right (sfp, a)
      case traverse toSafe files3 of
        Left e -> error $ T.unpack $ utf8BuilderToText $ "Unsupported tarball from " <> display loc <> ": " <> fromString e
        Right safeFiles -> do
          let toSave = Set.fromList $ map (seSource . snd) safeFiles
          blobs <-
            foldArchive fp at mempty $ \m me ->
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
          (tid, treeKey) <- withStorage $ storeTree tree
          pure (tid, treeKey, tree)

stripCommonPrefix :: [(FilePath, a)] -> [(FilePath, a)]
stripCommonPrefix [] = []
stripCommonPrefix pairs@((firstFP, _):_) = fromMaybe pairs $ do
  let firstDir = takeWhile (/= '/') firstFP
  guard $ not $ null firstDir
  let strip (fp, a) = (, a) <$> List.stripPrefix (firstDir ++ "/") fp
  traverse strip pairs

takeSubdir :: Text -> [(FilePath, a)] -> [(Text, a)]
takeSubdir subdir = mapMaybe $ \(fp, a) -> do
  stripped <- T.stripPrefix subdir $ T.pack fp
  Just (T.dropWhile (== '/') stripped, a)
