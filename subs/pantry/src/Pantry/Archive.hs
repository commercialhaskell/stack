{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- | Logic for loading up trees from HTTPS archives.
module Pantry.Archive
  ( getArchive
  ) where

import RIO
import RIO.FilePath (normalise, takeDirectory, (</>))
import Pantry.StaticSHA256
import Pantry.Storage
import Pantry.Types
import qualified RIO.Text as T
import qualified RIO.List as List
import qualified RIO.ByteString as B
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Data.Bits ((.&.))

import Conduit
import Crypto.Hash.Conduit
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit.Tar as Tar
import qualified Codec.Archive.Zip as Zip
import Network.HTTP.Client (parseUrlThrow)
import Network.HTTP.Simple (httpSink)

getArchive
  :: (HasPantryConfig env, HasLogFunc env)
  => Text -- ^ URL
  -> Text -- ^ subdir, besides the single-dir stripping logic
  -> Maybe StaticSHA256 -- ^ hash of the raw file
  -> Maybe FileSize -- ^ size of the raw file
  -> RIO env (TreeKey, Tree)
-- FIXME add caching in DB
getArchive url subdir msha msize = withSystemTempFile "archive" $ \fp hout -> do
  req <- parseUrlThrow $ T.unpack url
  logDebug $ "Downloading archive from " <> display url
  httpSink req $ const $ getZipSink $
    maybe id (\(FileSize size) -> (ZipSink (checkSize size) *>)) msize $
    maybe id (\sha -> (ZipSink (checkSha sha) *>)) msha $
    ZipSink (sinkHandle hout)
  hClose hout

  parseArchive url fp subdir
  where
    checkSha expected = do
      actual <- mkStaticSHA256FromDigest <$> sinkHash
      unless (actual == expected) $ error $ concat
        [ "Invalid SHA256 downloading from "
        , T.unpack url
        , ". Expected: "
        , show expected
        , ". Actual: "
        , show actual
        ]
    checkSize expected =
      loop 0
      where
        loop accum = do
          mbs <- await
          case mbs of
            Nothing
              | accum == expected -> pure ()
              | otherwise -> error $ concat
                    [ "Invalid file size downloading from "
                    , T.unpack url
                    , ". Expected: "
                    , show expected
                    , ". Actual: "
                    , show accum
                    ]
            Just bs -> do
              let accum' = accum + fromIntegral (B.length bs)
              if accum' > expected
                then error $ concat
                    [ "Invalid file size downloading from "
                    , T.unpack url
                    , ". Expected: "
                    , show expected
                    , ", but file is at least: "
                    , show accum'
                    ]
                else loop accum'

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
foldArchive fp ATZip accum f = undefined
  -- We're entering lazy I/O land thanks to zip-archive. We'll do a
  -- first pass through to get all the files, determine renamings and
  -- so on, and then a second pass to grab the blobs we need.

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
  => Text -- ^ URL, for error output
  -> FilePath -- ^ file holding the archive
  -> Text -- ^ subdir, besides the single-dir stripping logic
  -> RIO env (TreeKey, Tree)
parseArchive url fp subdir = do
  let getFiles [] = error $ "Unable to determine archive type of: " ++ T.unpack url
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
      error $ "Unsupported tarball from " ++ T.unpack url ++ ": " ++ e
    Right files1 -> do
      let files2 = stripCommonPrefix $ Map.toList files1
          files3 = takeSubdir subdir files2
          toSafe (fp, a) =
            case mkSafeFilePath fp of
              Nothing -> Left $ "Not a safe file path: " ++ T.unpack fp
              Just sfp -> Right (sfp, a)
      case traverse toSafe files3 of
        Left e -> error $ "Unsupported tarball from " ++ T.unpack url ++ ": " ++ e
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
          (_tid, treeKey) <- withStorage $ storeTree tree
          pure (treeKey, tree)

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
