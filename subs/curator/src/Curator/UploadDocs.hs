{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Curator.UploadDocs
    ( uploadDocs
    ) where

import Conduit
import Control.Monad.Trans.Resource (liftResourceT)
import Crypto.Hash (Digest, SHA256)
import Crypto.Hash.Conduit (sinkHash)
import Data.ByteArray.Encoding (convertToBase, Base(..))
import Data.Conduit.Zlib (WindowBits(WindowBits), compress)
import Data.XML.Types
       (Content(ContentText),
        Event(EventBeginDoctype, EventBeginElement, EventEndDoctype), Name)
import Distribution.Package (PackageIdentifier(..))
import Network.AWS
       (Credentials(Discover), Env, newEnv, runAWS, send, toBody)
import Network.AWS.S3
       (BucketName(..), ObjectCannedACL(OPublicRead), ObjectKey(..),
        poACL, poCacheControl, poContentEncoding, poContentType, putObject)
import Network.Mime (defaultMimeLookup)
import Pantry hiding (SHA256)
import RIO
import qualified RIO.Directory as D
import qualified RIO.FilePath as F
import RIO.List (stripPrefix)
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as T
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.HTML.DOM (eventConduit)
import Text.XML (fromEvents)


import qualified Codec.Archive.Tar             as Tar
import qualified Codec.Archive.Tar.Entry       as Tar

type M = ResourceT (RIO UploadEnv)

uploadDocs :: (HasLogFunc env)
           => FilePath -- ^ directory containing docs
           -> Text -- ^ name of current docs, used as prefix in object names
           -> Text -- ^ bucket name
           -> RIO env ()
uploadDocs input' name bucket = do
    env' <- newEnv Discover

    unlessM (D.doesDirectoryExist input') $ error $ "Could not find directory: " ++ show input'
    input <- fmap (F.</> "") $ D.canonicalizePath input'

    let inner :: M ()
        inner = do
          let threads = 16
              size = threads * 2
          queue <- liftIO $ newTBQueueIO (fromIntegral size)
          isOpenVar <- liftIO $ newTVarIO True
          let readIO' = liftIO $ atomically $
                  (Just <$> readTBQueue queue) <|>
                  (do isOpen <- readTVar isOpenVar
                      checkSTM $ not isOpen
                      return Nothing)
              close = liftIO $ atomically $ writeTVar isOpenVar False

              fillQueue = runConduit
                (sourceDirectoryDeep False input
                  .| mapM_C (liftIO . atomically . writeTBQueue queue))
                `finally` close

              srcQueue = fix $ \loop -> do
                mres <- readIO'
                case mres of
                  Nothing -> return ()
                  Just res -> yield res >> loop
          runConcurrently $
            Concurrently fillQueue *>
              mconcat (toList $ replicate threads
                          (Concurrently (runConduit $ srcQueue .| mapM_C (go input name))))

    logFunc <- view logFuncL
    hoogleFiles0 <- newIORef mempty
    hashedNames0 <- newIORef mempty
    let uploadEnv = UploadEnv
          { ueEnv = env'
          , ueBucket = bucket
          , ueLogFunc = logFunc
          , ueHoogleFiles = hoogleFiles0
          , ueHashedNames = hashedNames0
          } 
    runRIO uploadEnv $ runResourceT $ do
        inner
--        ((), _, hoogles) <- inner --  (env', bucket) mempty
        hoogles <- readIORef =<< view (to ueHoogleFiles)

        lbs <- liftIO $ fmap Tar.write $ mapM toEntry $ toList hoogles
        -- logF <- view logFuncL
        -- withReaderT _ -- runRIO UploadEnv{ueEnv = env', ueBucket = bucket, ueLogFunc = logFunc} $
        upload' True (name <> "/hoogle/orig.tar") $ sourceLazy lbs

-- | Create a TAR entry for each Hoogle txt file. Unfortunately doesn't stream.
toEntry :: FilePath -> IO Tar.Entry
toEntry fp = do
    tp <- either error return $ Tar.toTarPath False $ F.takeFileName fp
    Tar.packFileEntry fp tp

data UploadEnv = UploadEnv
  { ueEnv :: !Env
  , ueBucket :: !Text
  , ueLogFunc :: !LogFunc
  , ueHoogleFiles :: !(IORef (Set FilePath))
  , ueHashedNames :: !(IORef (Map FilePath Text, Set Text))
  }

instance HasLogFunc UploadEnv where
  logFuncL = lens ueLogFunc (\x y -> x { ueLogFunc = y })

upload' :: -- (MonadResource m, MonadReader UploadEnv m) =>
           Bool -- ^ compress?
        -> Text -- ^ S3 key
        -> ConduitT () ByteString (ResourceT (RIO UploadEnv)) ()
        -> ResourceT (RIO UploadEnv) ()
upload' toCompress name src = do
    UploadEnv{ueEnv = env, ueBucket = bucket} <- ask
    let loop i = do
            eres <- -- liftResourceT $
              tryAny $ runConduit $ src .| upload toCompress env bucket name
            case eres of
                Left e
                    | i > maxAttempts -> throwIO e
                    | otherwise -> do
                        logError $ "Exception (" <> display i <> "/" <> display maxAttempts <>
                                   "), retrying: " <> display e
                        liftIO $ threadDelay $ 2000000 * i
                        loop $! i + 1
                Right () -> return ()
    loop 1
  where
    maxAttempts = 20

upload ::
       ( HasLogFunc env
       , MonadReader env m
       , MonadResource m
       , MonadThrow m
       , PrimMonad m
       )
    => Bool -- ^ compression?
    -> Env
    -> Text
    -> Text
    -> ConduitT ByteString o m ()
upload toCompress env' bucket name = do
    let mime = defaultMimeLookup name

    body <-
        if toCompress
            then compress 9 (WindowBits 31) .| sinkLazy
            else sinkLazy

    let po = set poContentType (Just $ decodeUtf8Lenient mime)
           $ (if toCompress
                then set poContentEncoding (Just "gzip")
                else id)
           $ set poCacheControl (Just "maxage=31536000")
           $ set poACL (Just OPublicRead)
           $ putObject (BucketName bucket) (ObjectKey name) (toBody body)

    logInfo $ "Sending " <> display name
    _pors <- liftResourceT $ runAWS env' $ send po
    return ()

go :: FilePath -- ^ prefix for all input
   -> Text -- ^ upload name
   -> FilePath -- ^ current file
   -> ResourceT (RIO UploadEnv) ()
go input name fp
    | isHoogleFile input fp = do
        r <- view $ to ueHoogleFiles
        atomicModifyIORef' r $ \files -> (Set.insert fp files, ())
    | F.takeExtension fp == ".html" = do
        doc <- runConduit
             $ sourceFile fp
            .| eventConduit
            .| (do
                    yield (Nothing, EventBeginDoctype "html" Nothing)
                    yield (Nothing, EventEndDoctype)
                    mapMC $ \e -> do
                        e' <- goEvent fp toRoot packageUrl e
                        return (Nothing, e')
                    )
            .| fromEvents

        -- Sink to a Document and then use blaze-html to render to avoid using
        -- XML rendering rules (e.g., empty elements)
        upload' True key $ sourceLazy (renderHtml $ toHtml doc)
    | any (\ext -> F.takeExtension fp == ('.':ext)) $ words "css js png gif" = void $ getName fp
    | otherwise = upload' True key $ sourceFile fp
  where
    Just suffix = stripDirPrefix input fp
    toRoot = T.concat $ toList $ replicate (length $ F.splitDirectories suffix) "../"
    key = name <> "/" <> T.pack suffix
    packageUrl = T.concat
        [ "https://www.stackage.org/"
        , name
        , "/package/"
        , T.takeWhile (/= '/') $ T.pack suffix
        ]

isHoogleFile :: FilePath -> FilePath -> Bool
isHoogleFile input fp' = fromMaybe False $ do
    fp <- stripDirPrefix input fp'
    [pkgver, name] <- Just $ F.splitDirectories fp
    (pkg, ".txt") <- Just $ F.splitExtensions name
    PackageIdentifier pkg1 _ver <- parsePackageIdentifier pkgver
    pkg2 <- parsePackageName pkg
    return $ pkg1 == pkg2

stripDirPrefix :: FilePath -> FilePath -> Maybe FilePath
stripDirPrefix pref path = stripPrefix (F.addTrailingPathSeparator pref) path

goEvent :: FilePath -- HTML file path
        -> Text -- ^ relative prefix to root
        -> Text -- ^ package base page
        -> Event
        -> M Event
goEvent htmlfp toRoot packageUrl (EventBeginElement name attrs) =
    EventBeginElement name <$> mapM (goAttr htmlfp toRoot packageUrl) attrs
goEvent _ _ _ e = return e

goAttr :: FilePath -- ^ HTML file path
       -> Text -- ^ relative prefix to root
       -> Text -- ^ package base page
       -> (Name, [Content])
       -> M (Name, [Content])
goAttr htmlfp toRoot packageUrl pair@(name, [ContentText value])
    | name == "href" && value == "index.html" = return ("href", [ContentText packageUrl])
    | isRef name && not (".html" `T.isSuffixOf` value) = do
        let fp = F.takeDirectory htmlfp F.</> T.unpack value
        exists <- liftIO $ D.doesFileExist fp
        if exists
            then do
                x <- getName fp
                return (name, [ContentText $ toRoot <> x])
            else return pair
goAttr _ _ _ pair = return pair

isRef :: Name -> Bool
isRef "href" = True
isRef "src" = True
isRef _ = False

getName :: FilePath -> M Text
getName src = do
    r <- view $ to ueHashedNames
    (m, _) <- readIORef r
    case Map.lookup src m of
        Just x -> return x
        Nothing -> do
            x <- toHash r src
            atomicModifyIORef' r $ \(m', s) -> ((Map.insert src x m', s), ())
            return x

toHash :: IORef (Map a b, Set Text) -> FilePath -> M Text
toHash r src = do
    (digest, lbs) <- runConduit $ sourceFile src .| sink
    let hash' = T.unpack . decodeUtf8Lenient $ convertToBase Base16 (digest :: Digest SHA256)
        name = T.pack $ "byhash" F.</> hash' F.<.> F.takeExtensions src
    (_, s) <- readIORef r
    unless (name `Set.member` s) $ do
        atomicModifyIORef' r $ \(m, s') -> ((m, Set.insert name s'), ())
        upload' True name $ sourceLazy lbs
    return name
  where
    sink = getZipSink $ (,)
        <$> ZipSink sinkHash
        <*> ZipSink sinkLazy
