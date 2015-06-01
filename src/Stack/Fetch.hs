{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Functionality for downloading packages securely for cabal's usage.

module Stack.Fetch
    ( unpackPackages
    , unpackPackageIdents
    , resolvePackages
    , ResolvedPackage (..)
    , withCabalFiles
    ) where

import qualified Codec.Archive.Tar               as Tar
import qualified Codec.Archive.Tar.Check         as Tar
import           Codec.Compression.GZip          (decompress)
import           Control.Applicative             ((*>))
import           Control.Concurrent.Async.Lifted (Concurrently (..))
import           Control.Concurrent.STM          (TVar, atomically, modifyTVar,
                                                  newTVarIO, readTVar,
                                                  readTVarIO, writeTVar)
import           Control.Exception               (Exception, SomeException,
                                                  throwIO, toException)
import           Control.Monad                   (liftM)
import           Control.Monad                   (join, unless)
import           Control.Monad.Catch             (MonadThrow, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import           Crypto.Hash                     (Context, Digest, SHA512,
                                                  digestToHexByteString,
                                                  hashFinalize, hashInit,
                                                  hashUpdate)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as S
import qualified Data.ByteString.Lazy            as L
import           Data.Either                     (partitionEithers)
import qualified Data.Foldable                   as F
import           Data.Function                   (fix)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Monoid                     ((<>))
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8)
import           Data.Typeable                   (Typeable)
import           Data.Word                       (Word64)
import           Network.HTTP.Client             (Manager, brRead,
                                                  responseBody,
                                                  withResponse)
import           Network.HTTP.Download
import           Stack.PackageIndex
import           Stack.Types

import           Path
import           System.Directory                (canonicalizePath,
                                                  createDirectoryIfMissing,
                                                  doesDirectoryExist,
                                                  doesFileExist, renameFile)
import           System.FilePath                 (takeDirectory, (<.>))
import qualified System.FilePath                 as FP
import           System.IO                       (IOMode (ReadMode, WriteMode),
                                                  SeekMode (AbsoluteSeek),
                                                  hSeek, withBinaryFile)
import           System.Process.Read             (EnvOverride)

data FetchException
    = Couldn'tReadIndexTarball FilePath Tar.FormatError
    | Couldn'tReadPackageTarball FilePath SomeException
    | InvalidDownloadSize
        { _idsUrl             :: T.Text
        , _idsExpected        :: Word64
        , _idsTotalDownloaded :: Word64
        }
    | InvalidHash
        { _ihUrl      :: T.Text
        , _ihExpected :: S.ByteString
        , _ihActual   :: Digest SHA512
        }
    | UnpackDirectoryAlreadyExists (Set FilePath)
    | CouldNotParsePackageSelectors [String]
    | UnknownPackageNames (Set PackageName)
    | UnknownPackageIdentifiers (Set PackageIdentifier)
    deriving (Show, Typeable)
instance Exception FetchException

-- | Intended to work for the command line command.
unpackPackages :: (MonadIO m,MonadBaseControl IO m,MonadReader env m,HasHttpManager env,HasConfig env,MonadThrow m,MonadLogger m)
               => EnvOverride
               -> FilePath -- ^ destination
               -> [String] -- ^ names or identifiers
               -> m ()
unpackPackages menv dest input = do
    dest' <- liftIO (canonicalizePath dest) >>= parseAbsDir
    (names, idents) <- case partitionEithers $ map parse input of
        ([], x) -> return $ partitionEithers x
        (errs, _) -> throwM $ CouldNotParsePackageSelectors errs
    resolved <- resolvePackages menv (Set.fromList idents) (Set.fromList names)
    ToFetchResult toFetch alreadyUnpacked <- getToFetch dest' resolved
    unless (Map.null alreadyUnpacked) $
        throwM $ UnpackDirectoryAlreadyExists $ Set.fromList $ map toFilePath $ Map.elems alreadyUnpacked
    unpacked <- fetchPackages toFetch
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
    :: (MonadBaseControl IO m, MonadIO m, MonadReader env m, HasHttpManager env, HasConfig env, MonadThrow m, MonadLogger m)
    => EnvOverride
    -> Path Abs Dir -- ^ unpack directory
    -> Set PackageIdentifier
    -> m (Map PackageIdentifier (Path Abs Dir))
unpackPackageIdents menv unpackDir idents = do
    resolved <- resolvePackages menv idents Set.empty
    ToFetchResult toFetch alreadyUnpacked <- getToFetch unpackDir resolved
    nowUnpacked <- fetchPackages toFetch
    return $ alreadyUnpacked <> nowUnpacked

data ResolvedPackage = ResolvedPackage
    { rpCache    :: !PackageCache
    , rpIndex    :: !PackageIndex
    , rpDownload :: !(Maybe PackageDownload)
    }

-- | Resolve a set of package names and identifiers into @FetchPackage@ values.
resolvePackages :: (MonadIO m,MonadReader env m,HasHttpManager env,HasConfig env,MonadLogger m,MonadThrow m,MonadBaseControl IO m)
                => EnvOverride
                -> Set PackageIdentifier
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
    go = do
        (caches, downloads) <- getPackageCaches menv
        let versions = Map.fromListWith max $ map toTuple $ Map.keys caches
            (missing, idents1) = partitionEithers $ map
                (\name -> maybe (Left name ) (Right . PackageIdentifier name)
                    (Map.lookup name versions))
                (Set.toList names0)
        return $ if null missing
            then goIdents caches downloads $ idents0 <> Set.fromList idents1
            else Left $ UnknownPackageNames $ Set.fromList missing

    goIdents caches downloads idents =
        case partitionEithers $ map (goIdent caches downloads) $ Set.toList idents of
            ([], resolved) -> Right $ Map.fromList resolved
            (missing, _) -> Left $ UnknownPackageIdentifiers $ Set.fromList missing

    goIdent caches downloads ident =
        case Map.lookup ident caches of
            Nothing -> Left ident
            Just (index, cache) -> Right (ident, ResolvedPackage
                { rpCache = cache
                , rpIndex = index
                , rpDownload =
                    case Map.lookup ident downloads of
                        Just (index', download')
                            | indexName index == indexName index' -> Just download'
                        _ -> Nothing
                })

data ToFetch = ToFetch
    { tfTarball :: !(Path Abs File)
    , tfDestDir :: !(Path Abs Dir)
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
    :: (MonadThrow m, MonadIO m, MonadReader env m, HasConfig env)
    => IndexName
    -> [(PackageIdentifier, PackageCache, a)]
    -> (PackageIdentifier -> a -> ByteString -> IO b)
    -> m [b]
withCabalFiles name pkgs f = do
    indexPath <- configPackageIndex name
    liftIO $ withBinaryFile (toFilePath indexPath) ReadMode $ \h ->
        mapM (goPkg h) pkgs
  where
    goPkg h (ident, pc, tf) = do
        hSeek h AbsoluteSeek $ fromIntegral $ pcOffset pc
        cabalBS <- S.hGet h $ fromIntegral $ pcSize pc
        f ident tf cabalBS

-- | Figure out where to fetch from.
getToFetch :: (MonadThrow m, MonadIO m, MonadReader env m, HasConfig env)
           => Path Abs Dir -- ^ directory to unpack into
           -> Map PackageIdentifier ResolvedPackage
           -> m ToFetchResult
getToFetch dest resolvedAll = do
    (toFetch0, unpacked) <- liftM partitionEithers $ mapM checkUnpacked $ Map.toList resolvedAll
    toFetch1 <- mapM goIndex $ Map.toList $ Map.fromListWith (++) toFetch0
    return ToFetchResult
        { tfrToFetch = Map.unions toFetch1
        , tfrAlreadyUnpacked = Map.fromList unpacked
        }
  where
    checkUnpacked (ident, resolved) = do
        dirRel <- parseRelDir $ packageIdentifierString ident
        let destDir = dest </> dirRel
        exists <- liftIO $ doesDirectoryExist $ toFilePath destDir
        if exists
            then return $ Right (ident, destDir)
            else do
                let index = rpIndex resolved
                    d = rpDownload resolved
                    targz = T.pack $ packageIdentifierString ident ++ ".tar.gz"
                tarball <- configPackageTarball (indexName index) ident
                return $ Left (indexName index, [(ident, rpCache resolved, ToFetch
                    { tfTarball = tarball
                    , tfDestDir = destDir
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
fetchPackages :: (MonadIO m,MonadReader env m,HasHttpManager env,HasConfig env,MonadLogger m,MonadThrow m,MonadBaseControl IO m)
              => Map PackageIdentifier ToFetch
              -> m (Map PackageIdentifier (Path Abs Dir))
fetchPackages toFetchAll = do
    env <- ask
    let man = getHttpManager env
        config = getConfig env
    outputVar <- liftIO $ newTVarIO Map.empty

    parMapM_
        (configConnectionCount config)
        (go man outputVar)
        (Map.toList toFetchAll)

    liftIO $ readTVarIO outputVar
  where
    unlessM p f = do
        p' <- p
        unless p' f

    go :: (MonadIO m,Functor m,MonadThrow m,MonadLogger m)
       => Manager
       -> TVar (Map PackageIdentifier (Path Abs Dir))
       -> (PackageIdentifier, ToFetch)
       -> m ()
    go man outputVar (ident, toFetch) = do
        let fp = toFilePath $ tfTarball toFetch
        unlessM (liftIO (doesFileExist fp)) $ do
            $logInfo $ "Downloading " <> packageIdentifierText ident
            liftIO $ createDirectoryIfMissing True $ takeDirectory fp
            req <- parseUrl $ T.unpack $ tfUrl toFetch
            -- FIXME switch to using verifiedDownload
            liftIO $ withResponse req man $ \res -> do
                let tmp = fp <.> "tmp"
                withBinaryFile tmp WriteMode $ \h -> do
                    let loop total ctx = do
                            bs <- brRead $ responseBody res
                            if S.null bs
                                then
                                    case tfSize toFetch of
                                        Nothing -> return ()
                                        Just expected
                                            | expected /= total ->
                                                throwM InvalidDownloadSize
                                                    { _idsUrl = tfUrl toFetch
                                                    , _idsExpected = expected
                                                    , _idsTotalDownloaded = total
                                                    }
                                            | otherwise -> validHash (tfUrl toFetch) (tfSHA512 toFetch) ctx
                                else do
                                    S.hPut h bs
                                    let total' = total + fromIntegral (S.length bs)
                                    case tfSize toFetch of
                                        Just expected | expected < total' ->
                                            throwM InvalidDownloadSize
                                                { _idsUrl = tfUrl toFetch
                                                , _idsExpected = expected
                                                , _idsTotalDownloaded = total'
                                                }
                                        _ -> loop total' $! hashUpdate ctx bs
                    loop 0 hashInit
                renameFile tmp fp

        let dest = toFilePath $ parent $ tfDestDir toFetch
            innerDest = toFilePath $ tfDestDir toFetch

        liftIO $ createDirectoryIfMissing True dest

        liftIO $ withBinaryFile fp ReadMode $ \h -> do
            -- Avoid using L.readFile, which is more likely to leak
            -- resources
            lbs <- L.hGetContents h
            let entries = fmap (either wrap wrap)
                        $ Tar.checkTarbomb (packageIdentifierString ident)
                        $ Tar.read $ decompress lbs
                wrap :: Exception e => e -> FetchException
                wrap = Couldn'tReadPackageTarball fp . toException
            Tar.unpack dest entries

            let cabalFP =
                    innerDest FP.</>
                    packageNameString (packageIdentifierName ident)
                    <.> "cabal"
            S.writeFile cabalFP $ tfCabal toFetch

            atomically $ modifyTVar outputVar $ Map.insert ident $ tfDestDir toFetch

validHash :: T.Text -> Maybe S.ByteString -> Context SHA512 -> IO ()
validHash _ Nothing _ = return ()
validHash url (Just sha512) ctx
    | sha512 == digestToHexByteString dig = return ()
    | otherwise = throwIO InvalidHash
        { _ihUrl = url
        , _ihExpected = sha512
        , _ihActual = dig
        }
  where
    dig = hashFinalize ctx

parMapM_ :: (F.Foldable f,MonadIO m,MonadBaseControl IO m)
         => Int
         -> (a -> m ())
         -> f a
         -> m ()
parMapM_ (max 1 -> 1) f xs = F.mapM_ f xs
parMapM_ cnt f xs0 = do
    var <- liftIO (newTVarIO $ F.toList xs0)
    let worker = fix $ \loop -> join $ liftIO $ atomically $ do
            xs <- readTVar var
            case xs of
                [] -> return $ return ()
                x:xs' -> do
                    writeTVar var xs'
                    return $ do
                        f x
                        loop
        workers 1 = Concurrently worker
        workers i = Concurrently worker *> workers (i - 1)
    runConcurrently $ workers cnt
