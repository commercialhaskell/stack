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
import           Control.Concurrent.Async        (Concurrently (..))
import           Control.Concurrent.STM          (TVar, atomically, modifyTVar,
                                                  newTVarIO, readTVar,
                                                  readTVarIO, writeTVar)
import           Control.Exception               (Exception, SomeException,
                                                  toException)
import           Control.Monad                   (liftM, when, join, unless, void)
import           Control.Monad.Catch             (MonadThrow, throwM, catch)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader      (runReaderT)
import           Crypto.Hash                     (SHA512(..))
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as S
import qualified Data.ByteString.Char8           as C8
import qualified Data.ByteString.Lazy            as L
import           Data.Either                     (partitionEithers)
import qualified Data.Foldable                   as F
import           Data.Function                   (fix)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromMaybe, maybeToList)
import           Data.Monoid                     ((<>))
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import           Data.Text.Encoding              (decodeUtf8)
import           Data.Typeable                   (Typeable)
import           Data.Word                       (Word64)
import           Network.HTTP.Client             (Manager)
import           Network.HTTP.Download
import           Stack.PackageIndex
import           Stack.Types

import           Path
import           System.Directory                (canonicalizePath,
                                                  createDirectoryIfMissing,
                                                  doesDirectoryExist,
                                                  renameDirectory)
import           System.FilePath                 ((<.>))
import qualified System.FilePath                 as FP
import           System.IO                       (IOMode (ReadMode),
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
    | InvalidSha512
        { _ihUrl      :: T.Text
        , _ihExpected :: S.ByteString
        , _ihActual   :: String
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
    unpacked <- fetchPackages Nothing toFetch
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
    -> Maybe (Path Rel Dir) -- ^ the dist rename directory, see: https://github.com/fpco/stack/issues/157
    -> Set PackageIdentifier
    -> m (Map PackageIdentifier (Path Abs Dir))
unpackPackageIdents menv unpackDir mdistDir idents = do
    resolved <- resolvePackages menv idents Set.empty
    ToFetchResult toFetch alreadyUnpacked <- getToFetch unpackDir resolved
    nowUnpacked <- fetchPackages mdistDir toFetch
    return $ alreadyUnpacked <> nowUnpacked

data ResolvedPackage = ResolvedPackage
    { rpCache    :: !PackageCache
    , rpIndex    :: !PackageIndex
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
        caches <- getPackageCaches menv
        let versions = Map.fromListWith max $ map toTuple $ Map.keys caches
            (missing, idents1) = partitionEithers $ map
                (\name -> maybe (Left name ) (Right . PackageIdentifier name)
                    (Map.lookup name versions))
                (Set.toList names0)
        return $ if null missing
            then goIdents caches $ idents0 <> Set.fromList idents1
            else Left $ UnknownPackageNames $ Set.fromList missing

    goIdents caches idents =
        case partitionEithers $ map (goIdent caches) $ Set.toList idents of
            ([], resolved) -> Right $ Map.fromList resolved
            (missing, _) -> Left $ UnknownPackageIdentifiers $ Set.fromList missing

    goIdent caches ident =
        case Map.lookup ident caches of
            Nothing -> Left ident
            Just (index, cache) -> Right (ident, ResolvedPackage
                { rpCache = cache
                , rpIndex = index
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
                    d = pcDownload $ rpCache resolved
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
              => Maybe (Path Rel Dir) -- ^ the dist rename directory, see: https://github.com/fpco/stack/issues/157
              -> Map PackageIdentifier ToFetch
              -> m (Map PackageIdentifier (Path Abs Dir))
fetchPackages mdistDir toFetchAll = do
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
    go :: (MonadIO m,Functor m,MonadThrow m,MonadLogger m)
       => Manager
       -> TVar (Map PackageIdentifier (Path Abs Dir))
       -> (PackageIdentifier, ToFetch)
       -> m ()
    go man outputVar (ident, toFetch) = do
        req <- parseUrl $ T.unpack $ tfUrl toFetch
        let destpath = tfTarball toFetch

        let toHashCheck bs = HashCheck SHA512 (C8.unpack bs)
        let downloadReq = DownloadRequest
                { drRequest = req
                , drHashChecks = map toHashCheck $ maybeToList (tfSHA512 toFetch)
                , drLengthCheck = fmap fromIntegral $ tfSize toFetch
                }
        let progressSink = do
                -- TODO: logInfo
                liftIO $ T.putStrLn $ "Downloading " <> packageIdentifierText ident
        errMay <- liftIO $ do
            (flip runReaderT man (verifiedDownload downloadReq destpath progressSink) >> return Nothing)
                `catch` \e -> case e of
                    WrongContentLength _ actual -> return $ Just $ InvalidDownloadSize
                        { _idsUrl = tfUrl toFetch
                        , _idsExpected = fromMaybe (error "fetchPackagesImpossible cl") (tfSize toFetch)
                        , _idsTotalDownloaded = read (show actual) -- TODO(danburton): something better than this
                        }
                    WrongStreamLength _ actual -> return $ Just $ InvalidDownloadSize
                        { _idsUrl = tfUrl toFetch
                        , _idsExpected = fromMaybe (error "fetchPackagesImpossible sl") (tfSize toFetch)
                        , _idsTotalDownloaded = fromIntegral actual
                        }
                    WrongDigest _ _ actual -> return $ Just $ InvalidSha512
                        { _ihUrl = tfUrl toFetch
                        , _ihExpected = fromMaybe (error "fetchPackagesImpossible dg") (tfSHA512 toFetch)
                        , _ihActual = actual
                        }
        maybe (return ()) throwM errMay

        let fp = toFilePath destpath
        --unlessM (liftIO (doesFileExist fp)) $ do
        --    $logInfo $ "Downloading " <> packageIdentifierText ident
        --    liftIO $ createDirectoryIfMissing True $ takeDirectory fp
        --    req <- parseUrl $ T.unpack $ tfUrl toFetch
        --    -- FIXME switch to using verifiedDownload
        --    liftIO $ withResponse req man $ \res -> do
        --        let tmp = fp <.> "tmp"
        --        withBinaryFile tmp WriteMode $ \h -> do
        --            let loop total ctx = do
        --                    bs <- brRead $ responseBody res
        --                    if S.null bs
        --                        then
        --                            case tfSize toFetch of
        --                                Nothing -> return ()
        --                                Just expected
        --                                    | expected /= total ->
        --                                        throwM InvalidDownloadSize
        --                                            { _idsUrl = tfUrl toFetch
        --                                            , _idsExpected = expected
        --                                            , _idsTotalDownloaded = total
        --                                            }
        --                                    | otherwise -> validHash (tfUrl toFetch) (tfSHA512 toFetch) ctx
        --                        else do
        --                            S.hPut h bs
        --                            let total' = total + fromIntegral (S.length bs)
        --                            case tfSize toFetch of
        --                                Just expected | expected < total' ->
        --                                    throwM InvalidDownloadSize
        --                                        { _idsUrl = tfUrl toFetch
        --                                        , _idsExpected = expected
        --                                        , _idsTotalDownloaded = total'
        --                                        }
        --                                _ -> loop total' $! hashUpdate ctx bs
        --            loop 0 hashInit
        --        renameFile tmp fp

        let dest = toFilePath $ parent $ tfDestDir toFetch
            innerDest = toFilePath $ tfDestDir toFetch

        liftIO $ createDirectoryIfMissing True dest

        liftIO $ withBinaryFile fp ReadMode $ \h -> do
            -- Avoid using L.readFile, which is more likely to leak
            -- resources
            lbs <- L.hGetContents h
            let entries = fmap (either wrap wrap)
                        $ Tar.checkTarbomb identStr
                        $ Tar.read $ decompress lbs
                wrap :: Exception e => e -> FetchException
                wrap = Couldn'tReadPackageTarball fp . toException
                identStr = packageIdentifierString ident
            Tar.unpack dest entries

            case mdistDir of
                Nothing -> return ()
                -- See: https://github.com/fpco/stack/issues/157
                Just distDir -> do
                    let inner = dest FP.</> identStr
                        oldDist = inner FP.</> "dist"
                        newDist = inner FP.</> toFilePath distDir
                    exists <- doesDirectoryExist oldDist
                    when exists $ do
                        createDirectoryIfMissing True $ FP.takeDirectory newDist
                        renameDirectory oldDist newDist

            let cabalFP =
                    innerDest FP.</>
                    packageNameString (packageIdentifierName ident)
                    <.> "cabal"
            S.writeFile cabalFP $ tfCabal toFetch

            atomically $ modifyTVar outputVar $ Map.insert ident $ tfDestDir toFetch

--validHash :: T.Text -> Maybe S.ByteString -> Context SHA512 -> IO ()
--validHash _ Nothing _ = return ()
--validHash url (Just sha512) ctx
--    | sha512 == digestToHexByteString dig = return ()
--    | otherwise = throwIO InvalidHash
--        { _ihUrl = url
--        , _ihExpected = sha512
--        , _ihActual = dig
--        }
--  where
--    dig = hashFinalize ctx

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
