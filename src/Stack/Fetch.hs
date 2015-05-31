{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ViewPatterns       #-}

-- | Functionality for downloading packages securely for cabal's usage.

module Stack.Fetch
    ( fetchPackages
    , unpackPackages
    , unpackPackageIdentsForBuild
    ) where

import           Control.Monad.IO.Class
import           Control.Monad (forM, liftM)
import           Control.Monad.Logger
import           Control.Monad.Reader (asks, runReaderT)
import           Data.Monoid ((<>))
import           Stack.Types
import           Stack.PackageIndex
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Check as Tar
import           Control.Applicative ((*>), (<$>), (<*>))
import Control.Monad.Trans.Control
import           Control.Concurrent.Async.Lifted (Concurrently (..))
import           Control.Concurrent.STM   (atomically, newTVarIO, readTVar,
                                           writeTVar, TVar, modifyTVar, readTVarIO)
import           Control.Exception (Exception, throwIO, SomeException, toException)
import           Control.Monad (join, unless, when)
import           Control.Monad.Catch (MonadThrow, throwM)
import Codec.Compression.GZip (decompress)
import           Crypto.Hash              (Context, Digest, SHA512,
                                           digestToHexByteString, hashFinalize,
                                           hashInit, hashUpdate)
import           Data.Aeson               (FromJSON (..), decode, withObject,
                                           (.!=), (.:?))
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Conduit
import qualified Data.Foldable as F
import           Data.Either (partitionEithers)
import           Data.Function (fix)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           Network.HTTP.Download
import           Network.HTTP.Client      (Manager, brRead, checkStatus,
                                           responseBody,
                                           responseStatus, withResponse)
import           Network.HTTP.Types (statusCode)
import           Stack.Constants

import Path
import           System.Directory         (createDirectoryIfMissing,
                                           doesFileExist, doesDirectoryExist,
                                           renameFile, canonicalizePath)
import           System.FilePath (takeDirectory, (<.>), takeExtension)
import qualified System.FilePath as FP
import           System.IO                (IOMode (ReadMode, WriteMode),
                                           withBinaryFile, SeekMode (AbsoluteSeek),
                                           hSeek)
import System.Process.Read (EnvOverride)
import Data.Monoid (Monoid (..))
import Control.Applicative ((<|>))

data FetchException
    = Couldn'tReadIndexTarball FilePath Tar.FormatError
    | Couldn'tReadPackageTarball FilePath SomeException
    | InvalidDownloadSize
        { _idsUrl             :: String
        , _idsExpected        :: Word64
        , _idsTotalDownloaded :: Word64
        }
    | InvalidHash
        { _ihUrl      :: String
        , _ihExpected :: S.ByteString
        , _ihActual   :: Digest SHA512
        }
    | CabalFilesNotFound (Set PackageIdentifier)
    | UnpackDirectoryAlreadyExists FilePath
    | CouldNotParsePackageSelectors [String]
    deriving (Show, Typeable)
instance Exception FetchException

-- | Similar to 'fetchPackages', but optimized for command line input, where
-- the values may be either package names or package identifiers.
unpackPackages :: (MonadIO m,MonadBaseControl IO m,MonadReader env m,HasHttpManager env,HasConfig env,MonadThrow m,MonadLogger m)
               => EnvOverride
               -> FilePath -- ^ destination
               -> [String] -- ^ names or identifiers
               -> m ()
unpackPackages menv dest input = do
    dest' <- liftIO (canonicalizePath dest) >>= parseAbsDir
    (names, idents1) <- case partitionEithers $ map parse input of
        ([], x) -> return $ partitionEithers x
        (errs, _) -> throwM $ CouldNotParsePackageSelectors errs
    idents2 <-
        if null names
            then return []
            else do
                $logDebug $ "Finding latest versions of: " <> T.pack (show names)
                idents2 <- findNewestVersions menv names
                $logDebug $ "Newest versions are: " <> T.pack (show idents2)
                return idents2
    dests <- fetchPackages menv $ map (, Just dest') $ idents1 ++ idents2
    mapM_ (\dest'' -> $logInfo $ "Unpacked to " <> T.pack (toFilePath dest'')) dests
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
unpackPackageIdentsForBuild
    :: (MonadBaseControl IO m, MonadIO m, MonadReader env m, HasHttpManager env, HasBuildConfig env, MonadThrow m, MonadLogger m)
    => EnvOverride
    -> Map PackageName Version
    -> m (Set (Path Abs Dir))
unpackPackageIdentsForBuild menv idents0 = do
    bconfig <- asks getBuildConfig
    let unpackDir = configLocalUnpackDir bconfig
    (idents, paths1) <- liftM partitionEithers $ forM (Map.toList idents0) $ \(name, version) -> do
        let ident = PackageIdentifier name version
        rel <- parseRelDir $ packageIdentifierString ident
        let dir = unpackDir </> rel
        exists <- liftIO $ doesDirectoryExist $ toFilePath dir
        if exists
            then return $ Right dir
            else return $ Left ident
    if null idents
        then return $ Set.fromList paths1
        else do
            paths2 <- fetchPackages menv $ map (, Just unpackDir) idents
            return $ Set.fromList $ paths1 ++ paths2

-- | Add the contents of the cabal file from the index to packages that will be
-- unpacked.
addCabalFiles :: (MonadIO m,MonadReader env m,HasHttpManager env,HasConfig env,MonadLogger m,MonadThrow m)
              => EnvOverride
              -> [(PackageIdentifier, Maybe (Path Abs Dir))]
              -> m (Either (Set PackageIdentifier) [(PackageIdentifier, Maybe (Path Abs Dir, S8.ByteString))])
addCabalFiles menv pkgs0 = do
    let (noUnpack, toUnpack0) = partitionEithers $ map toEither pkgs0
    if null toUnpack0
        then return $ Right noUnpack
        else do
            pis <- readPackageIdents menv
            config <- asks getConfig
            let fp = toFilePath $ configPackageIndex config
            (missing, toUnpack) <- liftIO
                $ withBinaryFile fp ReadMode $ \h ->
                liftM partitionEithers $ mapM (go pis h) toUnpack0
            return $ if null missing
                then Right $ noUnpack ++ toUnpack
                else Left $ Set.fromList missing
  where
    toEither (ident, Nothing) = Left (ident, Nothing)
    toEither (ident, Just path) = Right (ident, path)

    go pis h (ident, path) =
        case Map.lookup ident pis of
            Nothing -> return $ Left ident
            Just pc -> do
                hSeek h AbsoluteSeek $ fromIntegral $ pcOffset pc
                bs <- S.hGet h $ fromIntegral $ pcSize pc
                return $ Right (ident, Just (path, bs))

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
              => EnvOverride
              -> [(PackageIdentifier, Maybe (Path Abs Dir))]
              -> m [Path Abs Dir]
fetchPackages menv pkgs0 = do
   env <- ask
   let man = getHttpManager env
       config = getConfig env
   indexFP <- liftM toFilePath configPackageIndex
   requireIndex menv
   outputVar <- liftIO (newTVarIO [])
   let packageLocation = flip runReaderT config . configPackageTarball

   pds <- getPackageDownloads menv

   eres <- addCabalFiles menv pkgs0
   pkgs <-
       case eres of
           Left _missing -> do
               $logInfo "Some cabal files not found, updating index"
               updateIndex menv
               eres' <- addCabalFiles menv pkgs0
               case eres' of
                   Left missing -> throwM $ CabalFilesNotFound missing
                   Right pkgs -> return pkgs
           Right pkgs -> return pkgs

   parMapM_
    (configConnectionCount config)
    (go packageLocation man pds outputVar)
    pkgs
   liftIO (readTVarIO outputVar)
  where
    unlessM p f = do
        p' <- p
        unless p' f

    go :: (MonadIO m,Functor m,MonadThrow m,MonadLogger m)
       => (PackageIdentifier -> m (Path Abs File))
       -> Manager
       -> Map PackageIdentifier PackageDownload
       -> TVar [Path Abs Dir]
       -> (PackageIdentifier, Maybe (Path Abs Dir, S8.ByteString))
       -> m ()
    go packageLocation man pds outputVar (ident, mdest) = do
        fp <- fmap toFilePath $ packageLocation ident
        unlessM (liftIO (doesFileExist fp)) $ do
            $logInfo $ "Downloading " <> packageIdentifierText ident
            let (msha512, url, msize) =
                    case Map.lookup ident pds of
                        Nothing -> (Nothing, defUrl, Nothing)
                        Just pd ->
                            ( Just $ pdSHA512 pd
                            , S8.unpack $ pdUrl pd
                            , Just $ pdSize pd
                            )
            liftIO $ createDirectoryIfMissing True $ takeDirectory fp
            req <- parseUrl url
            let req' = req
                    { checkStatus = \s' x y ->
                        if statusCode s' `elem` [401, 403]
                            -- See: https://github.com/fpco/stackage-install/issues/2
                            then Nothing
                            else checkStatus req s' x y
                    }
            ok <- liftIO $ withResponse req' man $ \res -> if statusCode (responseStatus res) == 200
                then do
                    let tmp = fp <.> "tmp"
                    withBinaryFile tmp WriteMode $ \h -> do
                        let loop total ctx = do
                                bs <- brRead $ responseBody res
                                if S.null bs
                                    then
                                        case msize of
                                            Nothing -> return ()
                                            Just expected
                                                | expected /= total ->
                                                    throwM InvalidDownloadSize
                                                        { _idsUrl = url
                                                        , _idsExpected = expected
                                                        , _idsTotalDownloaded = total
                                                        }
                                                | otherwise -> validHash url msha512 ctx
                                    else do
                                        S.hPut h bs
                                        let total' = total + fromIntegral (S.length bs)
                                        case msize of
                                            Just expected | expected < total' ->
                                                throwM InvalidDownloadSize
                                                    { _idsUrl = url
                                                    , _idsExpected = expected
                                                    , _idsTotalDownloaded = total'
                                                    }
                                            _ -> loop total' $! hashUpdate ctx bs
                        loop 0 hashInit
                    renameFile tmp fp
                    return True
                else return False
            when (not ok)
                 ($logError $ "Error downloading " <> packageIdentifierText ident)

        case mdest of
            Nothing -> return ()
            Just (dest', cabalBS) -> do
                let dest = toFilePath dest'
                    innerDest = dest FP.</> packageIdentifierString ident
                exists <- liftIO (doesDirectoryExist innerDest)
                when exists $ throwM $ UnpackDirectoryAlreadyExists innerDest

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
                    S.writeFile cabalFP cabalBS

                    res <- parseAbsDir innerDest
                    atomically $ modifyTVar outputVar (res:)

      where
        pkg = packageIdentifierString ident
        targz = pkg ++ ".tar.gz"
        defUrl = T.unpack packageDownloadPrefix ++ targz

validHash :: String -> Maybe S.ByteString -> Context SHA512 -> IO ()
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
