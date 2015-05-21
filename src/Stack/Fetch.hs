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
import           Data.Monoid ((<>))
import           Stack.Types
import           Stack.PackageIndex (findNewestVersions)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Check as Tar
import           Control.Applicative ((*>), (<$>), (<*>))

import           Control.Concurrent.Async (Concurrently (..))
import           Control.Concurrent.Async (wait, withAsync)
import           Control.Concurrent.STM   (atomically, newTVarIO, readTVar,
                                           writeTVar, TVar, modifyTVar, readTVarIO)
import           Control.Exception (Exception, throwIO, SomeException, toException)
import           Control.Monad (join, unless, when, void)
import           Control.Monad.Catch (MonadThrow, throwM)
import Codec.Compression.GZip (decompress)
import           Crypto.Hash              (Context, Digest, SHA512,
                                           digestToHexByteString, hashFinalize,
                                           hashInit, hashUpdate)
import           Data.Aeson               (FromJSON (..), decode, withObject,
                                           (.!=), (.:?))
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import           Data.Either (partitionEithers)
import           Data.Function (fix)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           Network.HTTP.Download
import           Network.HTTP.Client      (Manager, brRead, checkStatus,
                                           responseBody,
                                           responseStatus, withResponse)
import           Network.HTTP.Types (statusCode)
import           Stack.Constants
import           Stack.PackageIndex (requireIndex)

import Path
import           System.Directory         (createDirectoryIfMissing,
                                           doesFileExist, doesDirectoryExist,
                                           renameFile, canonicalizePath)
import           System.FilePath (takeDirectory, (<.>), takeExtension)
import qualified System.FilePath as FP
import           System.IO                (IOMode (ReadMode, WriteMode),
                                           withBinaryFile)
import Data.Monoid (Monoid (..))
import Control.Applicative ((<|>))

data Package = Package
    { packageHashes    :: Map Text Text
    , packageLocations :: [Text]
    , packageSize      :: Maybe Word64
    , packageCabal     :: !S.ByteString
    -- ^ use an empty ByteString to indicate nothing found
    }
    deriving Show
instance FromJSON Package where
    parseJSON = withObject "Package" $ \o -> Package
        <$> o .:? "package-hashes" .!= Map.empty
        <*> o .:? "package-locations" .!= []
        <*> o .:? "package-size"
        <*> return S.empty -- never stored in JSON
instance Monoid Package where
    mempty = Package Map.empty [] Nothing S.empty
    mappend l r = Package
        { packageHashes = Map.union (packageHashes l) (packageHashes r)
        , packageLocations = packageLocations l ++ packageLocations r
        , packageSize = packageSize l <|> packageSize r
        , packageCabal = if S.null (packageCabal l) then packageCabal r else packageCabal l
        }

getPackageInfo :: FilePath -> Set PackageIdentifier -> IO (Map PackageIdentifier Package)
getPackageInfo indexTar pkgs0 = withBinaryFile indexTar ReadMode $ \h -> do
    lbs <- L.hGetContents h
    loop pkgs0 pkgs0 Map.empty False $ Tar.read lbs
  where
    loop pkgsJ pkgsC m sawJSON Tar.Done = do
        let pkgs = mappend pkgsJ pkgsC
        when (not (Set.null pkgs) && sawJSON) $
            putStrLn $ "Warning: packages not found in index: " ++ show (Set.toList pkgs)
        return m
    loop _ _ _m _ (Tar.Fail e) = throwIO $ Couldn'tReadIndexTarball indexTar e
    loop pkgsJ pkgsC m _ (Tar.Next _ _)
        | Set.null pkgsJ && Set.null pkgsC = return m
    loop pkgsJ pkgsC m sawJSON (Tar.Next e es) = case Tar.entryContent e of
        Tar.NormalFile lbs _
            | Just pair <- getName ".json" (Tar.entryPath e)
            , pair `Set.member` pkgsJ
            , Just p <- decode lbs ->
                loop (Set.delete pair pkgsJ) pkgsC (add pair p m) sawJSON' es
            | Just pair <- getName ".cabal" (Tar.entryPath e)
            , pair `Set.member` pkgsC ->
                loop pkgsJ (Set.delete pair pkgsC) (add pair (fromCabal lbs) m) sawJSON' es
        _ -> loop pkgsJ pkgsC m sawJSON' es
      where
        sawJSON' = sawJSON || takeExtension (Tar.entryPath e) == ".json"

    add = Map.insertWith mappend

    fromCabal lbs = Package
        { packageHashes = Map.empty
        , packageLocations = []
        , packageSize = Nothing
        , packageCabal = L.toStrict lbs
        }

    getName ext name =
        case T.splitOn "/" $ T.pack name of
            [pkg, ver, fp]
              | T.stripSuffix ext fp == Just pkg ->
                do name' <- parsePackageNameFromString (T.unpack pkg)
                   ver' <- parseVersionFromString (T.unpack ver)
                   return (PackageIdentifier name' ver')
            _ -> Nothing

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
        , _ihExpected :: Text
        , _ihActual   :: Digest SHA512
        }
    | CabalFileNotFound PackageIdentifier
    | UnpackDirectoryAlreadyExists FilePath
    | CouldNotParsePackageSelectors [String]
    deriving (Show, Typeable)
instance Exception FetchException

-- | Similar to 'fetchPackages', but optimized for command line input, where
-- the values may be either package names or package identifiers.
unpackPackages :: (MonadIO m,MonadReader env m,HasHttpManager env,HasConfig env,MonadThrow m,MonadLogger m)
               => FilePath -- ^ destination
               -> [String] -- ^ names or identifiers
               -> m ()
unpackPackages dest input = do
    dest' <- liftIO (canonicalizePath dest) >>= parseAbsDir
    (names, idents1) <- case partitionEithers $ map parse input of
        ([], x) -> return $ partitionEithers x
        (errs, _) -> throwM $ CouldNotParsePackageSelectors errs
    idents2 <-
        if null names
            then return []
            else findNewestVersions names
    dests <- fetchPackages $ map (, Just dest') $ idents1 ++ idents2
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
    :: (MonadIO m, MonadReader env m, HasHttpManager env, HasConfig env, MonadThrow m, MonadLogger m)
    => Set PackageIdentifier
    -> m (Set (Path Abs Dir))
unpackPackageIdentsForBuild idents0 = do
    config <- askConfig
    let unpackDir = configLocalUnpackDir config
    (idents, paths1) <- liftM partitionEithers $ forM (Set.toList idents0) $ \ident -> do
        rel <- parseRelDir $ packageIdentifierString ident
        let dir = unpackDir </> rel
        exists <- liftIO $ doesDirectoryExist $ toFilePath dir
        if exists
            then return $ Right dir
            else return $ Left ident
    if null idents
        then return $ Set.fromList paths1
        else do
            paths2 <- fetchPackages $ map (, Just unpackDir) idents
            return $ Set.fromList $ paths1 ++ paths2

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
fetchPackages :: (F.Foldable f,Functor f,MonadIO m,MonadReader env m,HasHttpManager env,HasConfig env,MonadLogger m,MonadThrow m)
              => f (PackageIdentifier, Maybe (Path Abs Dir))
              -> m [Path Abs Dir]
fetchPackages pkgs = do
   env <- ask
   let man = getHttpManager env
       config = getConfig env
       indexFP = toFilePath $ configPackageIndex config
   requireIndex
   liftIO $ do
     outputVar <- newTVarIO []
     let packageLocation = configPackageTarball config
     withAsync (getPackageInfo indexFP $
                Set.fromList $
                map fst $
                F.toList pkgs) $ \a -> do
         parMapM_ connectionCount (go packageLocation man (wait a) outputVar) pkgs
     readTVarIO outputVar
  where
    connectionCount = 8 -- FIXME put in Config
    unlessM p f = do
        p' <- p
        unless p' f

    go :: (PackageIdentifier -> IO (Path Abs File))
       -> Manager
       -> IO (Map PackageIdentifier Package)
       -> TVar [Path Abs Dir]
       -> (PackageIdentifier, Maybe (Path Abs Dir))
       -> IO ()
    go packageLocation man getPackageInfo' outputVar (ident, mdest) = do
        fp <- fmap toFilePath $ packageLocation ident
        unlessM (doesFileExist fp) $ do
            -- FIXME $logInfo $ "Downloading " <> packageIdentifierText ident
            packageInfo <- getPackageInfo'
            let (msha512, url, msize) =
                    case Map.lookup ident packageInfo of
                        Nothing -> (Nothing, defUrl, Nothing)
                        Just p ->
                            ( Map.lookup "SHA512" $ packageHashes p
                            , case reverse $ packageLocations p of
                                [] -> defUrl
                                x:_ -> T.unpack x
                            , packageSize p
                            )
            createDirectoryIfMissing True $ takeDirectory fp
            req <- parseUrl url
            let req' = req
                    { checkStatus = \s' x y ->
                        if statusCode s' `elem` [401, 403]
                            -- See: https://github.com/fpco/stackage-install/issues/2
                            then Nothing
                            else checkStatus req s' x y
                    }
            withResponse req' man $ \res -> if statusCode (responseStatus res) == 200
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
                                                    throwIO InvalidDownloadSize
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
                                                throwIO InvalidDownloadSize
                                                    { _idsUrl = url
                                                    , _idsExpected = expected
                                                    , _idsTotalDownloaded = total'
                                                    }
                                            _ -> loop total' $! hashUpdate ctx bs
                        loop 0 hashInit
                    renameFile tmp fp
                else do
                    return ()
                    -- FIXME $logError $ "Error downloading " <> packageIdentifierText ident

        case mdest of
            Nothing -> return ()
            Just dest' -> do
                packageInfo <- getPackageInfo'
                cabalBS <-
                    case Map.lookup ident packageInfo of
                        Just p | not $ S.null $ packageCabal p
                            -> return $ packageCabal p
                        _ -> throwM $ CabalFileNotFound ident

                let dest = toFilePath dest'
                    innerDest = dest FP.</> packageIdentifierString ident
                exists <- doesDirectoryExist innerDest
                when exists $ throwM $ UnpackDirectoryAlreadyExists innerDest

                createDirectoryIfMissing True dest

                withBinaryFile fp ReadMode $ \h -> do
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

validHash :: String -> Maybe Text -> Context SHA512 -> IO ()
validHash _ Nothing _ = return ()
validHash url (Just sha512) ctx
    | encodeUtf8 sha512 == digestToHexByteString dig = return ()
    | otherwise = throwIO InvalidHash
        { _ihUrl = url
        , _ihExpected = sha512
        , _ihActual = dig
        }
  where
    dig = hashFinalize ctx

parMapM_ :: F.Foldable f
         => Int
         -> (a -> IO ())
         -> f a
         -> IO ()
parMapM_ (max 1 -> 1) f xs = F.mapM_ f xs
parMapM_ cnt f xs0 = do
    var <- newTVarIO $ F.toList xs0
    let worker :: IO ()
        worker = fix $ \loop -> join $ atomically $ do
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
