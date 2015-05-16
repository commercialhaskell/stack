{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}

-- | Functionality for downloading packages securely for cabal's usage.

module Stackage.Fetch
    ( fetchPackages
    , Settings
    , defaultSettings
    , setGetManager
    , setPackageLocation
    , defaultPackageLocation
    , setIndexLocation
    , defaultIndexLocation
    , packageLocationGetter
    ) where

import           Control.Monad.IO.Class
import           Stackage.PackageIdentifier
import           Stackage.PackageName
import           Stackage.PackageVersion

import qualified Codec.Archive.Tar as Tar
import           Control.Applicative ((*>), (<$>), (<*>))

import           Control.Concurrent.Async (Concurrently (..))
import           Control.Concurrent.Async (wait, withAsync)
import           Control.Concurrent.STM   (atomically, newTVarIO, readTVar,
                                           writeTVar)
import           Control.Exception (Exception, throwIO)
import           Control.Monad (join, unless, when)
import           Crypto.Hash              (Context, Digest, SHA512,
                                           digestToHexByteString, hashFinalize,
                                           hashInit, hashUpdate)
import           Data.Aeson               (FromJSON (..), decode, withObject,
                                           (.!=), (.:?))
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
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
import           Network.HTTP.Client      (Manager, brRead, checkStatus,
                                           managerResponseTimeout, newManager,
                                           parseUrl, responseBody,
                                           responseStatus, withResponse)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types (statusCode)
import           System.Directory         (createDirectoryIfMissing,
                                           doesFileExist,
                                           getAppUserDataDirectory, renameFile)
import           System.FilePath (takeDirectory, (<.>), (</>), takeExtension)
import           System.IO                (IOMode (ReadMode, WriteMode), stdout,
                                           withBinaryFile)

-- | Settings used by 'download'.
--
-- Since 0.1.0.0
data Settings = Settings
    { _getManager     :: !(IO Manager)
    , _cabalCommand   :: !FilePath
    , _downloadPrefix :: !String
    , _onDownload     :: !(String -> IO ())
    , _onDownloadErr  :: !(String -> IO ())
    , _connections    :: !Int
    , _packageLocation :: !(forall m. MonadIO m => m (PackageIdentifier -> FilePath))
    , _indexLocation :: !(IO FilePath)
    }

-- | Default value for 'Settings'.
--
-- Since 0.1.0.0
defaultSettings :: Settings
defaultSettings = Settings
    { _getManager = newManager tlsManagerSettings
        { managerResponseTimeout = Just 90000000
        }
    , _cabalCommand = "cabal"
    , _downloadPrefix = "https://s3.amazonaws.com/hackage.fpcomplete.com/package/"
    , _onDownload = \s -> S8.hPut stdout $ S8.pack $ concat
        [ "Downloading "
        , s
        , "\n"
        ]
    , _onDownloadErr = \s -> S8.hPut stdout $ S8.pack $ concat
        [ "Error downloading "
        , s
        , ", if this is a local package, this message can be ignored\n"
        ]
    , _connections = 8
    , _packageLocation = defaultPackageLocation
    , _indexLocation = defaultIndexLocation
    }

-- | Set how to get the connection manager
--
-- Default: @newManager tlsManagerSettings@
--
-- Since 0.1.1.0
setGetManager :: IO Manager -> Settings -> Settings
setGetManager x s = s { _getManager = x }

data Package = Package
    { packageHashes    :: Map Text Text
    , packageLocations :: [Text]
    , packageSize      :: Maybe Word64
    }
    deriving Show
instance FromJSON Package where
    parseJSON = withObject "Package" $ \o -> Package
        <$> o .:? "package-hashes" .!= Map.empty
        <*> o .:? "package-locations" .!= []
        <*> o .:? "package-size"

getPackageInfo :: FilePath -> Set PackageIdentifier -> IO (Map PackageIdentifier Package)
getPackageInfo indexTar pkgs0 = withBinaryFile indexTar ReadMode $ \h -> do
    lbs <- L.hGetContents h
    loop pkgs0 Map.empty False $ Tar.read lbs
  where
    loop pkgs m sawJSON Tar.Done = do
        when (not (Set.null pkgs) && sawJSON) $
            putStrLn $ "Warning: packages not found in index: " ++ show (Set.toList pkgs)
        return m
    loop _ _m _ (Tar.Fail e) = throwIO $ Couldn'tReadIndexTarball indexTar e
    loop pkgs m sawJSON (Tar.Next e es) =
        case (getName $ Tar.entryPath e, Tar.entryContent e) of
            (Just pair, Tar.NormalFile lbs _)
                    | pair `Set.member` pkgs
                    , Just p <- decode lbs ->
                loop (Set.delete pair pkgs) (Map.insert pair p m) sawJSON' es
            _ -> loop pkgs m sawJSON' es
      where
        sawJSON' = sawJSON || takeExtension (Tar.entryPath e) == ".json"

    getName name =
        case T.splitOn "/" $ T.pack name of
            [pkg, ver, fp]
              | T.stripSuffix ".json" fp == Just pkg ->
                do name <- parsePackageNameFromString (T.unpack pkg)
                   ver <- parsePackageVersionFromString (T.unpack ver)
                   return (PackageIdentifier name ver)
            _ -> Nothing

data StackageFetchException
    = Couldn'tReadIndexTarball FilePath Tar.FormatError
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
    deriving (Show, Typeable)
instance Exception StackageFetchException

-- | Get the location that a package name/package version combination is stored
-- on the filesystem.
--
-- @~/.cabal/packages/hackage.haskell.org/name/version/name-version.tar.gz@
--
-- Since 0.1.1.0
defaultPackageLocation :: MonadIO m => m (PackageIdentifier -> FilePath)
defaultPackageLocation =
  liftIO (do cabalDir <-
               getAppUserDataDirectory "cabal"
             let packageDir = cabalDir </> "packages" </> "hackage.haskell.org"
             return $
               \(PackageIdentifier (packageNameString -> name) (packageVersionString -> version)) ->
                 packageDir </> name </> version </>
                 concat [name,"-",version,".tar.gz"])

-- | Set the location packages are stored to.
--
-- Default: 'defaultPackageLocation'
--
-- Since 0.1.1.0
setPackageLocation :: (forall m. MonadIO m => m (PackageIdentifier -> FilePath)) -> Settings -> Settings
setPackageLocation x s = s { _packageLocation = x }

-- | Set the location the 00-index.tar file is stored.
--
-- Default: 'defaultIndexLocation'
--
-- Since 0.1.1.0
setIndexLocation :: IO FilePath -> Settings -> Settings
setIndexLocation x s = s { _indexLocation = x }

-- | Get the location that the 00-index.tar file is stored.
--
-- @~/.cabal/packages/hackage.haskell.org/00-index.tar@
--
-- Since 0.1.1.0
defaultIndexLocation :: IO FilePath
defaultIndexLocation = do
    cabalDir <- getAppUserDataDirectory "cabal"
    return $ cabalDir </> "packages" </> "hackage.haskell.org" </> "00-index.tar"

-- | Download the given name,version pairs into the directory expected by cabal.
--
-- Since 0.1.0.0
fetchPackages :: (F.Foldable f,Functor f,MonadIO m)
              => Settings -> f PackageIdentifier -> m ()
fetchPackages s pkgs = liftIO (do
     indexFP <- _indexLocation s
     packageLocation <- _packageLocation s
     withAsync (getPackageInfo indexFP $
                Set.fromList $
                F.toList pkgs) $ \a -> do
         man <- _getManager s
         parMapM_ (_connections s) (go packageLocation man (wait a)) pkgs)
  where
    unlessM p f = do
        p' <- p
        unless p' f

    go :: (PackageIdentifier -> FilePath) -> Manager -> IO (Map PackageIdentifier Package) -> PackageIdentifier -> IO ()
    go packageLocation man getPackageInfo' ident = do
        unlessM (doesFileExist fp) $ do
            _onDownload s pkg
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
                else _onDownloadErr s pkg
      where
        pkg = packageIdentifierString ident
        targz = pkg ++ ".tar.gz"
        defUrl = _downloadPrefix s ++ targz
        fp = packageLocation ident

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

packageLocationGetter :: Settings
                      -> (forall m. MonadIO m => m (PackageIdentifier -> FilePath))
packageLocationGetter = _packageLocation
