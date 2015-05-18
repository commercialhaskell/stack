{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ViewPatterns       #-}

-- | Functionality for downloading packages securely for cabal's usage.

module Stack.Fetch
    ( fetchPackages
    ) where

import           Control.Monad.IO.Class
import           Stack.Types

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
import           Network.HTTP.Download
import           Network.HTTP.Client      (Manager, brRead, checkStatus,
                                           responseBody,
                                           responseStatus, withResponse)
import           Network.HTTP.Types (statusCode)
import           Stack.Constants
import           Stack.Config
import Path
import           System.Directory         (createDirectoryIfMissing,
                                           doesFileExist,
                                           renameFile)
import           System.FilePath (takeDirectory, (<.>), takeExtension)
import           System.IO                (IOMode (ReadMode, WriteMode),
                                           withBinaryFile)

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
                do name' <- parsePackageNameFromString (T.unpack pkg)
                   ver' <- parseVersionFromString (T.unpack ver)
                   return (PackageIdentifier name' ver')
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

-- | Download the given name,version pairs into the directory expected by cabal.
--
-- Since 0.1.0.0
fetchPackages :: (F.Foldable f,Functor f,MonadIO m,MonadReader env m,HasHttpManager env,HasConfig env)
              => f PackageIdentifier -> m ()
fetchPackages pkgs = do
   env <- ask
   let man = getHttpManager env
       config = getConfig env
       indexFP = toFilePath $ configPackageIndex config
   liftIO (do
     let packageLocation = configPackageTarball config
     withAsync (getPackageInfo indexFP $
                Set.fromList $
                F.toList pkgs) $ \a -> do
         parMapM_ connectionCount (go packageLocation man (wait a)) pkgs)
  where
    connectionCount = 8 -- FIXME put in Config
    unlessM p f = do
        p' <- p
        unless p' f

    go :: (PackageIdentifier -> IO (Path Abs File)) -> Manager -> IO (Map PackageIdentifier Package) -> PackageIdentifier -> IO ()
    go packageLocation man getPackageInfo' ident = do
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
