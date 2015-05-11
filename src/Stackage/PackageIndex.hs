{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Package index handling.

module Stackage.PackageIndex
  (PackageIndex(..)
  ,PackageIndexException
  ,getPkgIndex
  ,loadPkgIndex
  ,downloadPkgIndex
  ,getPkgVersions)
  where

import           Codec.Archive.Tar
import           Codec.Compression.GZip as GZip
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger (logDebug,MonadLogger)
import qualified Data.ByteString.Lazy as L
import           Data.Data
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Version as V ( parseVersion )
import           Distribution.Version
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Path as FL
import           Prelude hiding (FilePath)
import           Stackage.PackageName
import           System.Directory
import           System.IO
import           System.IO.Temp
import qualified Text.ParserCombinators.ReadP as ReadP ( readP_to_S )

data PackageIndexException =
  FPIndexDownloadError (Response L.ByteString)
 deriving (Show,Typeable)
instance Exception PackageIndexException

-- | Wrapper to an existant package index.
newtype PackageIndex =
  PackageIndex (Path Abs Dir)

-- | Try to get the package index.
getPkgIndex :: MonadIO m => Path Abs Dir -> m (Maybe PackageIndex)
getPkgIndex dir =
  do exists <-
       liftIO (doesDirectoryExist (FL.toFilePath dir))
     return (if exists
                then Just (PackageIndex dir)
                else Nothing)

-- | Load the package index, if it does not exist, download it.
loadPkgIndex :: (MonadMask m,MonadLogger m,MonadThrow m,MonadIO m)
             => Path Abs Dir -> m PackageIndex
loadPkgIndex dir =
  do mindex <- liftIO (getPkgIndex dir)
     case mindex of
       Just index -> return index
       Nothing ->
         do liftIO (putStrLn "No package index. Downloading latest ...")
            index <- downloadPkgIndex dir "http://hackage.haskell.org/packages/archive/00-index.tar.gz"
            liftIO (putStrLn "Downloaded and unpacked package index.")
            return index

-- | Get the package index.
-- TODO: Catch http exceptions.
-- Example usage:
-- getPkgIndex $(mkAbsoluteDir "/home/chris/.stackage/pkg-index") "http://hackage.haskell.org/packages/archive/00-index.tar.gz"
downloadPkgIndex :: (MonadMask m,MonadLogger m,MonadThrow m,MonadIO m)
                 => Path Abs Dir -> String -> m PackageIndex
downloadPkgIndex dir url =
  do req <- parseUrl url
     $logDebug "Downloading package index ..."
     resp <-
       liftIO (withManager defaultManagerSettings
                           (httpLbs req))
     case responseStatus resp of
       Status 200 _ ->
         withSystemTempFile
           "pkg-index"
           (\fp h ->
              do $logDebug "Decompressing ..."
                 liftIO (L.hPutStr h (GZip.decompress (responseBody resp)))
                 liftIO (hClose h)
                 $logDebug "Extracting ..."
                 liftIO (createDirectoryIfMissing True (FL.toFilePath dir))
                 liftIO (extract (FL.toFilePath dir) fp)
                 return (PackageIndex dir))
       _ ->
         liftIO (throwIO (FPIndexDownloadError resp))

-- | Get versions available for the given package in the index.
getPkgVersions :: MonadIO m => PackageIndex -> PackageName -> m (Maybe (Set Version))
getPkgVersions (PackageIndex dir) name =
  liftIO (do exists <-
               doesDirectoryExist (FL.toFilePath pkgDir)
             if exists
                then do contents <-
                          fmap (mapMaybe parseVersion)
                               (getDirectoryContents (FL.toFilePath pkgDir))
                        return (Just (S.fromList contents))
                else return Nothing)
  where pkgDir =
          dir FL.</>
          (fromMaybe (error "Unable to produce valid directory name for package.")
                     (parseRelDir (packageNameString name)))

-- | Parse a package version.
parseVersion :: String -> Maybe Version
parseVersion s =
  case reverse (ReadP.readP_to_S V.parseVersion s) of
    ((ver,""):_) -> Just ver
    _ -> Nothing
