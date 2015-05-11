{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Fetching package archives.

module Stackage.Fetch
  (fetchPackage
  ,packageUnpacked)
  where

import           Codec.Archive.Tar
import           Codec.Compression.GZip as GZip
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger (logDebug,MonadLogger)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Data
import           Data.List
import           Data.Maybe
import qualified Data.Text as T
import           Distribution.Text (display)
import           Distribution.Version
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Path as FL
import           Prelude hiding (FilePath)
import           Stackage.PackageIndex
import           Stackage.PackageName
import           System.Directory
import           System.IO
import           System.IO.Temp

-- | A fetching exception.
data StackageFetchException =
  FPPackageDownloadError PackageName
                         (Response L.ByteString)
  deriving (Show,Typeable)
instance Exception StackageFetchException

-- | Fetch the package index.
-- Example usage: runStdoutLoggingT (fetchPackage $(mkAbsoluteDir "/home/chris/.stackage/pkg-index") (fromJust (parsePackageName "lens")) (fromJust (parseVersion "4.6.0.1")))
fetchPackage :: (MonadMask m,MonadLogger m,MonadThrow m,MonadIO m)
             => PackageIndex -> PackageName -> Version -> m (Path Abs Dir)
fetchPackage (PackageIndex dir) name ver =
  do unpacked <-
       packageUnpacked (PackageIndex dir)
                       name
                       ver
     if unpacked
        then return pkgVerContentsDir
        else do req <- parseUrl url
                liftIO (putStrLn ((packageNameString name) ++
                                  ": downloading " ++ display ver))
                resp <-
                  liftIO (withManager defaultManagerSettings
                                      (httpLbs req))
                case responseStatus resp of
                  Status 200 _ ->
                    withSystemTempFile
                      "pkg-index"
                      (\fp h ->
                         do indexCabalFile <-
                              liftIO (S.readFile oldCabalFilePath)
                            $logDebug (T.pack ("Decompressing " ++
                                               (packageNameString name)))
                            liftIO (L.hPutStr h (GZip.decompress (responseBody resp)))
                            liftIO (hClose h)
                            $logDebug (T.pack ("Extracting to " ++
                                               FL.toFilePath pkgVerDir))
                            liftIO (extract (FL.toFilePath pkgVerDir) fp)
                            $logDebug (T.pack ("Updating cabal file " ++
                                               newCabalFilePath))
                            liftIO (S.writeFile newCabalFilePath indexCabalFile)
                            return pkgVerContentsDir)
                  _ ->
                    liftIO (throwIO (FPPackageDownloadError name resp))
  where newCabalFilePath =
          FL.toFilePath
            (pkgVerDir </>
             (fromMaybe (error "Unable to make valid .cabal file name.")
                        (parseRelFile
                           (nameVer ++ "/" ++ packageNameString name ++ ".cabal"))))
        oldCabalFilePath =
          FL.toFilePath
            (pkgVerDir </>
             (fromMaybe (error "Unable to make valid .cabal file name.")
                        (parseRelFile (packageNameString name ++ ".cabal"))))
        url =
          concat ["http://hackage.haskell.org/package/"
                 ,nameVer
                 ,"/"
                 ,nameVer
                 ,".tar.gz"] -- TODO: customize this.
        nameVer =
          (packageNameString name) ++
          "-" ++ display ver
        pkgVerContentsDir :: Path Abs Dir
        pkgVerContentsDir =
          mkPkgVerContentsDir dir name ver
        pkgVerDir :: Path Abs Dir
        pkgVerDir = mkPkgVerDir dir name ver

-- | Has the package been unpacked already?
packageUnpacked :: (MonadIO m)
                => PackageIndex -> PackageName -> Version -> m Bool
packageUnpacked (PackageIndex dir) name ver =
  liftIO (doesDirectoryExist (FL.toFilePath (mkPkgVerContentsDir dir name ver)))

-- | Make the directory for the package version (with a single .cabal file in it).
mkPkgVerDir :: Path Abs Dir -> PackageName -> Version -> Path Abs Dir
mkPkgVerDir dir name ver =
  dir </>
  fromMaybe (error "Unable to make valid path name for package-version.")
            (parseRelDir (packageNameString name ++ "/" ++ display ver))

-- | Make the directory for the package contents (with the .cabal and sources, etc).
mkPkgVerContentsDir :: Path Abs Dir -> PackageName -> Version -> Path Abs Dir
mkPkgVerContentsDir dir name ver =
  mkPkgVerDir dir name ver </>
  fromMaybe (error "Unable to make valid path name for package-version.")
            (parseRelDir (packageNameString name ++ "-" ++ display ver))
