{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Pantry.Tree
  ( unpackTree
  , rawParseGPD
  , unpackTreeToDir
  , hpackToCabalBS
  ) where

import RIO
import RIO.Process
import qualified RIO.FilePath as FilePath
import qualified Hpack.Config as Hpack
import qualified RIO.Map as Map
import qualified RIO.Text as T
import qualified RIO.ByteString as B
import Pantry.Storage
import Pantry.Types
import RIO.FilePath ((</>), takeDirectory)
import RIO.Directory (createDirectoryIfMissing, setPermissions, getPermissions, setOwnerExecutable)
import Path (Path, Abs, Dir, toFilePath, parseAbsDir, fromAbsFile)
import Pantry.HPack (findOrGenerateCabalFile)
import Distribution.Parsec.Common (PWarning (..))
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parsec
import Path (File)

unpackTreeToDir
  :: (HasPantryConfig env, HasLogFunc env)
  => Path Abs Dir -- ^ dest dir, will be created if necessary
  -> Tree
  -> RIO env ()
unpackTreeToDir (toFilePath -> dir) (TreeMap m) = do
  withStorage $ for_ (Map.toList m) $ \(sfp, TreeEntry blobKey ft) -> do
    let dest = dir </> T.unpack (unSafeFilePath sfp)
    createDirectoryIfMissing True $ takeDirectory dest
    mbs <- loadBlob blobKey
    case mbs of
      Nothing -> do
        -- TODO when we have pantry wire stuff, try downloading
        throwIO $ TreeReferencesMissing sfp blobKey
      Just bs -> do
        B.writeFile dest bs
        case ft of
          FTNormal -> pure ()
          FTExecutable -> liftIO $ do
            perms <- getPermissions dest
            setPermissions dest $ setOwnerExecutable True perms


unpackTree
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocationImmutable -- for exceptions
  -> Path Abs Dir -- ^ dest dir, will be created if necessary
  -> Tree
  -> RIO env ()
unpackTree loc (toFilePath -> dir) (TreeMap m) = do
  withStorage $ for_ (Map.toList m) $ \(sfp, TreeEntry blobKey ft) -> do
    let dest = dir </> T.unpack (unSafeFilePath sfp)
    createDirectoryIfMissing True $ takeDirectory dest
    mbs <- loadBlob blobKey
    case mbs of
      Nothing -> do
        -- TODO when we have pantry wire stuff, try downloading
        throwIO $ TreeReferencesMissingBlob loc sfp blobKey
      Just bs -> do
        B.writeFile dest bs
        case ft of
          FTNormal -> pure ()
          FTExecutable -> liftIO $ do
            perms <- getPermissions dest
            setPermissions dest $ setOwnerExecutable True perms

-- | A helper function that performs the basic character encoding
-- necessary.
rawParseGPD
  :: MonadThrow m
  => Either PackageLocationImmutable (Path Abs File)
  -> ByteString
  -> m ([PWarning], GenericPackageDescription)
rawParseGPD loc bs =
    case eres of
      Left (mversion, errs) -> throwM $ InvalidCabalFile loc mversion errs warnings
      Right gpkg -> return (warnings, gpkg)
  where
    (warnings, eres) = runParseResult $ parseGenericPackageDescription bs

hpackToCabalBS :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
           => ByteString -- Hpack's content
           -> Tree
           -> RIO env (PackageName, ByteString)
hpackToCabalBS hpackBs tree = withSystemTempDirectory "hpack-pkg-dir" $ \tmpdir -> withWorkingDir tmpdir $ do
               B.writeFile (tmpdir FilePath.</> Hpack.packageConfig) hpackBs
               tdir <- parseAbsDir tmpdir
               unpackTreeToDir tdir tree
               (packageName, cfile) <- findOrGenerateCabalFile tdir
               bs <- B.readFile (fromAbsFile cfile)
               return (packageName, bs)
