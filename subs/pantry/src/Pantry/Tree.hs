{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Pantry.Tree
  ( unpackTree
  , findCabalFile
  , checkTreeKey
  , checkPackageMetadata
  , loadPackageIdentFromTree
  , rawParseGPD
  ) where

import RIO
import qualified RIO.Map as Map
import qualified RIO.Text as T
import qualified RIO.ByteString as B
import Pantry.Storage
import Pantry.Types
import RIO.FilePath ((</>), takeDirectory)
import RIO.Directory (createDirectoryIfMissing, setPermissions, getPermissions, setOwnerExecutable)
import Path (Path, Abs, Dir, toFilePath)
import Distribution.Parsec.Common (PWarning (..))
import Distribution.PackageDescription (packageDescription, package, GenericPackageDescription)
import Distribution.PackageDescription.Parsec
import Path (File)

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

findCabalFile
  :: MonadThrow m
  => PackageLocationImmutable -- ^ for exceptions
  -> Tree
  -> m (SafeFilePath, TreeEntry)
findCabalFile loc (TreeMap m) = do
  let isCabalFile (sfp, _) =
        let txt = unSafeFilePath sfp
         in not ("/" `T.isInfixOf` txt) && ".cabal" `T.isSuffixOf` txt
  case filter isCabalFile $ Map.toList m of
    [] -> throwM $ TreeWithoutCabalFile loc
    [(key, te)] -> pure (key, te)
    xs -> throwM $ TreeWithMultipleCabalFiles loc $ map fst xs

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

-- | Returns the cabal blob key
loadPackageIdentFromTree -- FIXME investigate overlap with loadCabalFile and parsing functions in Pantry module
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocationImmutable
  -> Tree
  -> RIO env (BlobKey, PackageIdentifier)
loadPackageIdentFromTree pl tree = do -- FIXME store this in a table to avoid the slow Cabal file parser
  (sfp, TreeEntry cabalBlobKey _) <- findCabalFile pl tree
  mbs <- withStorage $ loadBlob cabalBlobKey
  bs <-
    case mbs of
      Nothing -> throwIO $ TreeReferencesMissingBlob pl sfp cabalBlobKey
      Just bs -> pure bs
  (_warnings, gpd) <- rawParseGPD (Left pl) bs
  let ident@(PackageIdentifier name _) = package $ packageDescription $ gpd
  when (unSafeFilePath sfp /= displayC name <> ".cabal") $
    throwIO $ WrongCabalFileName pl sfp name
  pure (cabalBlobKey, ident)

-- ensure name, version, etc are correct
checkPackageMetadata
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocationImmutable
  -> PackageMetadata
  -> RIO env (TreeKey, Tree)
  -> RIO env (TreeKey, Tree)
checkPackageMetadata pl pm inner = do
  (treeKey, tree) <- checkTreeKey pl (pmTree pm) inner
  -- even if we aren't given a name and version, still load this to
  -- force the check of the cabal file name being accurate
  (cabalBlobKey, ident@(PackageIdentifier name version))
    <- loadPackageIdentFromTree pl tree
  let err = throwIO $ MismatchedPackageMetadata pl pm cabalBlobKey ident
  for_ (pmName pm) $ \name' -> when (name /= name') err
  for_ (pmVersion pm) $ \version' -> when (version /= version') err
  for_ (pmCabal pm) $ \cabal' -> when (cabalBlobKey /= cabal') err
  pure (treeKey, tree)

checkTreeKey
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocationImmutable
  -> Maybe TreeKey
  -> RIO env (TreeKey, Tree)
  -> RIO env (TreeKey, Tree)
checkTreeKey _ Nothing inner = inner
checkTreeKey pl (Just expectedTreeKey) inner = do
  mtree <- withStorage $ loadTree expectedTreeKey
  case mtree of
    Just tree -> pure (expectedTreeKey, tree)
    Nothing -> do
      res@(actualTreeKey, _) <- inner
      -- We do not need to store the tree at this point, it's the
      -- responsibility of inner to do that.
      when (actualTreeKey /= expectedTreeKey) $
          throwIO $ TreeKeyMismatch pl Mismatch
            { mismatchExpected = expectedTreeKey
            , mismatchActual = actualTreeKey
            }
      pure res
