{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions for the GHC package database.

module Stack.GhcPkg
  ( createDatabase
  , findGhcPkgField
  , getGlobalDB
  , ghcPkgPathEnvVar
  , mkGhcPackagePath
  , unregisterGhcPkgIds
  ) where

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Path ( (</>), parent )
import           Path.Extra ( toFilePathNoTrailingSep )
import           Path.IO
                   ( doesDirExist, doesFileExist, ensureDir, resolveDir' )
import           RIO.Process ( HasProcessContext, proc, readProcess_ )
import           Stack.Constants ( relFilePackageCache )
import           Stack.Prelude
import           Stack.Types.Config ( GhcPkgExe (..) )
import           Stack.Types.GhcPkgId ( GhcPkgId, ghcPkgIdString )
import           Stack.Types.Compiler ( WhichCompiler (..) )
import           System.FilePath ( searchPathSeparator )

-- | Get the global package database
getGlobalDB ::
     (HasProcessContext env, HasLogFunc env)
  => GhcPkgExe
  -> RIO env (Path Abs Dir)
getGlobalDB pkgexe = do
  logDebug "Getting global package database location"
  -- This seems like a strange way to get the global package database
  -- location, but I don't know of a better one
  bs <- ghcPkg pkgexe [] ["list", "--global"] >>= either throwIO pure
  let fp = S8.unpack $ stripTrailingColon $ firstLine bs
  liftIO $ resolveDir' fp
 where
  stripTrailingColon bs
    | S8.null bs = bs
    | S8.last bs == ':' = S8.init bs
    | otherwise = bs
  firstLine = S8.takeWhile (\c -> c /= '\r' && c /= '\n')

-- | Run the ghc-pkg executable
ghcPkg ::
     (HasProcessContext env, HasLogFunc env)
  => GhcPkgExe
  -> [Path Abs Dir]
  -> [String]
  -> RIO env (Either SomeException S8.ByteString)
ghcPkg pkgexe@(GhcPkgExe pkgPath) pkgDbs args = do
  eres <- go
  case eres of
    Left _ -> do
      mapM_ (createDatabase pkgexe) pkgDbs
      go
    Right _ -> pure eres
 where
  pkg = toFilePath pkgPath
  go = tryAny $ BL.toStrict . fst <$> proc pkg args' readProcess_
  args' = packageDbFlags pkgDbs ++ args

-- | Create a package database in the given directory, if it doesn't exist.
createDatabase ::
     (HasProcessContext env, HasLogFunc env)
  => GhcPkgExe
  -> Path Abs Dir
  -> RIO env ()
createDatabase (GhcPkgExe pkgPath) db = do
  exists <- doesFileExist (db </> relFilePackageCache)
  unless exists $ do
    -- ghc-pkg requires that the database directory does not exist
    -- yet. If the directory exists but the package.cache file
    -- does, we're in a corrupted state. Check for that state.
    dirExists <- doesDirExist db
    args <- if dirExists
      then do
        logWarn $
          "The package database located at " <>
          fromString (toFilePath db) <>
          " is corrupted (missing its package.cache file)."
        logWarn "Proceeding with a recache"
        pure ["--package-db", toFilePath db, "recache"]
      else do
        -- Creating the parent doesn't seem necessary, as ghc-pkg
        -- seems to be sufficiently smart. But I don't feel like
        -- finding out it isn't the hard way
        ensureDir (parent db)
        pure ["init", toFilePath db]
    void $ proc (toFilePath pkgPath) args $ \pc ->
      onException (readProcess_ pc) $
        logError $
          "Error: [S-9735]\n" <>
           "Unable to create package database at " <>
           fromString (toFilePath db)

-- | Get the environment variable to use for the package DB paths.
ghcPkgPathEnvVar :: WhichCompiler -> Text
ghcPkgPathEnvVar Ghc = "GHC_PACKAGE_PATH"

-- | Get the necessary ghc-pkg flags for setting up the given package database
packageDbFlags :: [Path Abs Dir] -> [String]
packageDbFlags pkgDbs =
    "--no-user-package-db"
  : map (\x -> "--package-db=" ++ toFilePath x) pkgDbs

-- | Get the value of a field of the package.
findGhcPkgField ::
     (HasProcessContext env, HasLogFunc env)
  => GhcPkgExe
  -> [Path Abs Dir] -- ^ package databases
  -> String -- ^ package identifier, or GhcPkgId
  -> Text
  -> RIO env (Maybe Text)
findGhcPkgField pkgexe pkgDbs name field = do
  result <-
    ghcPkg
      pkgexe
      pkgDbs
      ["field", "--simple-output", name, T.unpack field]
  pure $
    case result of
      Left{} -> Nothing
      Right bs ->
        fmap (stripCR . T.decodeUtf8) $ listToMaybe $ S8.lines bs

-- | unregister list of package ghcids, batching available from GHC 8.2.1,
-- see https://github.com/commercialhaskell/stack/issues/2662#issuecomment-460342402
-- using GHC package id where available (from GHC 7.9)
unregisterGhcPkgIds ::
     (HasProcessContext env, HasLogFunc env)
  => GhcPkgExe
  -> Path Abs Dir -- ^ package database
  -> NonEmpty (Either PackageIdentifier GhcPkgId)
  -> RIO env ()
unregisterGhcPkgIds pkgexe pkgDb epgids = do
  eres <- ghcPkg pkgexe [pkgDb] args
  case eres of
    Left e -> logWarn $ displayShow e
    Right _ -> pure ()
 where
  (idents, gids) = partitionEithers $ toList epgids
  args = "unregister" : "--user" : "--force" :
    map packageIdentifierString idents ++
    if null gids then [] else "--ipid" : map ghcPkgIdString gids

-- | Get the value for GHC_PACKAGE_PATH
mkGhcPackagePath :: Bool -> Path Abs Dir -> Path Abs Dir -> [Path Abs Dir] -> Path Abs Dir -> Text
mkGhcPackagePath locals localdb deps extras globaldb =
  T.pack $ L.intercalate [searchPathSeparator] $ concat
    [ [toFilePathNoTrailingSep localdb | locals]
    , [toFilePathNoTrailingSep deps]
    , [toFilePathNoTrailingSep db | db <- reverse extras]
    , [toFilePathNoTrailingSep globaldb]
    ]
