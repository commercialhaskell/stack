{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Functions for the GHC package database.

module Stack.GhcPkg
  (getGlobalDB
  ,findGhcPkgField
  ,createDatabase
  ,unregisterGhcPkgId
  ,getCabalPkgVer
  ,ghcPkgExeName
  ,ghcPkgPathEnvVar
  ,mkGhcPackagePath)
  where

import           Stack.Prelude
import qualified Data.ByteString.Char8 as S8
import           Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Path (parent, mkRelFile, (</>))
import           Path.Extra (toFilePathNoTrailingSep)
import           Path.IO
import           Stack.Constants
import           Stack.Types.Build
import           Stack.Types.GhcPkgId
import           Stack.Types.PackageIdentifier
import           Stack.Types.Compiler
import           Stack.Types.PackageName
import           Stack.Types.Version
import           System.FilePath (searchPathSeparator)
import           System.Process.Read

-- | Get the global package database
getGlobalDB :: (MonadUnliftIO m, MonadLogger m)
            => EnvOverride -> WhichCompiler -> m (Path Abs Dir)
getGlobalDB menv wc = do
    $logDebug "Getting global package database location"
    -- This seems like a strange way to get the global package database
    -- location, but I don't know of a better one
    bs <- ghcPkg menv wc [] ["list", "--global"] >>= either throwIO return
    let fp = S8.unpack $ stripTrailingColon $ firstLine bs
    liftIO $ resolveDir' fp
  where
    stripTrailingColon bs
        | S8.null bs = bs
        | S8.last bs == ':' = S8.init bs
        | otherwise = bs
    firstLine = S8.takeWhile (\c -> c /= '\r' && c /= '\n')

-- | Run the ghc-pkg executable
ghcPkg :: (MonadUnliftIO m, MonadLogger m)
       => EnvOverride
       -> WhichCompiler
       -> [Path Abs Dir]
       -> [String]
       -> m (Either ReadProcessException S8.ByteString)
ghcPkg menv wc pkgDbs args = do
    eres <- go
    case eres of
          Left _ -> do
              mapM_ (createDatabase menv wc) pkgDbs
              go
          Right _ -> return eres
  where
    go = tryProcessStdout Nothing menv (ghcPkgExeName wc) args'
    args' = packageDbFlags pkgDbs ++ args

-- | Create a package database in the given directory, if it doesn't exist.
createDatabase :: (MonadUnliftIO m, MonadLogger m)
               => EnvOverride -> WhichCompiler -> Path Abs Dir -> m ()
createDatabase menv wc db = do
    exists <- doesFileExist (db </> $(mkRelFile "package.cache"))
    unless exists $ do
        -- ghc-pkg requires that the database directory does not exist
        -- yet. If the directory exists but the package.cache file
        -- does, we're in a corrupted state. Check for that state.
        dirExists <- doesDirExist db
        args <- if dirExists
            then do
                $logWarn $ T.pack $ concat
                    [ "The package database located at "
                    , toFilePath db
                    , " is corrupted (missing its package.cache file)."
                    ]
                $logWarn "Proceeding with a recache"
                return ["--package-db", toFilePath db, "recache"]
            else do
                -- Creating the parent doesn't seem necessary, as ghc-pkg
                -- seems to be sufficiently smart. But I don't feel like
                -- finding out it isn't the hard way
                ensureDir (parent db)
                return ["init", toFilePath db]
        eres <- tryProcessStdout Nothing menv (ghcPkgExeName wc) args
        case eres of
            Left e -> do
                $logError $ T.pack $ "Unable to create package database at " ++ toFilePath db
                throwIO e
            Right _ -> return ()

-- | Get the name to use for "ghc-pkg", given the compiler version.
ghcPkgExeName :: WhichCompiler -> String
ghcPkgExeName Ghc = "ghc-pkg"
ghcPkgExeName Ghcjs = "ghcjs-pkg"

-- | Get the environment variable to use for the package DB paths.
ghcPkgPathEnvVar :: WhichCompiler -> Text
ghcPkgPathEnvVar Ghc = "GHC_PACKAGE_PATH"
ghcPkgPathEnvVar Ghcjs = "GHCJS_PACKAGE_PATH"

-- | Get the necessary ghc-pkg flags for setting up the given package database
packageDbFlags :: [Path Abs Dir] -> [String]
packageDbFlags pkgDbs =
          "--no-user-package-db"
        : map (\x -> "--package-db=" ++ toFilePath x) pkgDbs

-- | Get the value of a field of the package.
findGhcPkgField
    :: (MonadUnliftIO m, MonadLogger m)
    => EnvOverride
    -> WhichCompiler
    -> [Path Abs Dir] -- ^ package databases
    -> String -- ^ package identifier, or GhcPkgId
    -> Text
    -> m (Maybe Text)
findGhcPkgField menv wc pkgDbs name field = do
    result <-
        ghcPkg
            menv
            wc
            pkgDbs
            ["field", "--simple-output", name, T.unpack field]
    return $
        case result of
            Left{} -> Nothing
            Right lbs ->
                fmap (stripCR . T.decodeUtf8) $ listToMaybe $ S8.lines lbs

-- | Get the version of the package
findGhcPkgVersion :: (MonadUnliftIO m, MonadLogger m)
                  => EnvOverride
                  -> WhichCompiler
                  -> [Path Abs Dir] -- ^ package databases
                  -> PackageName
                  -> m (Maybe Version)
findGhcPkgVersion menv wc pkgDbs name = do
    mv <- findGhcPkgField menv wc pkgDbs (packageNameString name) "version"
    case mv of
        Just !v -> return (parseVersion v)
        _ -> return Nothing

unregisterGhcPkgId :: (MonadUnliftIO m, MonadLogger m)
                    => EnvOverride
                    -> WhichCompiler
                    -> CompilerVersion 'CVActual
                    -> Path Abs Dir -- ^ package database
                    -> GhcPkgId
                    -> PackageIdentifier
                    -> m ()
unregisterGhcPkgId menv wc cv pkgDb gid ident = do
    eres <- ghcPkg menv wc [pkgDb] args
    case eres of
        Left e -> $logWarn $ T.pack $ show e
        Right _ -> return ()
  where
    -- TODO ideally we'd tell ghc-pkg a GhcPkgId instead
    args = "unregister" : "--user" : "--force" :
        (case cv of
            GhcVersion v | v < $(mkVersion "7.9") ->
                [packageIdentifierString ident]
            _ -> ["--ipid", ghcPkgIdString gid])

-- | Get the version of Cabal from the global package database.
getCabalPkgVer :: (MonadUnliftIO m, MonadLogger m)
               => EnvOverride -> WhichCompiler -> m Version
getCabalPkgVer menv wc = do
    $logDebug "Getting Cabal package version"
    mres <- findGhcPkgVersion
        menv
        wc
        [] -- global DB
        cabalPackageName
    maybe (throwIO $ Couldn'tFindPkgId cabalPackageName) return mres

-- | Get the value for GHC_PACKAGE_PATH
mkGhcPackagePath :: Bool -> Path Abs Dir -> Path Abs Dir -> [Path Abs Dir] -> Path Abs Dir -> Text
mkGhcPackagePath locals localdb deps extras globaldb =
  T.pack $ intercalate [searchPathSeparator] $ concat
    [ [toFilePathNoTrailingSep localdb | locals]
    , [toFilePathNoTrailingSep deps]
    , [toFilePathNoTrailingSep db | db <- reverse extras]
    , [toFilePathNoTrailingSep globaldb]
    ]
