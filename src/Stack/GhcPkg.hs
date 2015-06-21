{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Functions for the GHC package database.

module Stack.GhcPkg
  (findGhcPkgId
  ,getGlobalDB
  ,EnvOverride
  ,envHelper
  ,createDatabase
  ,unregisterGhcPkgId
  ,getCabalPkgVer)
  where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Char8 as S8
import           Data.Either
import           Data.List
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Path (Path, Abs, Dir, toFilePath, parent, parseAbsDir)
import           Prelude hiding (FilePath)
import           Stack.Build.Types (StackBuildException (Couldn'tFindPkgId))
import           Stack.Constants
import           Stack.Types
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, canonicalizePath)
import           System.Process.Read

-- | Get the global package database
getGlobalDB :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
            => EnvOverride -> m (Path Abs Dir)
getGlobalDB menv = do
    -- This seems like a strange way to get the global package database
    -- location, but I don't know of a better one
    bs <- ghcPkg menv [] ["list", "--global"] >>= either throwM return
    let fp = S8.unpack $ stripTrailingColon $ firstLine bs
    liftIO (canonicalizePath fp) >>= parseAbsDir
  where
    stripTrailingColon bs
        | S8.null bs = bs
        | S8.last bs == ':' = S8.init bs
        | otherwise = bs
    firstLine = S8.takeWhile (\c -> c /= '\r' && c /= '\n')

-- | Run the ghc-pkg executable
ghcPkg :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
       => EnvOverride
       -> [Path Abs Dir]
       -> [String]
       -> m (Either ReadProcessException S8.ByteString)
ghcPkg menv pkgDbs args = do
    eres <- go
    r <- case eres of
            Left _ -> do
                mapM_ (createDatabase menv) pkgDbs
                go
            Right _ -> return eres
    return r
  where
    go = tryProcessStdout Nothing menv "ghc-pkg" args'
    args' = packageDbFlags pkgDbs ++ args

-- | Create a package database in the given directory, if it doesn't exist.
createDatabase :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
               => EnvOverride -> Path Abs Dir -> m ()
createDatabase menv db = do
    let db' = toFilePath db
    exists <- liftIO $ doesDirectoryExist db'
    unless exists $ do
        -- Creating the parent doesn't seem necessary, as ghc-pkg
        -- seems to be sufficiently smart. But I don't feel like
        -- finding out it isn't the hard way
        liftIO $ createDirectoryIfMissing True $ toFilePath $ parent db
        _ <- tryProcessStdout Nothing menv "ghc-pkg" ["init", db']
        return ()

-- | Get the necessary ghc-pkg flags for setting up the given package database
packageDbFlags :: [Path Abs Dir] -> [String]
packageDbFlags pkgDbs =
          "--no-user-package-db"
        : map (\x -> ("--package-db=" ++ toFilePath x)) pkgDbs

-- | Get the id of the package e.g. @foo-0.0.0-9c293923c0685761dcff6f8c3ad8f8ec@.
findGhcPkgId :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
             => EnvOverride
             -> [Path Abs Dir] -- ^ package databases
             -> PackageName
             -> m (Maybe GhcPkgId)
findGhcPkgId menv pkgDbs name = do
    result <-
        ghcPkg menv pkgDbs ["describe", packageNameString name]
    case result of
        Left{} ->
            return Nothing
        Right lbs -> do
            let mpid =
                    fmap
                        T.encodeUtf8
                        (listToMaybe
                             (mapMaybe
                                  (fmap stripCR .
                                   T.stripPrefix "id: ")
                                  (map T.decodeUtf8 (S8.lines lbs))))
            case mpid of
                Just !pid ->
                    return (parseGhcPkgId pid)
                _ ->
                    return Nothing
  where
    stripCR t =
        fromMaybe t (T.stripSuffix "\r" t)

unregisterGhcPkgId :: (MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m, MonadBaseControl IO m)
                    => EnvOverride
                    -> Path Abs Dir -- ^ package database
                    -> GhcPkgId
                    -> m ()
unregisterGhcPkgId menv pkgDb gid = do
    eres <- ghcPkg menv [pkgDb] args
    case eres of
        Left e -> $logWarn $ T.pack $ show e
        Right _ -> return ()
  where
    -- TODO ideally we'd tell ghc-pkg a GhcPkgId instead
    args = ["unregister", "--user", "--force", packageIdentifierString $ ghcPkgIdPackageIdentifier gid]

-- | Get the version of Cabal from the global package database.
getCabalPkgVer :: (MonadThrow m, MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
               => EnvOverride -> m Version
getCabalPkgVer menv =
    findGhcPkgId
        menv
        [] -- global DB
        cabalPackageName >>=
        maybe
            (throwM $ Couldn'tFindPkgId cabalPackageName)
            (return . packageIdentifierVersion . ghcPkgIdPackageIdentifier)
