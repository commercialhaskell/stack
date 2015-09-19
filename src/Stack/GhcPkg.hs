-- FIXME See how much of this module can be deleted.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Functions for the GHC package database.

module Stack.GhcPkg
  (findGhcPkgId
  ,findGhcPkgKey
  ,getGlobalDB
  ,EnvOverride
  ,envHelper
  ,createDatabase
  ,unregisterGhcPkgId
  ,getCabalPkgVer
  ,findGhcPkgHaddockHtml
  ,findGhcPkgDepends
  ,findTransitiveGhcPkgDepends
  ,listGhcPkgDbs
  ,ghcPkgExeName
  ,mkGhcPackagePath)
  where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Char8 as S8
import           Data.Either
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Path (Path, Abs, Dir, toFilePath, parent, parseAbsDir)
import           Path.IO (dirExists, createTree)
import           Prelude hiding (FilePath)
import           Stack.Constants
import           Stack.Types
import           System.Directory (canonicalizePath, doesDirectoryExist)
import           System.FilePath (FilePath, searchPathSeparator, dropTrailingPathSeparator)
import           System.Process.Read

-- | Get the global package database
getGlobalDB :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
            => EnvOverride -> WhichCompiler -> m (Path Abs Dir)
getGlobalDB menv wc = do
    -- This seems like a strange way to get the global package database
    -- location, but I don't know of a better one
    bs <- ghcPkg menv wc [] ["list", "--global"] >>= either throwM return
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
       -> WhichCompiler
       -> [Path Abs Dir]
       -> [String]
       -> m (Either ReadProcessException S8.ByteString)
ghcPkg menv wc pkgDbs args = do
    eres <- go
    r <- case eres of
            Left _ -> do
                mapM_ (createDatabase menv wc) pkgDbs
                go
            Right _ -> return eres
    return r
  where
    go = tryProcessStdout Nothing menv (ghcPkgExeName wc) args'
    args' = packageDbFlags pkgDbs ++ args

-- | Create a package database in the given directory, if it doesn't exist.
createDatabase :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
               => EnvOverride -> WhichCompiler -> Path Abs Dir -> m ()
createDatabase menv wc db = do
    exists <- dirExists db
    unless exists $ do
        -- Creating the parent doesn't seem necessary, as ghc-pkg
        -- seems to be sufficiently smart. But I don't feel like
        -- finding out it isn't the hard way
        createTree (parent db)
        _ <- tryProcessStdout Nothing menv (ghcPkgExeName wc) ["init", toFilePath db]
        return ()

-- | Get the name to use for "ghc-pkg", given the compiler version.
ghcPkgExeName :: WhichCompiler -> String
ghcPkgExeName Ghc = "ghc-pkg"
ghcPkgExeName Ghcjs = "ghcjs-pkg"

-- | Get the necessary ghc-pkg flags for setting up the given package database
packageDbFlags :: [Path Abs Dir] -> [String]
packageDbFlags pkgDbs =
          "--no-user-package-db"
        : map (\x -> ("--package-db=" ++ toFilePath x)) pkgDbs

-- | Get the value of a field of the package.
findGhcPkgField
    :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
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
  where
    stripCR t = fromMaybe t (T.stripSuffix "\r" t)

-- | Get the id of the package e.g. @foo-0.0.0-9c293923c0685761dcff6f8c3ad8f8ec@.
findGhcPkgId :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
             => EnvOverride
             -> WhichCompiler
             -> [Path Abs Dir] -- ^ package databases
             -> PackageName
             -> m (Maybe GhcPkgId)
findGhcPkgId menv wc pkgDbs name = do
    mpid <- findGhcPkgField menv wc pkgDbs (packageNameString name) "id"
    case mpid of
        Just !pid -> return (parseGhcPkgId (T.encodeUtf8 pid))
        _ -> return Nothing

-- | Get the package key e.g. @foo_9bTCpMF7G4UFWJJvtDrIdB@.
--
-- NOTE: GHC > 7.10 only! Will always yield 'Nothing' otherwise.
findGhcPkgKey :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
             => EnvOverride
             -> WhichCompiler
             -> [Path Abs Dir] -- ^ package databases
             -> PackageName
             -> m (Maybe Text)
findGhcPkgKey menv wc pkgDbs name =
    findGhcPkgField menv wc pkgDbs (packageNameString name) "key"

-- | Get the version of the package
findGhcPkgVersion :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
                  => EnvOverride
                  -> WhichCompiler
                  -> [Path Abs Dir] -- ^ package databases
                  -> PackageName
                  -> m (Maybe Version)
findGhcPkgVersion menv wc pkgDbs name = do
    mv <- findGhcPkgField menv wc pkgDbs (packageNameString name) "version"
    case mv of
        Just !v -> return (parseVersion (T.encodeUtf8 v))
        _ -> return Nothing

-- | Get the Haddock HTML documentation path of the package.
findGhcPkgHaddockHtml :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
                      => EnvOverride
                      -> WhichCompiler
                      -> [Path Abs Dir] -- ^ package databases
                      -> String -- ^ PackageIdentifier or GhcPkgId
                      -> m (Maybe (PackageIdentifier, Path Abs Dir))
findGhcPkgHaddockHtml menv wc pkgDbs ghcPkgId = do
    mpath <- findGhcPkgField menv wc pkgDbs ghcPkgId "haddock-html"
    mid <- findGhcPkgField menv wc pkgDbs ghcPkgId "id"
    mversion <- findGhcPkgField menv wc pkgDbs ghcPkgId "version"
    let mpkgId = PackageIdentifier
            <$> (mid >>= parsePackageName . T.encodeUtf8)
            <*> (mversion >>= parseVersion . T.encodeUtf8)
    case (,) <$> mpath <*> mpkgId of
        Just (path0, pkgId) -> do
            let path = T.unpack path0
            exists <- liftIO $ doesDirectoryExist path
            path' <- if exists
                then liftIO $ canonicalizePath path
                else return path

            return $ fmap (pkgId,) (parseAbsDir path')
        _ -> return Nothing

-- | Finds dependencies of package, and all their dependencies, etc.
findTransitiveGhcPkgDepends
    :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
    => EnvOverride
    -> WhichCompiler
    -> [Path Abs Dir] -- ^ package databases
    -> PackageIdentifier
    -> m (Set PackageIdentifier)
findTransitiveGhcPkgDepends menv wc pkgDbs pkgId0 =
    liftM (Set.fromList . Map.elems)
    (go (packageIdentifierString pkgId0) Map.empty)
  where
    go pkgId res = do
        deps <- findGhcPkgDepends menv wc pkgDbs pkgId
        loop deps res
    loop [] res = return res
    loop (dep:deps) res = do
        if Map.member dep res
            then loop deps res
            else do
                let pkgId = ghcPkgIdString dep
                mname <- findGhcPkgField menv wc pkgDbs pkgId "name"
                mversion <- findGhcPkgField menv wc pkgDbs pkgId "version"
                let mident = do
                        name <- mname >>= parsePackageName . T.encodeUtf8
                        version <- mversion >>= parseVersion . T.encodeUtf8
                        Just $ PackageIdentifier name version
                    res' = maybe id (Map.insert dep) mident res
                res'' <- go pkgId res'
                -- FIXME is the Map.union actually necessary?
                loop deps (Map.union res res'')

-- | Get the dependencies of the package.
findGhcPkgDepends :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
                  => EnvOverride
                  -> WhichCompiler
                  -> [Path Abs Dir] -- ^ package databases
                  -> String -- ^ package identifier or GhcPkgId
                  -> m [GhcPkgId]
findGhcPkgDepends menv wc pkgDbs pkgId = do
    mdeps <- findGhcPkgField menv wc pkgDbs pkgId "depends"
    case mdeps of
        Just !deps -> return (mapMaybe (parseGhcPkgId . T.encodeUtf8) (T.words deps))
        _ -> return []

unregisterGhcPkgId :: (MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m, MonadBaseControl IO m)
                    => EnvOverride
                    -> WhichCompiler
                    -> CompilerVersion
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
getCabalPkgVer :: (MonadThrow m, MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
               => EnvOverride -> WhichCompiler -> m Version
getCabalPkgVer menv wc =
    findGhcPkgVersion
        menv
        wc
        [] -- global DB
        cabalPackageName >>=
        maybe (throwM $ Couldn'tFindPkgId cabalPackageName) return

listGhcPkgDbs
    :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)
    => EnvOverride -> WhichCompiler -> [Path Abs Dir] -> m [PackageIdentifier]
listGhcPkgDbs menv wc pkgDbs = do
    result <-
        ghcPkg
            menv
            wc
            pkgDbs
            ["list", "--simple-output"]
    return $
        case result of
            Left{} -> []
            Right lbs -> mapMaybe parsePackageIdentifier (S8.words lbs)

-- | Get the value for GHC_PACKAGE_PATH
mkGhcPackagePath :: Bool -> Path Abs Dir -> Path Abs Dir -> Path Abs Dir -> Text
mkGhcPackagePath locals localdb deps globaldb =
  T.pack $ intercalate [searchPathSeparator] $ concat
    [ [toFilePathNoTrailingSlash localdb | locals]
    , [toFilePathNoTrailingSlash deps]
    , [toFilePathNoTrailingSlash globaldb]
    ]

-- TODO: dedupe with copy in Stack.Setup
toFilePathNoTrailingSlash :: Path loc Dir -> FilePath
toFilePathNoTrailingSlash = dropTrailingPathSeparator . toFilePath
