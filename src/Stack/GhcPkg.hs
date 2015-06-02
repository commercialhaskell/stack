{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Functions for the GHC package database.

module Stack.GhcPkg
  (getPackageVersionMapWithGlobalDb
  ,getPackageVersionsSet
  ,findGhcPkgId
  ,getGhcPkgIds
  ,getGlobalDB
  ,EnvOverride
  ,envHelper
  ,unregisterPackages)
  where

import           Control.Applicative
import           Control.Exception hiding (catch)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy as AttoLazy
import           Data.Bifunctor
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Data
import           Data.Either
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Set.Monad as Set
import           Data.Streaming.Process
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Path (Path, Abs, Dir, toFilePath, parent, parseAbsDir)
import           Path.IO
import           Prelude hiding (FilePath)
import           Stack.Types
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, canonicalizePath)
import           System.Process.Read

-- | A ghc-pkg exception.
data GhcPkgException
  = GetAllPackagesFail
  | GetUserDbPathFail
  | FindPackageIdFail PackageName ProcessExitedUnsuccessfully
  deriving (Typeable,Show)
instance Exception GhcPkgException

-- | Get the global package database
getGlobalDB :: (MonadIO m, MonadLogger m, MonadThrow m)
            => EnvOverride
            -> m (Path Abs Dir)
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
ghcPkg :: (MonadIO m, MonadLogger m)
       => EnvOverride
       -> [Path Abs Dir]
       -> [String]
       -> m (Either ProcessExitedUnsuccessfully S8.ByteString)
ghcPkg menv pkgDbs args = do
    $logDebug $ "Calling ghc-pkg with: " <> T.pack (show args')
    eres <- go
    case eres of
        Left _ -> do
            forM_ pkgDbs $ \db -> do
                let db' = toFilePath db
                exists <- liftIO $ doesDirectoryExist db'
                unless exists $ do
                    -- Creating the parent doesn't seem necessary, as ghc-pkg
                    -- seems to be sufficiently smart. But I don't feel like
                    -- finding out it isn't the hard way
                    liftIO $ createDirectoryIfMissing True $ toFilePath $ parent db
                    _ <- tryProcessStdout menv "ghc-pkg" ["init", db']
                    return ()
            go
        Right _ -> return eres
  where
    args' =
          "--no-user-package-db"
        : map (\x -> ("--package-db=" ++ toFilePath x)) pkgDbs
       ++ args
    go = tryProcessStdout menv "ghc-pkg" args'

-- | In the given databases, get a single version for all packages, chooses the
-- latest version of each package.
--
-- Package databases passed to this function override eachother in a
-- left-biased way when containing two packages of the same name.
getPackageVersionMapWithGlobalDb
    :: (MonadCatch m, MonadIO m, MonadThrow m, MonadLogger m)
    => EnvOverride
    -> Maybe MiniBuildPlan
    -> [Path Abs Dir] -- ^ package databases
    -> m (Map PackageName Version)
getPackageVersionMapWithGlobalDb menv mmbp pkgDbs = do
    gdb <- getGlobalDB menv
    allGlobals <-
        getPackageVersions
            menv
            [gdb]
            (const True)
            (M.unions . map (M.fromList . rights))
    $logDebug ("All globals: " <> T.pack (show allGlobals))
    globals <-
        case mmbp of
            Nothing ->
                return allGlobals
            Just mbp ->
                filtering gdb mbp allGlobals
    $logDebug ("Filtered globals: " <> T.pack (show globals))
    -- M.unions is left-biased.
    rest <-
        getPackageVersions
            menv
            pkgDbs
            (flip elem pkgDbs)
            (M.unions .
             map (M.fromList . rights))
    return (M.unions [rest,globals])
  where
    filtering gdb mbp allGlobals =
        foldM
            (\acc ident ->
                  let expunge =
                          foldr
                              M.delete
                              acc
                              (map
                                   packageIdentifierName
                                   (ident :
                                    getTransInclusiveRevDeps mbp (packageIdentifierName ident)))
                  in if versionMatches mbp ident
                         then do
                             hasProfiling <- packageHasProfiling menv [gdb] ident
                             $logDebug (packageIdentifierText ident <> " has profiling: " <> T.pack (show hasProfiling))
                             return (if hasProfiling
                                         then acc
                                         else expunge)
                         else return expunge)
            allGlobals
            (map fromTuple (M.toList allGlobals))

-- | Does this version match what's in the snapshot plan?
versionMatches :: MiniBuildPlan -> PackageIdentifier -> Bool
versionMatches mbp ident =
    M.member
        (packageIdentifierName ident)
        (mbpPackages mbp)

-- | Get the packages depended on by the given package.
getTransInclusiveRevDeps :: MiniBuildPlan -> PackageName -> [PackageIdentifier]
getTransInclusiveRevDeps mbp name =
    case M.lookup name (mbpPackages mbp) of
        Nothing -> []
        Just miniPkgInfo ->
            let revDeps = packageRevDeps mbp name
            in mapMaybe lookupPackageIdent revDeps <>
               concatMap (getTransInclusiveRevDeps mbp) revDeps
  where
    lookupPackageIdent depname =
        fmap
            (\miniPkgInfo -> PackageIdentifier depname (mpiVersion miniPkgInfo))
            (M.lookup depname (mbpPackages mbp))

-- | Get reverse dependencies of a package.
packageRevDeps :: MiniBuildPlan -> PackageName -> [PackageName]
packageRevDeps mbp name =
    mapMaybe
        (\(name,miniPkgInfo) ->
              if S.member name (mpiPackageDeps miniPkgInfo)
                  then Just (name)
                  else Nothing)
        (M.toList (mbpPackages mbp))

-- | Does the given package identifier from the given package db have
-- profiling libs built?
packageHasProfiling :: (MonadIO m, MonadThrow m, MonadLogger m)
                    => EnvOverride
                    -> [Path Abs Dir]
                    -> PackageIdentifier
                    -> m Bool
packageHasProfiling env dbs ident = do
    mlibDir <- findLibDir env dbs (packageIdentifierName ident)
    case mlibDir of
        Nothing ->
            return False
        Just dir -> do
            (_dirs,files) <- listDirectory dir
            return (any (isSuffixOf "_p.a" . toFilePath) files)

-- | In the given databases, get every version of every package.
getPackageVersionsSet :: (MonadCatch m, MonadIO m, MonadThrow m, MonadLogger m)
                      => EnvOverride
                      -> [Path Abs Dir]         -- ^ Package databases to enable.
                      -> (Path Abs Dir -> Bool) -- ^ Return only packages matching this database predicate.
                      -> m (Set PackageIdentifier)
getPackageVersionsSet menv pkgDbs predicate =
    getPackageVersions
        menv
        pkgDbs
        predicate
        (S.fromList .
         concatMap (map fromTuple . rights))

-- | In the given databases, broken packages according to the given predicate.
getBrokenPackages :: (MonadCatch m, MonadIO m, MonadThrow m, MonadLogger m)
                  => EnvOverride
                  -> [Path Abs Dir]         -- ^ Package databases to enable.
                  -> (Path Abs Dir -> Bool) -- ^ Return only packages matching this database predicate.
                  -> m (Set PackageIdentifier)
getBrokenPackages menv pkgDbs predicate =
    getPackageVersions
        menv
        pkgDbs
        predicate
        (S.fromList .
         concatMap (map fromTuple . lefts))

-- | In the given databases, get all available packages.
getPackageVersions :: (MonadCatch m, MonadIO m, MonadThrow m, MonadLogger m)
                   => EnvOverride
                   -> [Path Abs Dir]         -- ^ Package databases to enable.
                   -> (Path Abs Dir -> Bool) -- ^ Return only packages matching this database predicate.
                   -> ([[Either (PackageName, Version) (PackageName, Version)]] -> a)
                   -> m a
getPackageVersions menv pkgDbs predicate f = do
    result <-
        ghcPkg menv pkgDbs ["list"]
    case result of
        Left{} ->
            throw GetAllPackagesFail
        Right lbs ->
            case AttoLazy.parse
                     (packageVersionsParser predicate f)
                     (L.fromStrict lbs) of
                AttoLazy.Fail _ _ _ ->
                    throw GetAllPackagesFail
                AttoLazy.Done _ r ->
                    liftIO (evaluate r)

-- | Parser for ghc-pkg's list output.
packageVersionsParser :: (Path Abs Dir -> Bool)
                      -> ([[Either (PackageName, Version) (PackageName, Version)]] -> a)
                      -> Parser a
packageVersionsParser sectionPredicate f =
    fmap
        (f .
         mapMaybe
             (\(fp,pkgs) ->
                   do dir <- parseAbsDir fp
                      guard (sectionPredicate dir)
                      return pkgs))
        sections
  where
    sections =
        many
            (do fp <- heading
                pkgs <- many (pkg <* endOfLine)
                optional endOfLine
                return (fp, pkgs))
    heading =
        fmap
            (reverse .
             dropWhile (== ':') .
             dropWhile (== '\r') .
             reverse)
            (many1 (satisfy (not . (== '\n')))) <*
        endOfLine
    pkg = do
        space
        space
        space
        space
        fmap
            (bimap toTuple toTuple)
            ((Right <$> packageIdentifierParser) <|> -- normal packages
             (Right <$> ("(" *> packageIdentifierParser <* ")")) <|> -- hidden packages
             (Left <$> ("{" *> packageIdentifierParser <* "}"))) -- broken packages

-- | Get the id of the package e.g. @foo-0.0.0-9c293923c0685761dcff6f8c3ad8f8ec@.
findGhcPkgId :: (MonadIO m, MonadLogger m)
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

-- | Get the library directory of the package
-- e.g. @/opt/ghc/7.8.4/lib/ghc-7.8.4/base-4.7.0.2@.
findLibDir :: (MonadIO m, MonadLogger m)
           => EnvOverride
           -> [Path Abs Dir] -- ^ package databases
           -> PackageName
           -> m (Maybe (Path Abs Dir))
findLibDir menv pkgDbs name = do
    result <-
        ghcPkg menv pkgDbs ["describe", packageNameString name]
    case result of
        Left{} ->
            return Nothing
        Right lbs -> do
            let mpid =
                    fmap
                        T.unpack
                        (listToMaybe
                             (mapMaybe
                                  (fmap stripCR .
                                   T.stripPrefix "library-dirs: ")
                                  (map T.decodeUtf8 (S8.lines lbs))))
            case mpid of
                Just !p ->
                    return (parseAbsDir p)
                _ ->
                    return Nothing
  where
    stripCR t =
        fromMaybe t (T.stripSuffix "\r" t)

-- | Get all current package ids.
getGhcPkgIds :: (MonadIO m, MonadLogger m)
             => EnvOverride
             -> [Path Abs Dir] -- ^ package databases
             -> [PackageName]
             -> m (Map PackageName GhcPkgId)
getGhcPkgIds menv pkgDbs pkgs =
    collect pkgs >>= liftIO . evaluate
  where
    collect =
        liftM (M.fromList . catMaybes) .
        mapM getTuple
    getTuple name = do
        mpid <- findGhcPkgId menv pkgDbs name
        case mpid of
            Nothing ->
                return Nothing
            Just pid ->
                return (Just (name, pid))

-- | Unregister the given package(s) and any dependent packages which
-- become broken as a result.
unregisterPackages :: (MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m)
                   => EnvOverride
                   -> [Path Abs Dir]
                   -> (Path Abs Dir -> Bool)
                   -> Set PackageIdentifier
                   -> m ()
unregisterPackages menv pkgDbs predicate idents = do
    Set.mapM_ (unregisterPackage menv pkgDbs) idents
    broken <- getBrokenPackages menv pkgDbs predicate
    Set.mapM_ (unregisterPackage menv pkgDbs) broken

-- | Unregister the given package.
unregisterPackage :: (MonadIO m, MonadLogger m, MonadThrow m)
                  => EnvOverride -> [Path Abs Dir] -> PackageIdentifier -> m ()
unregisterPackage menv pkgDbs ident =
    do $logInfo (packageIdentifierText ident <> ": unregistering")
       liftM
           (const ())
           (ghcPkg menv pkgDbs ["unregister", "--force", packageIdentifierString ident] >>=
            either throwM return)
