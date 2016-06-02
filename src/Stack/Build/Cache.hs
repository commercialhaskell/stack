{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-- | Cache information about previous builds
module Stack.Build.Cache
    ( tryGetBuildCache
    , tryGetConfigCache
    , tryGetCabalMod
    , getInstalledExes
    , tryGetFlagCache
    , deleteCaches
    , markExeInstalled
    , markExeNotInstalled
    , writeFlagCache
    , writeBuildCache
    , writeConfigCache
    , writeCabalMod
    , setTestSuccess
    , unsetTestSuccess
    , checkTestSuccess
    , writePrecompiledCache
    , readPrecompiledCache
    -- Exported for testing
    , BuildCache(..)
    ) where

import           Control.DeepSeq (NFData)
import           Control.Exception.Enclosed (handleIO)
import           Control.Monad (liftM)
import           Control.Monad.Catch (MonadThrow, MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Logger (MonadLogger, logDebug)
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as S8
import           Data.Map (Map)
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Store (Store)
import qualified Data.Store as Store
import           Data.Store.TypeHash (HasTypeHash, mkManyHasTypeHash)
import           Data.Store.VersionTagged
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (forM)
import           GHC.Generics (Generic)
import           Path
import           Path.IO
import           Stack.Constants
import           Stack.Types

-- | Directory containing files to mark an executable as installed
exeInstalledDir :: (MonadReader env m, HasEnvConfig env, MonadThrow m)
                => InstallLocation -> m (Path Abs Dir)
exeInstalledDir Snap = (</> $(mkRelDir "installed-packages")) `liftM` installationRootDeps
exeInstalledDir Local = (</> $(mkRelDir "installed-packages")) `liftM` installationRootLocal

-- | Get all of the installed executables
getInstalledExes :: (MonadReader env m, HasEnvConfig env, MonadIO m, MonadThrow m)
                 => InstallLocation -> m [PackageIdentifier]
getInstalledExes loc = do
    dir <- exeInstalledDir loc
    (_, files) <- liftIO $ handleIO (const $ return ([], [])) $ listDir dir
    return $ mapMaybe (parsePackageIdentifierFromString . toFilePath . filename) files

-- | Mark the given executable as installed
markExeInstalled :: (MonadReader env m, HasEnvConfig env, MonadIO m, MonadThrow m)
                 => InstallLocation -> PackageIdentifier -> m ()
markExeInstalled loc ident = do
    dir <- exeInstalledDir loc
    ensureDir dir
    ident' <- parseRelFile $ packageIdentifierString ident
    let fp = toFilePath $ dir </> ident'
    -- TODO consideration for the future: list all of the executables
    -- installed, and invalidate this file in getInstalledExes if they no
    -- longer exist
    liftIO $ writeFile fp "Installed"

-- | Mark the given executable as not installed
markExeNotInstalled :: (MonadReader env m, HasEnvConfig env, MonadIO m, MonadCatch m)
                    => InstallLocation -> PackageIdentifier -> m ()
markExeNotInstalled loc ident = do
    dir <- exeInstalledDir loc
    ident' <- parseRelFile $ packageIdentifierString ident
    ignoringAbsence (removeFile $ dir </> ident')

-- | Stored on disk to know whether the flags have changed or any
-- files have changed.
data BuildCache = BuildCache
    { buildCacheTimes :: !(Map FilePath FileCacheInfo)
      -- ^ Modification times of files.
    }
    deriving (Generic, Eq, Show)
instance Store BuildCache
instance NFData BuildCache

-- | Try to read the dirtiness cache for the given package directory.
tryGetBuildCache :: (MonadIO m, MonadReader env m, HasConfig env, MonadThrow m, MonadLogger m, HasEnvConfig env, MonadBaseControl IO m)
                 => Path Abs Dir -> m (Maybe (Map FilePath FileCacheInfo))
tryGetBuildCache = liftM (fmap buildCacheTimes) . tryGetCache buildCacheFile

-- | Try to read the dirtiness cache for the given package directory.
tryGetConfigCache :: (MonadIO m, MonadReader env m, MonadThrow m, HasEnvConfig env, MonadBaseControl IO m, MonadLogger m)
                  => Path Abs Dir -> m (Maybe ConfigCache)
tryGetConfigCache = tryGetCache configCacheFile

-- | Try to read the mod time of the cabal file from the last build
tryGetCabalMod :: (MonadIO m, MonadReader env m, MonadThrow m, HasEnvConfig env, MonadBaseControl IO m, MonadLogger m)
               => Path Abs Dir -> m (Maybe ModTime)
tryGetCabalMod = tryGetCache configCabalMod

-- | Try to load a cache.
tryGetCache :: (MonadIO m, Store a, HasTypeHash a, MonadBaseControl IO m, MonadLogger m)
            => (Path Abs Dir -> m (Path Abs File))
            -> Path Abs Dir
            -> m (Maybe a)
tryGetCache get' dir = do
    fp <- get' dir
    decodeFileMaybe fp

-- | Write the dirtiness cache for this package's files.
writeBuildCache :: (MonadIO m, MonadReader env m, MonadThrow m, HasEnvConfig env, MonadLogger m)
                => Path Abs Dir -> Map FilePath FileCacheInfo -> m ()
writeBuildCache dir times =
    writeCache
        dir
        buildCacheFile
        BuildCache
         { buildCacheTimes = times
         }

-- | Write the dirtiness cache for this package's configuration.
writeConfigCache :: (MonadIO m, MonadReader env m, MonadThrow m, HasEnvConfig env, MonadLogger m)
                => Path Abs Dir
                -> ConfigCache
                -> m ()
writeConfigCache dir = writeCache dir configCacheFile

-- | See 'tryGetCabalMod'
writeCabalMod :: (MonadIO m, MonadReader env m, MonadThrow m, HasEnvConfig env, MonadLogger m)
              => Path Abs Dir
              -> ModTime
              -> m ()
writeCabalMod dir = writeCache dir configCabalMod

-- | Delete the caches for the project.
deleteCaches :: (MonadIO m, MonadReader env m, MonadCatch m, HasEnvConfig env)
             => Path Abs Dir -> m ()
deleteCaches dir = do
    {- FIXME confirm that this is acceptable to remove
    bfp <- buildCacheFile dir
    removeFileIfExists bfp
    -}
    cfp <- configCacheFile dir
    ignoringAbsence (removeFile cfp)

-- | Write to a cache.
writeCache :: (Store a, NFData a, HasTypeHash a, Eq a, MonadIO m, MonadLogger m)
           => Path Abs Dir
           -> (Path Abs Dir -> m (Path Abs File))
           -> a
           -> m ()
writeCache dir get' content = do
    fp <- get' dir
    taggedEncodeFile fp content

flagCacheFile :: (MonadIO m, MonadThrow m, MonadReader env m, HasEnvConfig env)
              => Installed
              -> m (Path Abs File)
flagCacheFile installed = do
    rel <- parseRelFile $
        case installed of
            Library _ gid -> ghcPkgIdString gid
            Executable ident -> packageIdentifierString ident
    dir <- flagCacheLocal
    return $ dir </> rel

-- | Loads the flag cache for the given installed extra-deps
tryGetFlagCache :: (MonadIO m, MonadThrow m, MonadReader env m, HasEnvConfig env, MonadBaseControl IO m, MonadLogger m)
                => Installed
                -> m (Maybe ConfigCache)
tryGetFlagCache gid = do
    fp <- flagCacheFile gid
    decodeFileMaybe fp

writeFlagCache :: (MonadIO m, MonadReader env m, HasEnvConfig env, MonadThrow m, MonadLogger m, MonadBaseControl IO m)
               => Installed
               -> ConfigCache
               -> m ()
writeFlagCache gid cache = do
    file <- flagCacheFile gid
    ensureDir (parent file)
    taggedEncodeFile file cache

-- | Mark a test suite as having succeeded
setTestSuccess :: (MonadIO m, MonadThrow m, MonadReader env m, HasEnvConfig env, MonadLogger m)
               => Path Abs Dir
               -> m ()
setTestSuccess dir =
    writeCache
        dir
        testSuccessFile
        True

-- | Mark a test suite as not having succeeded
unsetTestSuccess :: (MonadIO m, MonadThrow m, MonadReader env m, HasEnvConfig env, MonadLogger m)
                 => Path Abs Dir
                 -> m ()
unsetTestSuccess dir =
    writeCache
        dir
        testSuccessFile
        False

-- | Check if the test suite already passed
checkTestSuccess :: (MonadIO m, MonadThrow m, MonadReader env m, HasEnvConfig env, MonadBaseControl IO m, MonadLogger m)
                 => Path Abs Dir
                 -> m Bool
checkTestSuccess dir =
    liftM
        (fromMaybe False)
        (tryGetCache testSuccessFile dir)

--------------------------------------
-- Precompiled Cache
--
-- Idea is simple: cache information about packages built in other snapshots,
-- and then for identical matches (same flags, config options, dependencies)
-- just copy over the executables and reregister the libraries.
--------------------------------------

-- | The file containing information on the given package/configuration
-- combination. The filename contains a hash of the non-directory configure
-- options for quick lookup if there's a match.
--
-- It also returns an action yielding the location of the precompiled
-- path based on the old binary encoding.
--
-- We only pay attention to non-directory options. We don't want to avoid a
-- cache hit just because it was installed in a different directory.
precompiledCacheFile :: (MonadThrow m, MonadReader env m, HasEnvConfig env, MonadLogger m)
                     => PackageIdentifier
                     -> ConfigureOpts
                     -> Set GhcPkgId -- ^ dependencies
                     -> m (Path Abs File)
precompiledCacheFile pkgident copts installedPackageIDs = do
    ec <- asks getEnvConfig

    compiler <- parseRelDir $ compilerVersionString $ envConfigCompilerVersion ec
    cabal <- parseRelDir $ versionString $ envConfigCabalVersion ec
    pkg <- parseRelDir $ packageIdentifierString pkgident
    platformRelDir <- platformGhcRelDir

    let input = (coNoDirs copts, installedPackageIDs)
    let hashToPath hash = do
            hashPath <- parseRelFile $ S8.unpack hash
            return $ getStackRoot ec
                 </> $(mkRelDir "precompiled")
                 </> platformRelDir
                 </> compiler
                 </> cabal
                 </> pkg
                 </> hashPath

    $logDebug $ "Precompiled cache input = " <> T.pack (show input)
    hashToPath $ B64URL.encode $ SHA256.hash $ Store.encode input

-- | Write out information about a newly built package
writePrecompiledCache :: (MonadThrow m, MonadReader env m, HasEnvConfig env, MonadIO m, MonadLogger m)
                      => BaseConfigOpts
                      -> PackageIdentifier
                      -> ConfigureOpts
                      -> Set GhcPkgId -- ^ dependencies
                      -> Installed -- ^ library
                      -> Set Text -- ^ executables
                      -> m ()
writePrecompiledCache baseConfigOpts pkgident copts depIDs mghcPkgId exes = do
    file <- precompiledCacheFile pkgident copts depIDs
    ensureDir (parent file)
    mlibpath <-
        case mghcPkgId of
            Executable _ -> return Nothing
            Library _ ipid -> liftM Just $ do
                ipid' <- parseRelFile $ ghcPkgIdString ipid ++ ".conf"
                return $ toFilePath $ bcoSnapDB baseConfigOpts </> ipid'
    exes' <- forM (Set.toList exes) $ \exe -> do
        name <- parseRelFile $ T.unpack exe
        return $ toFilePath $ bcoSnapInstallRoot baseConfigOpts </> bindirSuffix </> name
    taggedEncodeFile file PrecompiledCache
        { pcLibrary = mlibpath
        , pcExes = exes'
        }

-- | Check the cache for a precompiled package matching the given
-- configuration.
readPrecompiledCache :: (MonadThrow m, MonadReader env m, HasEnvConfig env, MonadIO m, MonadLogger m, MonadBaseControl IO m)
                     => PackageIdentifier -- ^ target package
                     -> ConfigureOpts
                     -> Set GhcPkgId -- ^ dependencies
                     -> m (Maybe PrecompiledCache)
readPrecompiledCache pkgident copts depIDs = do
    file <- precompiledCacheFile pkgident copts depIDs
    decodeFileMaybe file

$(mkManyHasTypeHash
    [ [t| BuildCache |]
    -- TODO: put this orphan elsewhere? Not sure if we want tons of
    -- instances of HasTypeHash or not.
    , [t| Bool |]
    ])
