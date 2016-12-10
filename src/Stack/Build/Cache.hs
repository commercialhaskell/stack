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

import           Control.Applicative
import           Control.DeepSeq (NFData)
import           Control.Exception.Safe (handleIO, tryAnyDeep)
import           Control.Monad (liftM)
import           Control.Monad.Catch (MonadThrow, MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Logger (MonadLogger, logDebug)
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Crypto.Hash.SHA256 as SHA256
import           Data.Binary (Binary (..))
import qualified Data.Binary as Binary
import           Data.Binary.Tagged (HasStructuralInfo, HasSemanticVersion)
import qualified Data.Binary.Tagged as BinaryTagged
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (forM_)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Store as Store
import           Data.Store.VersionTagged
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (forM)
import           Path
import           Path.IO
import           Prelude -- Fix redundant import warnings
import           Stack.Constants
import           Stack.Types.Build
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.GhcPkgId
import           Stack.Types.Package
import           Stack.Types.PackageIdentifier
import           Stack.Types.Version
import qualified System.FilePath as FilePath

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
    return $
        concat $
        M.elems $
        -- If there are multiple install records (from a stack version
        -- before https://github.com/commercialhaskell/stack/issues/2373
        -- was fixed), then we don't know which is correct - ignore them.
        M.fromListWith (\_ _ -> []) $
        map (\x -> (packageIdentifierName x, [x])) $
        mapMaybe (parsePackageIdentifierFromString . toFilePath . filename) files

-- | Mark the given executable as installed
markExeInstalled :: (MonadReader env m, HasEnvConfig env, MonadIO m, MonadCatch m)
                 => InstallLocation -> PackageIdentifier -> m ()
markExeInstalled loc ident = do
    dir <- exeInstalledDir loc
    ensureDir dir
    ident' <- parseRelFile $ packageIdentifierString ident
    let fp = toFilePath $ dir </> ident'
    -- Remove old install records for this package.
    -- TODO: This is a bit in-efficient. Put all this metadata into one file?
    installed <- getInstalledExes loc
    forM_ (filter (\x -> packageIdentifierName ident == packageIdentifierName x) installed)
          (markExeNotInstalled loc)
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

-- | Try to read the dirtiness cache for the given package directory.
tryGetBuildCache :: (MonadIO m, MonadReader env m, MonadThrow m, MonadLogger m, HasEnvConfig env, MonadBaseControl IO m)
                 => Path Abs Dir -> m (Maybe (Map FilePath FileCacheInfo))
tryGetBuildCache dir = liftM (fmap buildCacheTimes) . $(versionedDecodeFile buildCacheVC) =<< buildCacheFile dir

-- | Try to read the dirtiness cache for the given package directory.
tryGetConfigCache :: (MonadIO m, MonadReader env m, MonadThrow m, HasEnvConfig env, MonadBaseControl IO m, MonadLogger m)
                  => Path Abs Dir -> m (Maybe ConfigCache)
tryGetConfigCache dir = $(versionedDecodeFile configCacheVC) =<< configCacheFile dir

-- | Try to read the mod time of the cabal file from the last build
tryGetCabalMod :: (MonadIO m, MonadReader env m, MonadThrow m, HasEnvConfig env, MonadBaseControl IO m, MonadLogger m)
               => Path Abs Dir -> m (Maybe ModTime)
tryGetCabalMod dir = $(versionedDecodeFile modTimeVC) =<< configCabalMod dir

-- | Write the dirtiness cache for this package's files.
writeBuildCache :: (MonadIO m, MonadReader env m, MonadThrow m, HasEnvConfig env, MonadLogger m)
                => Path Abs Dir -> Map FilePath FileCacheInfo -> m ()
writeBuildCache dir times = do
    fp <- buildCacheFile dir
    $(versionedEncodeFile buildCacheVC) fp BuildCache
        { buildCacheTimes = times
        }

-- | Write the dirtiness cache for this package's configuration.
writeConfigCache :: (MonadIO m, MonadReader env m, MonadThrow m, HasEnvConfig env, MonadLogger m)
                => Path Abs Dir
                -> ConfigCache
                -> m ()
writeConfigCache dir x = do
    fp <- configCacheFile dir
    $(versionedEncodeFile configCacheVC) fp x

-- | See 'tryGetCabalMod'
writeCabalMod :: (MonadIO m, MonadReader env m, MonadThrow m, HasEnvConfig env, MonadLogger m)
              => Path Abs Dir
              -> ModTime
              -> m ()
writeCabalMod dir x = do
    fp <- configCabalMod dir
    $(versionedEncodeFile modTimeVC) fp x

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
    $(versionedDecodeFile configCacheVC) fp

writeFlagCache :: (MonadIO m, MonadReader env m, HasEnvConfig env, MonadThrow m, MonadLogger m)
               => Installed
               -> ConfigCache
               -> m ()
writeFlagCache gid cache = do
    file <- flagCacheFile gid
    ensureDir (parent file)
    $(versionedEncodeFile configCacheVC) file cache

-- | Mark a test suite as having succeeded
setTestSuccess :: (MonadIO m, MonadThrow m, MonadReader env m, HasEnvConfig env, MonadLogger m)
               => Path Abs Dir
               -> m ()
setTestSuccess dir = do
    fp <- testSuccessFile dir
    $(versionedEncodeFile testSuccessVC) fp True

-- | Mark a test suite as not having succeeded
unsetTestSuccess :: (MonadIO m, MonadThrow m, MonadReader env m, HasEnvConfig env, MonadLogger m)
                 => Path Abs Dir
                 -> m ()
unsetTestSuccess dir = do
    fp <- testSuccessFile dir
    $(versionedEncodeFile testSuccessVC) fp False

-- | Check if the test suite already passed
checkTestSuccess :: (MonadIO m, MonadThrow m, MonadReader env m, HasEnvConfig env, MonadBaseControl IO m, MonadLogger m)
                 => Path Abs Dir
                 -> m Bool
checkTestSuccess dir =
    liftM
        (fromMaybe False)
        ($(versionedDecodeFile testSuccessVC) =<< testSuccessFile dir)

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
                     -> m (Path Abs File, m (Path Abs File))
precompiledCacheFile pkgident copts installedPackageIDs = do
    ec <- view envConfigL

    compiler <- view actualCompilerVersionL >>= parseRelDir . compilerVersionString
    cabal <- view cabalVersionL >>= parseRelDir . versionString
    pkg <- parseRelDir $ packageIdentifierString pkgident
    platformRelDir <- platformGhcRelDir

    let input = (coNoDirs copts, installedPackageIDs)

    -- In Cabal versions 1.22 and later, the configure options contain the
    -- installed package IDs, which is what we need for a unique hash.
    -- Unfortunately, earlier Cabals don't have the information, so we must
    -- supplement it with the installed package IDs directly.
    -- See issue: https://github.com/commercialhaskell/stack/issues/1103
    let oldHash = B16.encode $ SHA256.hash $ LBS.toStrict $
            if view cabalVersionL ec >= $(mkVersion "1.22")
                then Binary.encode (coNoDirs copts)
                else Binary.encode input
        hashToPath hash = do
            hashPath <- parseRelFile $ S8.unpack hash
            return $ view stackRootL ec
                 </> $(mkRelDir "precompiled")
                 </> platformRelDir
                 </> compiler
                 </> cabal
                 </> pkg
                 </> hashPath

    $logDebug $ "Precompiled cache input = " <> T.pack (show input)
    newPath <- hashToPath $ B64URL.encode $ SHA256.hash $ Store.encode input
    return (newPath, hashToPath oldHash)

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
    (file, _) <- precompiledCacheFile pkgident copts depIDs
    ensureDir (parent file)
    ec <- view envConfigL
    let stackRootRelative = makeRelative (view stackRootL ec)
    mlibpath <-
        case mghcPkgId of
            Executable _ -> return Nothing
            Library _ ipid -> liftM Just $ do
                ipid' <- parseRelFile $ ghcPkgIdString ipid ++ ".conf"
                relPath <- stackRootRelative $ bcoSnapDB baseConfigOpts </> ipid'
                return $ toFilePath relPath
    exes' <- forM (Set.toList exes) $ \exe -> do
        name <- parseRelFile $ T.unpack exe
        relPath <- stackRootRelative $ bcoSnapInstallRoot baseConfigOpts </> bindirSuffix </> name
        return $ toFilePath relPath
    $(versionedEncodeFile precompiledCacheVC) file PrecompiledCache
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
    ec <- view envConfigL
    let toAbsPath path = do
          if FilePath.isAbsolute path
              then path -- Only older version store absolute path
              else toFilePath (view stackRootL ec) FilePath.</> path
    let toAbsPC pc =
            PrecompiledCache
                  { pcLibrary = fmap toAbsPath (pcLibrary pc)
                  , pcExes = map toAbsPath (pcExes pc)
                  }

    (file, getOldFile) <- precompiledCacheFile pkgident copts depIDs
    mres <- $(versionedDecodeFile precompiledCacheVC) file
    case mres of
        Just res -> return (Just $ toAbsPC res)
        Nothing -> do
            -- Fallback on trying the old binary format.
            oldFile <- getOldFile
            mpc <- fmap toAbsPC <$> binaryDecodeFileOrFailDeep oldFile
            -- Write out file in new format. Keep old file around for
            -- the benefit of older stack versions.
            forM_ mpc ($(versionedEncodeFile precompiledCacheVC) file)
            return mpc

-- | Ensure that there are no lurking exceptions deep inside the parsed
-- value... because that happens unfortunately. See
-- https://github.com/commercialhaskell/stack/issues/554
binaryDecodeFileOrFailDeep :: (BinarySchema a, MonadIO m)
                           => Path loc File
                           -> m (Maybe a)
binaryDecodeFileOrFailDeep fp = liftIO $ fmap (either (const Nothing) id) $ tryAnyDeep $ do
    eres <- BinaryTagged.taggedDecodeFileOrFail (toFilePath fp)
    case eres of
        Left _ -> return Nothing
        Right x -> return (Just x)

type BinarySchema a = (Binary a, NFData a, HasStructuralInfo a, HasSemanticVersion a)
