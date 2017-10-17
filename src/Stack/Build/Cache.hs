{-# LANGUAGE NoImplicitPrelude #-}
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

import           Stack.Prelude
import           Crypto.Hash (hashWith, SHA256(..))
import           Control.Monad.Trans.Maybe
import qualified Data.ByteArray as Mem (convert)
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as S8
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Store as Store
import           Data.Store.VersionTagged
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Path
import           Path.IO
import           Stack.Constants.Config
import           Stack.Types.Build
import           Stack.Types.BuildPlan
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.GhcPkgId
import           Stack.Types.Package
import           Stack.Types.PackageIdentifier
import           Stack.Types.Version
import qualified System.FilePath as FP

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
markExeInstalled :: (MonadReader env m, HasEnvConfig env, MonadIO m, MonadThrow m)
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
    liftIO $ B.writeFile fp "Installed"

-- | Mark the given executable as not installed
markExeNotInstalled :: (MonadReader env m, HasEnvConfig env, MonadIO m, MonadThrow m)
                    => InstallLocation -> PackageIdentifier -> m ()
markExeNotInstalled loc ident = do
    dir <- exeInstalledDir loc
    ident' <- parseRelFile $ packageIdentifierString ident
    liftIO $ ignoringAbsence (removeFile $ dir </> ident')

-- | Try to read the dirtiness cache for the given package directory.
tryGetBuildCache :: (MonadUnliftIO m, MonadReader env m, MonadThrow m, MonadLogger m, HasEnvConfig env)
                 => Path Abs Dir -> m (Maybe (Map FilePath FileCacheInfo))
tryGetBuildCache dir = liftM (fmap buildCacheTimes) . $(versionedDecodeFile buildCacheVC) =<< buildCacheFile dir

-- | Try to read the dirtiness cache for the given package directory.
tryGetConfigCache :: (MonadUnliftIO m, MonadReader env m, MonadThrow m, HasEnvConfig env, MonadLogger m)
                  => Path Abs Dir -> m (Maybe ConfigCache)
tryGetConfigCache dir = $(versionedDecodeFile configCacheVC) =<< configCacheFile dir

-- | Try to read the mod time of the cabal file from the last build
tryGetCabalMod :: (MonadUnliftIO m, MonadReader env m, MonadThrow m, HasEnvConfig env, MonadLogger m)
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
deleteCaches :: (MonadIO m, MonadReader env m, HasEnvConfig env, MonadThrow m)
             => Path Abs Dir -> m ()
deleteCaches dir = do
    {- FIXME confirm that this is acceptable to remove
    bfp <- buildCacheFile dir
    removeFileIfExists bfp
    -}
    cfp <- configCacheFile dir
    liftIO $ ignoringAbsence (removeFile cfp)

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
tryGetFlagCache :: (MonadUnliftIO m, MonadThrow m, MonadReader env m, HasEnvConfig env, MonadLogger m)
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
checkTestSuccess :: (MonadUnliftIO m, MonadThrow m, MonadReader env m, HasEnvConfig env, MonadLogger m)
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
                     => PackageLocationIndex FilePath
                     -> ConfigureOpts
                     -> Set GhcPkgId -- ^ dependencies
                     -> m (Maybe (Path Abs File))
precompiledCacheFile loc copts installedPackageIDs = do
  ec <- view envConfigL

  compiler <- view actualCompilerVersionL >>= parseRelDir . compilerVersionString
  cabal <- view cabalVersionL >>= parseRelDir . versionString
  let mpkgRaw =
        -- The goal here is to come up with a string representing the
        -- package location which is unique. For archives and repos,
        -- we rely upon cryptographic hashes paired with
        -- subdirectories to identify this specific package version.
        case loc of
          PLIndex pir -> Just $ packageIdentifierRevisionString pir
          PLOther other -> case other of
            PLFilePath _ -> assert False Nothing -- no PLFilePaths should end up in a snapshot
            PLArchive a -> fmap
              (\h -> T.unpack (staticSHA256ToText h) ++ archiveSubdirs a)
              (archiveHash a)
            PLRepo r -> Just $ T.unpack (repoCommit r) ++ repoSubdirs r

  forM mpkgRaw $ \pkgRaw -> do
    pkg <-
      case parseRelDir pkgRaw of
        Just x -> return x
        Nothing -> parseRelDir
                 $ T.unpack
                 $ TE.decodeUtf8
                 $ B64URL.encode
                 $ TE.encodeUtf8
                 $ T.pack pkgRaw
    platformRelDir <- platformGhcRelDir

    -- In Cabal versions 1.22 and later, the configure options contain the
    -- installed package IDs, which is what we need for a unique hash.
    -- Unfortunately, earlier Cabals don't have the information, so we must
    -- supplement it with the installed package IDs directly.
    -- See issue: https://github.com/commercialhaskell/stack/issues/1103
    let input = (coNoDirs copts, installedPackageIDs)
    hashPath <- parseRelFile $ S8.unpack $ B64URL.encode
              $ Mem.convert $ hashWith SHA256 $ Store.encode input

    return $ view stackRootL ec
         </> $(mkRelDir "precompiled")
         </> platformRelDir
         </> compiler
         </> cabal
         </> pkg
         </> hashPath

-- | Write out information about a newly built package
writePrecompiledCache :: (MonadThrow m, MonadReader env m, HasEnvConfig env, MonadIO m, MonadLogger m)
                      => BaseConfigOpts
                      -> PackageLocationIndex FilePath
                      -> ConfigureOpts
                      -> Set GhcPkgId -- ^ dependencies
                      -> Installed -- ^ library
                      -> Set Text -- ^ executables
                      -> m ()
writePrecompiledCache baseConfigOpts loc copts depIDs mghcPkgId exes = do
  mfile <- precompiledCacheFile loc copts depIDs
  forM_ mfile $ \file -> do
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
readPrecompiledCache :: forall env. HasEnvConfig env
                     => PackageLocationIndex FilePath -- ^ target package
                     -> ConfigureOpts
                     -> Set GhcPkgId -- ^ dependencies
                     -> RIO env (Maybe PrecompiledCache)
readPrecompiledCache loc copts depIDs = runMaybeT $
    MaybeT (precompiledCacheFile loc copts depIDs) >>=
    MaybeT . $(versionedDecodeFile precompiledCacheVC) >>=
    lift . mkAbs
  where
    -- Since commit ed9ccc08f327bad68dd2d09a1851ce0d055c0422,
    -- pcLibrary paths are stored as relative to the stack
    -- root. Therefore, we need to prepend the stack root when
    -- checking that the file exists. For the older cached paths, the
    -- file will contain an absolute path, which will make `stackRoot
    -- </>` a no-op.
    mkAbs :: PrecompiledCache -> RIO env PrecompiledCache
    mkAbs pc0 = do
      stackRoot <- view stackRootL
      let mkAbs' = (toFilePath stackRoot FP.</>)
      return PrecompiledCache
        { pcLibrary = mkAbs' <$> pcLibrary pc0
        , pcExes = mkAbs' <$> pcExes pc0
        }
