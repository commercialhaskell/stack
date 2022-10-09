{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Cache information about previous builds
module Stack.Build.Cache
    ( tryGetBuildCache
    , tryGetConfigCache
    , tryGetCabalMod
    , tryGetSetupConfigMod
    , tryGetPackageProjectRoot
    , getInstalledExes
    , tryGetFlagCache
    , deleteCaches
    , markExeInstalled
    , markExeNotInstalled
    , writeFlagCache
    , writeBuildCache
    , writeConfigCache
    , writeCabalMod
    , writeSetupConfigMod
    , writePackageProjectRoot
    , TestStatus (..)
    , setTestStatus
    , getTestStatus
    , writePrecompiledCache
    , readPrecompiledCache
    -- Exported for testing
    , BuildCache(..)
    ) where

import           Stack.Prelude
import           Crypto.Hash (hashWith, SHA256(..))
import qualified Data.ByteArray as Mem (convert)
import           Data.ByteString.Builder (byteString)
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import           Foreign.C.Types (CTime)
import           Path
import           Path.IO
import           Stack.Constants
import           Stack.Constants.Config
import           Stack.Storage.Project
import           Stack.Storage.User
import           Stack.Types.Build
import           Stack.Types.Cache
import           Stack.Types.Config
import           Stack.Types.GhcPkgId
import           Stack.Types.NamedComponent
import           Stack.Types.SourceMap (smRelDir)
import           System.PosixCompat.Files (modificationTime, getFileStatus, setFileTimes)

-- | Directory containing files to mark an executable as installed
exeInstalledDir :: (HasEnvConfig env)
                => InstallLocation -> RIO env (Path Abs Dir)
exeInstalledDir Snap = (</> relDirInstalledPackages) `liftM` installationRootDeps
exeInstalledDir Local = (</> relDirInstalledPackages) `liftM` installationRootLocal

-- | Get all of the installed executables
getInstalledExes :: (HasEnvConfig env)
                 => InstallLocation -> RIO env [PackageIdentifier]
getInstalledExes loc = do
    dir <- exeInstalledDir loc
    (_, files) <- liftIO $ handleIO (const $ pure ([], [])) $ listDir dir
    pure $
        concat $
        M.elems $
        -- If there are multiple install records (from a Stack version
        -- before https://github.com/commercialhaskell/stack/issues/2373
        -- was fixed), then we don't know which is correct - ignore them.
        M.fromListWith (\_ _ -> []) $
        map (\x -> (pkgName x, [x])) $
        mapMaybe (parsePackageIdentifier . toFilePath . filename) files

-- | Mark the given executable as installed
markExeInstalled :: (HasEnvConfig env)
                 => InstallLocation -> PackageIdentifier -> RIO env ()
markExeInstalled loc ident = do
    dir <- exeInstalledDir loc
    ensureDir dir
    ident' <- parseRelFile $ packageIdentifierString ident
    let fp = dir </> ident'
    -- Remove old install records for this package.
    -- TODO: This is a bit in-efficient. Put all this metadata into one file?
    installed <- getInstalledExes loc
    forM_ (filter (\x -> pkgName ident == pkgName x) installed)
          (markExeNotInstalled loc)
    -- TODO consideration for the future: list all of the executables
    -- installed, and invalidate this file in getInstalledExes if they no
    -- longer exist
    writeBinaryFileAtomic fp "Installed"

-- | Mark the given executable as not installed
markExeNotInstalled :: (HasEnvConfig env)
                    => InstallLocation -> PackageIdentifier -> RIO env ()
markExeNotInstalled loc ident = do
    dir <- exeInstalledDir loc
    ident' <- parseRelFile $ packageIdentifierString ident
    liftIO $ ignoringAbsence (removeFile $ dir </> ident')

buildCacheFile :: (HasEnvConfig env, MonadReader env m, MonadThrow m)
               => Path Abs Dir
               -> NamedComponent
               -> m (Path Abs File)
buildCacheFile dir component = do
    cachesDir <- buildCachesDir dir
    smh <- view $ envConfigL.to envConfigSourceMapHash
    smDirName <- smRelDir smh
    let nonLibComponent prefix name = prefix <> "-" <> T.unpack name
    cacheFileName <- parseRelFile $ case component of
        CLib -> "lib"
        CInternalLib name -> nonLibComponent "internal-lib" name
        CExe name -> nonLibComponent "exe" name
        CTest name -> nonLibComponent "test" name
        CBench name -> nonLibComponent "bench" name
    pure $ cachesDir </> smDirName </> cacheFileName

-- | Try to read the dirtiness cache for the given package directory.
tryGetBuildCache :: HasEnvConfig env
                 => Path Abs Dir
                 -> NamedComponent
                 -> RIO env (Maybe (Map FilePath FileCacheInfo))
tryGetBuildCache dir component = do
  fp <- buildCacheFile dir component
  ensureDir $ parent fp
  either (const Nothing) (Just . buildCacheTimes) <$>
    liftIO (tryAny (Yaml.decodeFileThrow (toFilePath fp)))

-- | Try to read the dirtiness cache for the given package directory.
tryGetConfigCache :: HasEnvConfig env
                  => Path Abs Dir -> RIO env (Maybe ConfigCache)
tryGetConfigCache dir =
    loadConfigCache $ configCacheKey dir ConfigCacheTypeConfig

-- | Try to read the mod time of the Cabal file from the last build
tryGetCabalMod :: HasEnvConfig env
               => Path Abs Dir -> RIO env (Maybe CTime)
tryGetCabalMod dir = do
  fp <- toFilePath <$> configCabalMod dir
  tryGetFileMod fp

-- | Try to read the mod time of setup-config file from the last build
tryGetSetupConfigMod :: HasEnvConfig env
               => Path Abs Dir -> RIO env (Maybe CTime)
tryGetSetupConfigMod dir = do
  fp <- toFilePath <$> configSetupConfigMod dir
  tryGetFileMod fp

tryGetFileMod :: MonadIO m => FilePath -> m (Maybe CTime)
tryGetFileMod fp =
  liftIO $ either (const Nothing) (Just . modificationTime) <$>
      tryIO (getFileStatus fp)

-- | Try to read the project root from the last build of a package
tryGetPackageProjectRoot :: HasEnvConfig env
               => Path Abs Dir -> RIO env (Maybe ByteString)
tryGetPackageProjectRoot dir = do
  fp <- toFilePath <$> configPackageProjectRoot dir
  tryReadFileBinary fp

tryReadFileBinary :: MonadIO m => FilePath -> m (Maybe ByteString)
tryReadFileBinary fp =
  liftIO $ either (const Nothing) Just <$>
      tryIO (readFileBinary fp)

-- | Write the dirtiness cache for this package's files.
writeBuildCache :: HasEnvConfig env
                => Path Abs Dir
                -> NamedComponent
                -> Map FilePath FileCacheInfo -> RIO env ()
writeBuildCache dir component times = do
    fp <- toFilePath <$> buildCacheFile dir component
    liftIO $ Yaml.encodeFile fp BuildCache
        { buildCacheTimes = times
        }

-- | Write the dirtiness cache for this package's configuration.
writeConfigCache :: HasEnvConfig env
                => Path Abs Dir
                -> ConfigCache
                -> RIO env ()
writeConfigCache dir =
    saveConfigCache (configCacheKey dir ConfigCacheTypeConfig)

-- | See 'tryGetCabalMod'
writeCabalMod :: HasEnvConfig env
              => Path Abs Dir
              -> CTime
              -> RIO env ()
writeCabalMod dir x = do
    fp <- configCabalMod dir
    writeBinaryFileAtomic fp "Just used for its modification time"
    liftIO $ setFileTimes (toFilePath fp) x x

-- | See 'tryGetSetupConfigMod'
writeSetupConfigMod
  :: HasEnvConfig env
  => Path Abs Dir
  -> Maybe CTime
  -> RIO env ()
writeSetupConfigMod dir Nothing = do
    fp <- configSetupConfigMod dir
    ignoringAbsence $ removeFile fp
writeSetupConfigMod dir (Just x) = do
    fp <- configSetupConfigMod dir
    writeBinaryFileAtomic fp "Just used for its modification time"
    liftIO $ setFileTimes (toFilePath fp) x x

-- | See 'tryGetPackageProjectRoot'
writePackageProjectRoot
  :: HasEnvConfig env
  => Path Abs Dir
  -> ByteString
  -> RIO env ()
writePackageProjectRoot dir projectRoot = do
    fp <- configPackageProjectRoot dir
    writeBinaryFileAtomic fp (byteString projectRoot)

-- | Delete the caches for the project.
deleteCaches :: HasEnvConfig env => Path Abs Dir -> RIO env ()
deleteCaches dir
    {- FIXME confirm that this is acceptable to remove
    bfp <- buildCacheFile dir
    removeFileIfExists bfp
    -}
 = deactiveConfigCache $ configCacheKey dir ConfigCacheTypeConfig

flagCacheKey :: (HasEnvConfig env) => Installed -> RIO env ConfigCacheKey
flagCacheKey installed = do
    installationRoot <- installationRootLocal
    case installed of
        Library _ gid _ ->
            pure $
            configCacheKey installationRoot (ConfigCacheTypeFlagLibrary gid)
        Executable ident ->
            pure $
            configCacheKey
                installationRoot
                (ConfigCacheTypeFlagExecutable ident)

-- | Loads the flag cache for the given installed extra-deps
tryGetFlagCache :: HasEnvConfig env
                => Installed
                -> RIO env (Maybe ConfigCache)
tryGetFlagCache gid = do
    key <- flagCacheKey gid
    loadConfigCache key

writeFlagCache :: HasEnvConfig env
               => Installed
               -> ConfigCache
               -> RIO env ()
writeFlagCache gid cache = do
    key <- flagCacheKey gid
    saveConfigCache key cache

successBS, failureBS, unknownBS :: IsString s => s
successBS = "success"
failureBS = "failure"
unknownBS = "unknown"

-- | Status of a test suite
data TestStatus = TSSuccess | TSFailure | TSUnknown

-- | Mark test suite status
setTestStatus :: HasEnvConfig env
              => Path Abs Dir
              -> TestStatus
              -> RIO env ()
setTestStatus dir status = do
    fp <- testSuccessFile dir
    writeBinaryFileAtomic fp $
      case status of
        TSSuccess -> successBS
        TSFailure -> failureBS
        TSUnknown -> unknownBS

-- | Check if the test suite already passed
getTestStatus :: HasEnvConfig env
              => Path Abs Dir
              -> RIO env TestStatus
getTestStatus dir = do
  fp <- testSuccessFile dir
  -- we could ensure the file is the right size first,
  -- but we're not expected an attack from the user's filesystem
  eres <- tryIO (readFileBinary $ toFilePath fp)
  pure $
    case eres of
      Right bs
        | bs == successBS -> TSSuccess
        | bs == failureBS -> TSFailure
      _ -> TSUnknown

--------------------------------------
-- Precompiled Cache
--
-- Idea is simple: cache information about packages built in other snapshots,
-- and then for identical matches (same flags, config options, dependencies)
-- just copy over the executables and reregister the libraries.
--------------------------------------

-- | The key containing information on the given package/configuration
-- combination. The key contains a hash of the non-directory configure
-- options for quick lookup if there's a match.
--
-- We only pay attention to non-directory options. We don't want to avoid a
-- cache hit just because it was installed in a different directory.
getPrecompiledCacheKey :: HasEnvConfig env
                    => PackageLocationImmutable
                    -> ConfigureOpts
                    -> Bool -- ^ build haddocks
                    -> Set GhcPkgId -- ^ dependencies
                    -> RIO env PrecompiledCacheKey
getPrecompiledCacheKey loc copts buildHaddocks installedPackageIDs = do
  compiler <- view actualCompilerVersionL
  cabalVersion <- view cabalVersionL

  -- The goal here is to come up with a string representing the
  -- package location which is unique. Luckily @TreeKey@s are exactly
  -- that!
  treeKey <- getPackageLocationTreeKey loc
  let packageKey = utf8BuilderToText $ display treeKey

  platformGhcDir <- platformGhcRelDir

  -- In Cabal versions 1.22 and later, the configure options contain the
  -- installed package IDs, which is what we need for a unique hash.
  -- Unfortunately, earlier Cabals don't have the information, so we must
  -- supplement it with the installed package IDs directly.
  -- See issue: https://github.com/commercialhaskell/stack/issues/1103
  let input = (coNoDirs copts, installedPackageIDs)
      optionsHash = Mem.convert $ hashWith SHA256 $ encodeUtf8 $ tshow input

  pure $ precompiledCacheKey platformGhcDir compiler cabalVersion packageKey optionsHash buildHaddocks

-- | Write out information about a newly built package
writePrecompiledCache :: HasEnvConfig env
                      => BaseConfigOpts
                      -> PackageLocationImmutable
                      -> ConfigureOpts
                      -> Bool -- ^ build haddocks
                      -> Set GhcPkgId -- ^ dependencies
                      -> Installed -- ^ library
                      -> [GhcPkgId] -- ^ sublibraries, in the GhcPkgId format
                      -> Set Text -- ^ executables
                      -> RIO env ()
writePrecompiledCache baseConfigOpts loc copts buildHaddocks depIDs mghcPkgId sublibs exes = do
  key <- getPrecompiledCacheKey loc copts buildHaddocks depIDs
  ec <- view envConfigL
  let stackRootRelative = makeRelative (view stackRootL ec)
  mlibpath <- case mghcPkgId of
    Executable _ -> pure Nothing
    Library _ ipid _ -> Just <$> pathFromPkgId stackRootRelative ipid
  sublibpaths <- mapM (pathFromPkgId stackRootRelative) sublibs
  exes' <- forM (Set.toList exes) $ \exe -> do
      name <- parseRelFile $ T.unpack exe
      stackRootRelative $ bcoSnapInstallRoot baseConfigOpts </> bindirSuffix </> name
  let precompiled = PrecompiledCache
        { pcLibrary = mlibpath
        , pcSubLibs = sublibpaths
        , pcExes = exes'
        }
  savePrecompiledCache key precompiled
  -- reuse precompiled cache with haddocks also in case when haddocks are not required
  when buildHaddocks $ do
    key' <- getPrecompiledCacheKey loc copts False depIDs
    savePrecompiledCache key' precompiled
  where
    pathFromPkgId stackRootRelative ipid = do
      ipid' <- parseRelFile $ ghcPkgIdString ipid ++ ".conf"
      stackRootRelative $ bcoSnapDB baseConfigOpts </> ipid'

-- | Check the cache for a precompiled package matching the given
-- configuration.
readPrecompiledCache :: forall env. HasEnvConfig env
                     => PackageLocationImmutable -- ^ target package
                     -> ConfigureOpts
                     -> Bool -- ^ build haddocks
                     -> Set GhcPkgId -- ^ dependencies
                     -> RIO env (Maybe (PrecompiledCache Abs))
readPrecompiledCache loc copts buildHaddocks depIDs = do
    key <- getPrecompiledCacheKey loc copts buildHaddocks depIDs
    mcache <- loadPrecompiledCache key
    maybe (pure Nothing) (fmap Just . mkAbs) mcache
  where
    -- Since commit ed9ccc08f327bad68dd2d09a1851ce0d055c0422,
    -- pcLibrary paths are stored as relative to the Stack
    -- root. Therefore, we need to prepend the Stack root when
    -- checking that the file exists. For the older cached paths, the
    -- file will contain an absolute path, which will make `stackRoot
    -- </>` a no-op.
    mkAbs :: PrecompiledCache Rel -> RIO env (PrecompiledCache Abs)
    mkAbs pc0 = do
      stackRoot <- view stackRootL
      let mkAbs' = (stackRoot </>)
      pure PrecompiledCache
        { pcLibrary = mkAbs' <$> pcLibrary pc0
        , pcSubLibs = mkAbs' <$> pcSubLibs pc0
        , pcExes = mkAbs' <$> pcExes pc0
        }
