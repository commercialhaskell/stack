{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-identities #-}

-- | Work with SQLite database used for caches.
module Stack.Storage
    ( initStorage
    , withStorage
    , ConfigCacheKey
    , configCacheKey
    , loadConfigCache
    , saveConfigCache
    , deactiveConfigCache
    , PrecompiledCacheKey
    , precompiledCacheKey
    , loadPrecompiledCache
    , savePrecompiledCache
    , loadDockerImageExeCache
    , saveDockerImageExeCache
    , loadCompilerPaths
    , saveCompilerPaths
    , upgradeChecksSince
    , logUpgradeCheck
    ) where

import qualified Data.ByteString as S
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Sqlite
import Database.Persist.TH
import Distribution.Text (simpleParse, display)
import Foreign.C.Types (CTime (..))
import qualified Pantry.SQLite as SQLite
import Path
import Path.IO (resolveFile', resolveDir')
import qualified RIO.FilePath as FP
import Stack.Prelude hiding (MigrationFailure)
import Stack.Types.Build
import Stack.Types.Cache
import Stack.Types.Compiler
import Stack.Types.CompilerBuild (CompilerBuild)
import Stack.Types.Config (HasConfig, configL, configStorage, CompilerPaths (..), GhcPkgExe (..))
import Stack.Types.GhcPkgId
import System.Posix.Types (COff (..))
import System.PosixCompat.Files (getFileStatus, fileSize, modificationTime)

share [ mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateAll"
    ]
    [persistLowerCase|
ConfigCacheParent sql="config_cache"
  directory FilePath "default=(hex(randomblob(16)))"
  type ConfigCacheType
  pkgSrc CachePkgSrc
  active Bool
  pathEnvVar Text
  haddock Bool default=0
  UniqueConfigCacheParent directory type sql="unique_config_cache"
  deriving Show

ConfigCacheDirOption
  parent ConfigCacheParentId sql="config_cache_id"
  index Int
  value String sql="option"
  UniqueConfigCacheDirOption parent index
  deriving Show

ConfigCacheNoDirOption
  parent ConfigCacheParentId sql="config_cache_id"
  index Int
  value String sql="option"
  UniqueConfigCacheNoDirOption parent index
  deriving Show

ConfigCacheDep
  parent ConfigCacheParentId sql="config_cache_id"
  value GhcPkgId sql="ghc_pkg_id"
  UniqueConfigCacheDep parent value
  deriving Show

ConfigCacheComponent
  parent ConfigCacheParentId sql="config_cache_id"
  value S.ByteString sql="component"
  UniqueConfigCacheComponent parent value
  deriving Show

PrecompiledCacheParent sql="precompiled_cache"
  platformGhcDir FilePath "default=(hex(randomblob(16)))"
  compiler Text
  cabalVersion Text
  packageKey Text
  optionsHash ByteString
  haddock Bool default=0
  library FilePath Maybe
  UniquePrecompiledCacheParent platformGhcDir compiler cabalVersion packageKey optionsHash haddock sql="unique_precompiled_cache"
  deriving Show

PrecompiledCacheSubLib
  parent PrecompiledCacheParentId sql="precompiled_cache_id"
  value FilePath sql="sub_lib"
  UniquePrecompiledCacheSubLib parent value
  deriving Show

PrecompiledCacheExe
  parent PrecompiledCacheParentId sql="precompiled_cache_id"
  value FilePath sql="exe"
  UniquePrecompiledCacheExe parent value
  deriving Show

DockerImageExeCache
  imageHash Text
  exePath FilePath
  exeTimestamp UTCTime
  compatible Bool
  DockerImageExeCacheUnique imageHash exePath exeTimestamp
  deriving Show

CompilerCache
  actualVersion ActualCompiler
  arch Text

  -- Include ghc executable size and modified time for sanity checking entries
  ghcPath FilePath
  ghcSize Int64
  ghcModified Int64

  ghcPkgPath FilePath
  runghcPath FilePath
  haddockPath FilePath

  cabalVersion Text
  globalDb FilePath
  globalDbCacheSize Int64
  globalDbCacheModified Int64
  info ByteString

  -- This is the ugliest part of this table, simply storing a Show/Read version of the
  -- data. We could do a better job with normalized data and proper table structure.
  -- However, recomputing this value in the future if the data representation changes
  -- is very cheap, so we'll take the easy way out for now.
  globalDump Text

  UniqueCompilerInfo ghcPath

-- Last time certain actions were performed
LastPerformed
  action Action
  timestamp UTCTime
  UniqueAction action
|]

-- | Initialize the database.
initStorage ::
       HasLogFunc env
    => Path Abs File -- ^ storage file
    -> (SQLite.Storage -> RIO env a)
    -> RIO env a
initStorage = SQLite.initStorage "Stack" migrateAll

-- | Run an action in a database transaction
withStorage ::
       (HasConfig env, HasLogFunc env)
    => ReaderT SqlBackend (RIO env) a
    -> RIO env a
withStorage inner =
    flip SQLite.withStorage_ inner =<< view (configL . to configStorage)

-- | Key used to retrieve configuration or flag cache
type ConfigCacheKey = Unique ConfigCacheParent

-- | Build key used to retrieve configuration or flag cache
configCacheKey :: Path Abs Dir -> ConfigCacheType -> ConfigCacheKey
configCacheKey dir = UniqueConfigCacheParent (toFilePath dir)

-- | Internal helper to read the 'ConfigCache'
readConfigCache ::
       (HasConfig env, HasLogFunc env)
    => Entity ConfigCacheParent
    -> ReaderT SqlBackend (RIO env) ConfigCache
readConfigCache (Entity parentId ConfigCacheParent {..}) = do
    let configCachePkgSrc = configCacheParentPkgSrc
    coDirs <-
        map (configCacheDirOptionValue . entityVal) <$>
        selectList
            [ConfigCacheDirOptionParent ==. parentId]
            [Asc ConfigCacheDirOptionIndex]
    coNoDirs <-
        map (configCacheNoDirOptionValue . entityVal) <$>
        selectList
            [ConfigCacheNoDirOptionParent ==. parentId]
            [Asc ConfigCacheNoDirOptionIndex]
    let configCacheOpts = ConfigureOpts {..}
    configCacheDeps <-
        Set.fromList . map (configCacheDepValue . entityVal) <$>
        selectList [ConfigCacheDepParent ==. parentId] []
    configCacheComponents <-
        Set.fromList . map (configCacheComponentValue . entityVal) <$>
        selectList [ConfigCacheComponentParent ==. parentId] []
    let configCachePathEnvVar = configCacheParentPathEnvVar
    let configCacheHaddock = configCacheParentHaddock
    return ConfigCache {..}

-- | Load 'ConfigCache' from the database.
loadConfigCache ::
       (HasConfig env, HasLogFunc env)
    => ConfigCacheKey
    -> RIO env (Maybe ConfigCache)
loadConfigCache key =
    withStorage $ do
        mparent <- getBy key
        case mparent of
            Nothing -> return Nothing
            Just parentEntity@(Entity _ ConfigCacheParent {..})
                | configCacheParentActive ->
                    Just <$> readConfigCache parentEntity
                | otherwise -> return Nothing

-- | Insert or update 'ConfigCache' to the database.
saveConfigCache ::
       (HasConfig env, HasLogFunc env)
    => ConfigCacheKey
    -> ConfigCache
    -> RIO env ()
saveConfigCache key@(UniqueConfigCacheParent dir type_) new =
    withStorage $ do
        mparent <- getBy key
        (parentId, mold) <-
            case mparent of
                Nothing ->
                    (, Nothing) <$>
                    insert
                        ConfigCacheParent
                            { configCacheParentDirectory = dir
                            , configCacheParentType = type_
                            , configCacheParentPkgSrc = configCachePkgSrc new
                            , configCacheParentActive = True
                            , configCacheParentPathEnvVar = configCachePathEnvVar new
                            , configCacheParentHaddock = configCacheHaddock new
                            }
                Just parentEntity@(Entity parentId _) -> do
                    old <- readConfigCache parentEntity
                    update
                        parentId
                        [ ConfigCacheParentPkgSrc =. configCachePkgSrc new
                        , ConfigCacheParentActive =. True
                        , ConfigCacheParentPathEnvVar =. configCachePathEnvVar new
                        ]
                    return (parentId, Just old)
        updateList
            ConfigCacheDirOption
            ConfigCacheDirOptionParent
            parentId
            ConfigCacheDirOptionIndex
            (maybe [] (coDirs . configCacheOpts) mold)
            (coDirs $ configCacheOpts new)
        updateList
            ConfigCacheNoDirOption
            ConfigCacheNoDirOptionParent
            parentId
            ConfigCacheNoDirOptionIndex
            (maybe [] (coNoDirs . configCacheOpts) mold)
            (coNoDirs $ configCacheOpts new)
        updateSet
            ConfigCacheDep
            ConfigCacheDepParent
            parentId
            ConfigCacheDepValue
            (maybe Set.empty configCacheDeps mold)
            (configCacheDeps new)
        updateSet
            ConfigCacheComponent
            ConfigCacheComponentParent
            parentId
            ConfigCacheComponentValue
            (maybe Set.empty configCacheComponents mold)
            (configCacheComponents new)

-- | Mark 'ConfigCache' as inactive in the database.
-- We use a flag instead of deleting the records since, in most cases, the same
-- cache will be written again within in a few seconds (after
-- `cabal configure`), so this avoids unnecessary database churn.
deactiveConfigCache :: HasConfig env => ConfigCacheKey -> RIO env ()
deactiveConfigCache (UniqueConfigCacheParent dir type_) =
    withStorage $
    updateWhere
        [ConfigCacheParentDirectory ==. dir, ConfigCacheParentType ==. type_]
        [ConfigCacheParentActive =. False]

-- | Key used to retrieve the precompiled cache
type PrecompiledCacheKey = Unique PrecompiledCacheParent

-- | Build key used to retrieve the precompiled cache
precompiledCacheKey ::
       Path Rel Dir
    -> ActualCompiler
    -> Version
    -> Text
    -> ByteString
    -> Bool
    -> PrecompiledCacheKey
precompiledCacheKey platformGhcDir compiler cabalVersion =
    UniquePrecompiledCacheParent
        (toFilePath platformGhcDir)
        (compilerVersionText compiler)
        (T.pack $ versionString cabalVersion)

-- | Internal helper to read the 'PrecompiledCache' from the database
readPrecompiledCache ::
       (HasConfig env, HasLogFunc env)
    => PrecompiledCacheKey
    -> ReaderT SqlBackend (RIO env) (Maybe ( PrecompiledCacheParentId
                                           , PrecompiledCache Rel))
readPrecompiledCache key = do
    mparent <- getBy key
    forM mparent $ \(Entity parentId PrecompiledCacheParent {..}) -> do
        pcLibrary <- mapM parseRelFile precompiledCacheParentLibrary
        pcSubLibs <-
            mapM (parseRelFile . precompiledCacheSubLibValue . entityVal) =<<
            selectList [PrecompiledCacheSubLibParent ==. parentId] []
        pcExes <-
            mapM (parseRelFile . precompiledCacheExeValue . entityVal) =<<
            selectList [PrecompiledCacheExeParent ==. parentId] []
        return (parentId, PrecompiledCache {..})

-- | Load 'PrecompiledCache' from the database.
loadPrecompiledCache ::
       (HasConfig env, HasLogFunc env)
    => PrecompiledCacheKey
    -> RIO env (Maybe (PrecompiledCache Rel))
loadPrecompiledCache key = withStorage $ fmap snd <$> readPrecompiledCache key

-- | Insert or update 'PrecompiledCache' to the database.
savePrecompiledCache ::
       (HasConfig env, HasLogFunc env)
    => PrecompiledCacheKey
    -> PrecompiledCache Rel
    -> RIO env ()
savePrecompiledCache key@(UniquePrecompiledCacheParent precompiledCacheParentPlatformGhcDir precompiledCacheParentCompiler precompiledCacheParentCabalVersion precompiledCacheParentPackageKey precompiledCacheParentOptionsHash precompiledCacheParentHaddock) new =
    withStorage $ do
        let precompiledCacheParentLibrary = fmap toFilePath (pcLibrary new)
        mIdOld <- readPrecompiledCache key
        (parentId, mold) <-
            case mIdOld of
                Nothing -> (, Nothing) <$> insert PrecompiledCacheParent {..}
                Just (parentId, old) -> do
                    update
                        parentId
                        [ PrecompiledCacheParentLibrary =.
                          precompiledCacheParentLibrary
                        ]
                    return (parentId, Just old)
        updateSet
            PrecompiledCacheSubLib
            PrecompiledCacheSubLibParent
            parentId
            PrecompiledCacheSubLibValue
            (maybe Set.empty (toFilePathSet . pcSubLibs) mold)
            (toFilePathSet $ pcSubLibs new)
        updateSet
            PrecompiledCacheExe
            PrecompiledCacheExeParent
            parentId
            PrecompiledCacheExeValue
            (maybe Set.empty (toFilePathSet . pcExes) mold)
            (toFilePathSet $ pcExes new)
  where
    toFilePathSet = Set.fromList . map toFilePath

-- | Get the record of whether an executable is compatible with a Docker image
loadDockerImageExeCache ::
       (HasConfig env, HasLogFunc env)
    => Text
    -> Path Abs File
    -> UTCTime
    -> RIO env (Maybe Bool)
loadDockerImageExeCache imageId exePath exeTimestamp =
    withStorage $
    fmap (dockerImageExeCacheCompatible . entityVal) <$>
    getBy (DockerImageExeCacheUnique imageId (toFilePath exePath) exeTimestamp)

-- | Sest the record of whether an executable is compatible with a Docker image
saveDockerImageExeCache ::
       (HasConfig env, HasLogFunc env)
    => Text
    -> Path Abs File
    -> UTCTime
    -> Bool
    -> RIO env ()
saveDockerImageExeCache imageId exePath exeTimestamp compatible =
    void $
    withStorage $
    upsert
        (DockerImageExeCache
             imageId
             (toFilePath exePath)
             exeTimestamp
             compatible)
        []

-- | Efficiently update a set of values stored in a database table
updateSet ::
       ( PersistEntityBackend record ~ BaseBackend backend
       , PersistField parentid
       , PersistField value
       , Ord value
       , PersistEntity record
       , MonadIO m
       , PersistQueryWrite backend
       )
    => (parentid -> value -> record)
    -> EntityField record parentid
    -> parentid
    -> EntityField record value
    -> Set value
    -> Set value
    -> ReaderT backend m ()
updateSet recordCons parentFieldCons parentId valueFieldCons old new =
    when (old /= new) $ do
        deleteWhere
            [ parentFieldCons ==. parentId
            , valueFieldCons <-. Set.toList (Set.difference old new)
            ]
        insertMany_ $
            map (recordCons parentId) $ Set.toList (Set.difference new old)

-- | Efficiently update a list of values stored in a database table.
updateList ::
       ( PersistEntityBackend record ~ BaseBackend backend
       , PersistField parentid
       , Ord value
       , PersistEntity record
       , MonadIO m
       , PersistQueryWrite backend
       )
    => (parentid -> Int -> value -> record)
    -> EntityField record parentid
    -> parentid
    -> EntityField record Int
    -> [value]
    -> [value]
    -> ReaderT backend m ()
updateList recordCons parentFieldCons parentId indexFieldCons old new =
    when (old /= new) $ do
        let oldSet = Set.fromList (zip [0 ..] old)
            newSet = Set.fromList (zip [0 ..] new)
        deleteWhere
            [ parentFieldCons ==. parentId
            , indexFieldCons <-.
              map fst (Set.toList $ Set.difference oldSet newSet)
            ]
        insertMany_ $
            map (uncurry $ recordCons parentId) $
            Set.toList (Set.difference newSet oldSet)

-- | Type-restricted version of 'fromIntegral' to ensure we're making
-- the value bigger, not smaller.
sizeToInt64 :: COff -> Int64
sizeToInt64 (COff i) = fromIntegral i -- fromIntegral added for 32-bit systems

-- | Type-restricted version of 'fromIntegral' to ensure we're making
-- the value bigger, not smaller.
timeToInt64 :: CTime -> Int64
timeToInt64 (CTime i) = fromIntegral i -- fromIntegral added for 32-bit systems

-- | Load compiler information, if available, and confirm that the
-- referenced files are unchanged. May throw exceptions!
loadCompilerPaths
  :: HasConfig env
  => Path Abs File -- ^ compiler executable
  -> CompilerBuild
  -> Bool -- ^ sandboxed?
  -> RIO env (Maybe CompilerPaths)
loadCompilerPaths compiler build sandboxed = do
  mres <- withStorage $ getBy $ UniqueCompilerInfo $ toFilePath compiler
  for mres $ \(Entity _ CompilerCache {..}) -> do
    compilerStatus <- liftIO $ getFileStatus $ toFilePath compiler
    when
      (compilerCacheGhcSize /= sizeToInt64 (fileSize compilerStatus) ||
       compilerCacheGhcModified /= timeToInt64 (modificationTime compilerStatus))
      (throwString "Compiler file metadata mismatch, ignoring cache")
    globalDbStatus <- liftIO $ getFileStatus $ compilerCacheGlobalDb FP.</> "package.cache"
    when
      (compilerCacheGlobalDbCacheSize /= sizeToInt64 (fileSize globalDbStatus) ||
       compilerCacheGlobalDbCacheModified /= timeToInt64 (modificationTime globalDbStatus))
      (throwString "Global package cache file metadata mismatch, ignoring cache")

    -- We could use parseAbsFile instead of resolveFile' below to
    -- bypass some system calls, at the cost of some really wonky
    -- error messages in case someone screws up their GHC installation
    pkgexe <- resolveFile' compilerCacheGhcPkgPath
    runghc <- resolveFile' compilerCacheRunghcPath
    haddock <- resolveFile' compilerCacheHaddockPath
    globaldb <- resolveDir' compilerCacheGlobalDb

    cabalVersion <- parseVersionThrowing $ T.unpack compilerCacheCabalVersion
    globalDump <-
      case readMaybe $ T.unpack compilerCacheGlobalDump of
        Nothing -> throwString "Global dump did not parse correctly"
        Just globalDump -> pure globalDump
    arch <-
      case simpleParse $ T.unpack compilerCacheArch of
        Nothing -> throwString $ "Invalid arch: " ++ show compilerCacheArch
        Just arch -> pure arch

    pure CompilerPaths
      { cpCompiler = compiler
      , cpCompilerVersion = compilerCacheActualVersion
      , cpArch = arch
      , cpBuild = build
      , cpPkg = GhcPkgExe pkgexe
      , cpInterpreter = runghc
      , cpHaddock = haddock
      , cpSandboxed = sandboxed
      , cpCabalVersion = cabalVersion
      , cpGlobalDB = globaldb
      , cpGhcInfo = compilerCacheInfo
      , cpGlobalDump = globalDump
      }

-- | Save compiler information. May throw exceptions!
saveCompilerPaths
  :: HasConfig env
  => CompilerPaths
  -> RIO env ()
saveCompilerPaths CompilerPaths {..} = withStorage $ do
  deleteBy $ UniqueCompilerInfo $ toFilePath cpCompiler
  compilerStatus <- liftIO $ getFileStatus $ toFilePath cpCompiler
  globalDbStatus <- liftIO $ getFileStatus $ toFilePath $ cpGlobalDB </> $(mkRelFile "package.cache")
  let GhcPkgExe pkgexe = cpPkg
  insert_ CompilerCache
    { compilerCacheActualVersion = cpCompilerVersion
    , compilerCacheGhcPath = toFilePath cpCompiler
    , compilerCacheGhcSize = sizeToInt64 $ fileSize compilerStatus
    , compilerCacheGhcModified = timeToInt64 $ modificationTime compilerStatus
    , compilerCacheGhcPkgPath = toFilePath pkgexe
    , compilerCacheRunghcPath = toFilePath cpInterpreter
    , compilerCacheHaddockPath = toFilePath cpHaddock
    , compilerCacheCabalVersion = T.pack $ versionString cpCabalVersion
    , compilerCacheGlobalDb = toFilePath cpGlobalDB
    , compilerCacheGlobalDbCacheSize = sizeToInt64 $ fileSize globalDbStatus
    , compilerCacheGlobalDbCacheModified = timeToInt64 $ modificationTime globalDbStatus
    , compilerCacheInfo = cpGhcInfo
    , compilerCacheGlobalDump = tshow cpGlobalDump
    , compilerCacheArch = T.pack $ Distribution.Text.display cpArch
    }

-- | How many upgrade checks have occurred since the given timestamp?
upgradeChecksSince :: HasConfig env => UTCTime -> RIO env Int
upgradeChecksSince since = withStorage $ count
  [ LastPerformedAction ==. UpgradeCheck
  , LastPerformedTimestamp >=. since
  ]

-- | Log in the database that an upgrade check occurred at the given time.
logUpgradeCheck :: HasConfig env => UTCTime -> RIO env ()
logUpgradeCheck time = withStorage $ void $ upsert
  (LastPerformed UpgradeCheck time)
  [LastPerformedTimestamp =. time]
