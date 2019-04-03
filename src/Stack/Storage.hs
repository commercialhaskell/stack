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
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Work with SQLite database used for caches.
module Stack.Storage
    ( initStorage
    , withStorage
    , loadConfigCache
    , saveConfigCache
    , deactiveConfigCache
    , loadPrecompiledCache
    , savePrecompiledCache
    , loadDockerImageExeCache
    , saveDockerImageExeCache
    ) where

import qualified Data.ByteString as S
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime)
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Pantry.SQLite as SQLite
import Path
import Stack.Prelude hiding (MigrationFailure)
import Stack.Types.Build
import Stack.Types.Cache
import Stack.Types.Config (HasConfig, configStorage, configL)
import Stack.Types.GhcPkgId

share [ mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateAll"
    ]
    [persistLowerCase|
ConfigCacheParent sql="config_cache"
  key ConfigCacheKey
  pkgSrc CachePkgSrc
  active Bool
  UniqueConfigCacheParent key sql="unique_config_cache"
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
  key PrecompiledCacheKey
  library FilePath Maybe
  UniquePrecompiledCacheParent key sql="unique_precompiled_cache"
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
|]

-- | Initialize the database.
initStorage
  :: HasLogFunc env
  => Path Abs File -- ^ storage file
  -> (SQLite.Storage -> RIO env a)
  -> RIO env a
initStorage =
  SQLite.initStorage "Stack" migrateAll

-- | Run an action in a database transaction
withStorage ::
       (HasConfig env, HasLogFunc env)
    => ReaderT SqlBackend (RIO env) a
    -> RIO env a
withStorage inner =
    SQLite.withStorage inner =<< view (configL . to configStorage)

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
    return ConfigCache {..}

-- | Load 'ConfigCache' from the database.
loadConfigCache ::
       (HasConfig env, HasLogFunc env)
    => ConfigCacheKey
    -> RIO env (Maybe ConfigCache)
loadConfigCache key =
    withStorage $ do
        mparent <- getBy (UniqueConfigCacheParent key)
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
saveConfigCache key new =
    withStorage $ do
        mparent <- getBy (UniqueConfigCacheParent key)
        (parentId, mold) <-
            case mparent of
                Nothing ->
                    (, Nothing) <$>
                    insert
                        ConfigCacheParent
                            { configCacheParentKey = key
                            , configCacheParentPkgSrc = configCachePkgSrc new
                            , configCacheParentActive = True
                            }
                Just parentEntity@(Entity parentId _) -> do
                    old <- readConfigCache parentEntity
                    update
                        parentId
                        [ ConfigCacheParentPkgSrc =. configCachePkgSrc new
                        , ConfigCacheParentActive =. True
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
deactiveConfigCache key =
    withStorage $
    updateWhere
        [ConfigCacheParentKey ==. key]
        [ConfigCacheParentActive =. False]

-- | Internal helper to read the 'PrecompiledCache' from the database
readPrecompiledCache ::
       (HasConfig env, HasLogFunc env)
    => PrecompiledCacheKey
    -> ReaderT SqlBackend (RIO env) (Maybe ( PrecompiledCacheParentId
                                           , PrecompiledCache Rel))
readPrecompiledCache key = do
    mparent <- getBy (UniquePrecompiledCacheParent key)
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
loadPrecompiledCache key =
    withStorage $ fmap snd <$> readPrecompiledCache key

-- | Insert or update 'PrecompiledCache' to the database.
savePrecompiledCache ::
       (HasConfig env, HasLogFunc env)
    => PrecompiledCacheKey
    -> PrecompiledCache Rel
    -> RIO env ()
savePrecompiledCache key new =
    withStorage $ do
        mIdOld <- readPrecompiledCache key
        (parentId, mold) <-
            case mIdOld of
                Nothing ->
                    (, Nothing) <$>
                    insert
                        (PrecompiledCacheParent
                             key
                             (toFilePath <$> pcLibrary new))
                Just (parentId, old) -> do
                    update
                        parentId
                        [ PrecompiledCacheParentLibrary =.
                          fmap toFilePath (pcLibrary new)
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
