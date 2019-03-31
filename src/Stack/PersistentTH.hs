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
module Stack.PersistentTH
    ( initCacheStorage
    , withCacheStorage
    , loadConfigCache
    , saveConfigCache
    , deactiveConfigCache
    , loadPrecompiledCache
    , savePrecompiledCache
    ) where

import qualified Data.ByteString as S
import Data.Pool (Pool, destroyAllResources)
import qualified Data.Set as Set
import Database.Persist.Sql (SqlBackend, runMigrationSilent, runSqlPool)
import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Sqlite (SqliteException)
import Path
import Path.IO (ensureDir)
import qualified RIO.Text as T
import Stack.Prelude hiding (MigrationFailure)
import Stack.Types.Build
import Stack.Types.Cache
import Stack.Types.Config (HasConfig, configCachePool, configL)
import Stack.Types.GhcPkgId

share [ mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateAllCache"
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
|]

data PersistentException =
    MigrationFailure !(Path Abs File)
                     !SqliteException
    deriving (Typeable)

instance Exception PersistentException

instance Show PersistentException where
    show = T.unpack . utf8BuilderToText . display

instance Display PersistentException where
    display (MigrationFailure fp ex) =
        "Encountered error while migrating cache database:" <> "\n    " <>
        displayShow ex <>
        "\nPlease report this on https://github.com/commercialhaskell/stack/issues" <>
        "\nAs a workaround you may delete the database in " <>
        fromString (toFilePath fp) <>
        " triggering its recreation."

-- | Initialize the cache database.
initCacheStorage ::
       HasLogFunc env
    => Path Abs File -- ^ storage file
    -> (Pool SqlBackend -> RIO env a)
    -> RIO env a
initCacheStorage fp inner = do
    ensureDir $ parent fp
    bracket
        (createSqlitePoolFromInfo (sqinfo False) 1)
        (liftIO . destroyAllResources) $ \pool -> do
        migrates <-
            wrapMigrationFailure $
            runSqlPool (runMigrationSilent migrateAllCache) pool
        forM_ migrates $ \mig ->
            logDebug $ "Migration executed: " <> display mig
    bracket
        (createSqlitePoolFromInfo (sqinfo True) 1)
        (liftIO . destroyAllResources) $ \pool -> inner pool
  where
    wrapMigrationFailure = handle (throwIO . MigrationFailure fp)
    sqinfo fk =
        set extraPragmas ["PRAGMA busy_timeout=2000;"] $
        set fkEnabled fk $ mkSqliteConnectionInfo (fromString $ toFilePath fp)

-- | Run an action in a cache database transaction.
withCacheStorage ::
       (HasConfig env, HasLogFunc env)
    => ReaderT SqlBackend (RIO env) a
    -> RIO env a
withCacheStorage action = do
    pool <- view $ configL . to configCachePool
    runSqlPool action pool

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
    withCacheStorage $ do
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
    withCacheStorage $ do
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
deactiveConfigCache :: HasConfig env => ConfigCacheKey -> RIO env ()
deactiveConfigCache key =
    withCacheStorage $
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
    withCacheStorage $ fmap snd <$> readPrecompiledCache key

-- | Insert or update 'PrecompiledCache' to the database.
savePrecompiledCache ::
       (HasConfig env, HasLogFunc env)
    => PrecompiledCacheKey
    -> PrecompiledCache Rel
    -> RIO env ()
savePrecompiledCache key new =
    withCacheStorage $ do
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
