{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-identities #-}

-- | Work with SQLite database used for caches across a single project.
module Stack.Storage.Project
    ( initProjectStorage
    , ConfigCacheKey
    , configCacheKey
    , loadConfigCache
    , saveConfigCache
    , deactiveConfigCache
    ) where

import qualified Data.ByteString as S
import qualified Data.Set as Set
import           Database.Persist.Sqlite
import           Database.Persist.TH
import qualified Pantry.Internal as SQLite
import           Path
import           Stack.Prelude
import           Stack.Storage.Util
                   ( handleMigrationException, updateList, updateSet )
import           Stack.Types.Build
import           Stack.Types.Cache
import           Stack.Types.Config
                   ( HasBuildConfig, ProjectStorage (..), bcProjectStorage
                   , buildConfigL
                   )
import           Stack.Types.GhcPkgId

share [ mkPersist sqlSettings
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
  parent ConfigCacheParentId sql="config_cache_id" OnDeleteCascade
  index Int
  value String sql="option"
  UniqueConfigCacheDirOption parent index
  deriving Show

ConfigCacheNoDirOption
  parent ConfigCacheParentId sql="config_cache_id" OnDeleteCascade
  index Int
  value String sql="option"
  UniqueConfigCacheNoDirOption parent index
  deriving Show

ConfigCacheDep
  parent ConfigCacheParentId sql="config_cache_id" OnDeleteCascade
  value GhcPkgId sql="ghc_pkg_id"
  UniqueConfigCacheDep parent value
  deriving Show

ConfigCacheComponent
  parent ConfigCacheParentId sql="config_cache_id" OnDeleteCascade
  value S.ByteString sql="component"
  UniqueConfigCacheComponent parent value
  deriving Show
|]

-- | Initialize the database.
initProjectStorage ::
       HasLogFunc env
    => Path Abs File -- ^ storage file
    -> (ProjectStorage -> RIO env a)
    -> RIO env a
initProjectStorage fp f = handleMigrationException $
    SQLite.initStorage "Stack" migrateAll fp $ f . ProjectStorage

-- | Run an action in a database transaction
withProjectStorage ::
       (HasBuildConfig env, HasLogFunc env)
    => ReaderT SqlBackend (RIO env) a
    -> RIO env a
withProjectStorage inner = do
    storage <- view (buildConfigL . to bcProjectStorage . to unProjectStorage)
    SQLite.withStorage_ storage inner

-- | Key used to retrieve configuration or flag cache
type ConfigCacheKey = Unique ConfigCacheParent

-- | Build key used to retrieve configuration or flag cache
configCacheKey :: Path Abs Dir -> ConfigCacheType -> ConfigCacheKey
configCacheKey dir = UniqueConfigCacheParent (toFilePath dir)

-- | Internal helper to read the 'ConfigCache'
readConfigCache ::
       (HasBuildConfig env, HasLogFunc env)
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
    pure ConfigCache {..}

-- | Load 'ConfigCache' from the database.
loadConfigCache ::
       (HasBuildConfig env, HasLogFunc env)
    => ConfigCacheKey
    -> RIO env (Maybe ConfigCache)
loadConfigCache key =
    withProjectStorage $ do
        mparent <- getBy key
        case mparent of
            Nothing -> pure Nothing
            Just parentEntity@(Entity _ ConfigCacheParent {..})
                | configCacheParentActive ->
                    Just <$> readConfigCache parentEntity
                | otherwise -> pure Nothing

-- | Insert or update 'ConfigCache' to the database.
saveConfigCache ::
       (HasBuildConfig env, HasLogFunc env)
    => ConfigCacheKey
    -> ConfigCache
    -> RIO env ()
saveConfigCache key@(UniqueConfigCacheParent dir type_) new =
    withProjectStorage $ do
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
                    pure (parentId, Just old)
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
deactiveConfigCache :: HasBuildConfig env => ConfigCacheKey -> RIO env ()
deactiveConfigCache (UniqueConfigCacheParent dir type_) =
    withProjectStorage $
    updateWhere
        [ConfigCacheParentDirectory ==. dir, ConfigCacheParentType ==. type_]
        [ConfigCacheParentActive =. False]
