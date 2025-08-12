{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-identities #-}

{-|
Module      : Stack.Storage.Project
Description : Work with SQLite DB for caches across a project.
License     : BSD-3-Clause

Work with SQLite database used for caches across a single project.
-}

module Stack.Storage.Project
  ( initProjectStorage
  , ConfigCacheKey
  , ConfigCacheParent (..)
  , ConfigCacheParentId
  , configCacheKey
  , loadConfigCache
  , saveConfigCache
  , deactiveConfigCache
  ) where

import qualified Data.ByteString as S
import qualified Data.Set as Set
import           Database.Persist.Sqlite
                   ( Entity (..), SelectOpt (..), SqlBackend, Unique, (=.)
                   , (==.), getBy, insert, selectList, update, updateWhere
                   )
import           Database.Persist.TH
                   ( mkMigrate, mkPersist, persistLowerCase, share
                   , sqlSettings
                   )
import           Pantry.SQLite ( initStorage, withStorage_ )
import           Stack.Prelude
import           Stack.Storage.Util
                   ( handleMigrationException, listUpdateDiff, setUpdateDiff
                   , updateCollection
                   )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..) )
import           Stack.Types.Cache
                   ( CachePkgSrc, ConfigCache (..), ConfigCacheType )
import           Stack.Types.ConfigureOpts
                   ( ConfigureOpts (..), configureOptsFromDb )
import           Stack.Types.GhcPkgId ( GhcPkgId )
import           Stack.Types.Storage ( ProjectStorage (..) )

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll"
      ]
      [persistLowerCase|
ConfigCacheParent sql="config_cache"
  directory FilePath default="(hex(randomblob(16)))"
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
  initStorage "Stack" migrateAll fp $ f . ProjectStorage

-- | Run an action in a database transaction
withProjectStorage ::
     (HasBuildConfig env, HasLogFunc env)
  => ReaderT SqlBackend (RIO env) a
  -> RIO env a
withProjectStorage inner = do
  storage <- view (buildConfigL . to (.projectStorage.projectStorage))
  withStorage_ storage inner

-- | Key used to retrieve configuration or flag cache
type ConfigCacheKey = Unique ConfigCacheParent

-- | Build key used to retrieve configuration or flag cache
configCacheKey :: Path Abs Dir -> ConfigCacheType -> ConfigCacheKey
configCacheKey dir = UniqueConfigCacheParent (toFilePath dir)

-- | Internal helper to read the t'ConfigCache'
readConfigCache ::
     (HasBuildConfig env, HasLogFunc env)
  => Entity ConfigCacheParent
  -> ReaderT SqlBackend (RIO env) ConfigCache
readConfigCache (Entity parentId configCacheParent) = do
  let pkgSrc = configCacheParent.configCacheParentPkgSrc
  pathRelatedInfo <-
    selectList
      [ConfigCacheDirOptionParent ==. parentId]
      [Asc ConfigCacheDirOptionIndex]
  nonPathRelatedInfo <-
    selectList
      [ConfigCacheNoDirOptionParent ==. parentId]
      [Asc ConfigCacheNoDirOptionIndex]
  let configureOpts = configureOptsFromDb pathRelatedInfo nonPathRelatedInfo
  deps <-
    Set.fromList . map ((.configCacheDepValue) . entityVal) <$>
    selectList [ConfigCacheDepParent ==. parentId] []
  components <-
    Set.fromList . map ((.configCacheComponentValue) . entityVal) <$>
    selectList [ConfigCacheComponentParent ==. parentId] []
  let pathEnvVar = configCacheParent.configCacheParentPathEnvVar
  let buildHaddocks = configCacheParent.configCacheParentHaddock
  pure ConfigCache
    { configureOpts
    , deps
    , components
    , buildHaddocks
    , pkgSrc
    , pathEnvVar
    }

-- | Load t'ConfigCache' from the database.
loadConfigCache ::
     (HasBuildConfig env, HasLogFunc env)
  => ConfigCacheKey
  -> RIO env (Maybe ConfigCache)
loadConfigCache key =
  withProjectStorage $
    getBy key >>= \case
      Nothing -> pure Nothing
      Just parentEntity@(Entity _ configCacheParent)
        |  configCacheParent.configCacheParentActive ->
            Just <$> readConfigCache parentEntity
        | otherwise -> pure Nothing

-- | Insert or update t'ConfigCache' to the database.
saveConfigCache ::
     (HasBuildConfig env, HasLogFunc env)
  => ConfigCacheKey
  -> ConfigCache
  -> RIO env ()
saveConfigCache key@(UniqueConfigCacheParent dir type_) new =
  withProjectStorage $ do
    (parentId, mold) <- getBy key >>= \case
        Nothing ->
          (, Nothing) <$>
          insert
            ConfigCacheParent
              { configCacheParentDirectory = dir
              , configCacheParentType = type_
              , configCacheParentPkgSrc = new.pkgSrc
              , configCacheParentActive = True
              , configCacheParentPathEnvVar = new.pathEnvVar
              , configCacheParentHaddock = new.buildHaddocks
              }
        Just parentEntity@(Entity parentId _) -> do
          old <- readConfigCache parentEntity
          update
            parentId
            [ ConfigCacheParentPkgSrc =. new.pkgSrc
            , ConfigCacheParentActive =. True
            , ConfigCacheParentPathEnvVar =. new.pathEnvVar
            ]
          pure (parentId, Just old)
    updateCollection
      (listUpdateDiff ConfigCacheDirOptionIndex)
      (uncurry $ ConfigCacheDirOption parentId)
      [ConfigCacheDirOptionParent ==. parentId]
      (maybe [] (.configureOpts.pathRelated) mold)
      new.configureOpts.pathRelated
    updateCollection
      (listUpdateDiff ConfigCacheNoDirOptionIndex)
      (uncurry $ ConfigCacheNoDirOption parentId)
      [ConfigCacheNoDirOptionParent ==. parentId]
      (maybe [] (.configureOpts.nonPathRelated) mold)
      new.configureOpts.nonPathRelated
    updateCollection
      (setUpdateDiff ConfigCacheDepValue)
      (ConfigCacheDep parentId)
      [ConfigCacheDepParent ==. parentId]
      (maybe Set.empty (.deps) mold)
      new.deps
    updateCollection
      (setUpdateDiff ConfigCacheComponentValue)
      (ConfigCacheComponent parentId)
      [ConfigCacheComponentParent ==. parentId]
      (maybe Set.empty (.components) mold)
      new.components

-- | Mark t'ConfigCache' as inactive in the database. We use a flag instead of
-- deleting the records since, in most cases, the same cache will be written
-- again within in a few seconds (after `cabal configure`), so this avoids
-- unnecessary database churn.
deactiveConfigCache :: HasBuildConfig env => ConfigCacheKey -> RIO env ()
deactiveConfigCache (UniqueConfigCacheParent dir type_) =
  withProjectStorage $
    updateWhere
      [ConfigCacheParentDirectory ==. dir, ConfigCacheParentType ==. type_]
      [ConfigCacheParentActive =. False]
