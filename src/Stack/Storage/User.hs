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

-- | Work with SQLite database used for caches across an entire user account.
module Stack.Storage.User
  ( initUserStorage
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

import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Time.Clock ( UTCTime )
import           Database.Persist.Sqlite
                   ( Entity (..), SqlBackend, Unique, (=.), (==.), (>=.), count
                   , deleteBy, getBy, insert, insert_, selectList, update
                   , upsert
                   )
import           Database.Persist.TH
                   ( mkMigrate, mkPersist, persistLowerCase, share
                   , sqlSettings
                   )
import           Distribution.Text ( simpleParse, display )
import           Foreign.C.Types ( CTime (..) )
import qualified Pantry.Internal as SQLite
import           Path ( (</>), mkRelFile, parseRelFile )
import           Path.IO ( resolveFile', resolveDir' )
import qualified RIO.FilePath as FP
import           Stack.Prelude
import           Stack.Storage.Util ( handleMigrationException, updateSet )
import           Stack.Types.Build ( PrecompiledCache (..) )
import           Stack.Types.Cache ( Action (..) )
import           Stack.Types.Compiler ( ActualCompiler, compilerVersionText )
import           Stack.Types.CompilerBuild ( CompilerBuild )
import           Stack.Types.Config
                   ( CompilerPaths (..), GhcPkgExe (..), HasConfig (..)
                   , UserStorage (..), configUserStorage
                   )
import           System.Posix.Types ( COff (..) )
import           System.PosixCompat.Files
                   ( fileSize, getFileStatus, modificationTime )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Storage.User" module.
data StorageUserException
  = CompilerFileMetadataMismatch
  | GlobalPackageCacheFileMetadataMismatch
  | GlobalDumpParseFailure
  | CompilerCacheArchitectureInvalid Text
  deriving (Show, Typeable)

instance Exception StorageUserException where
  displayException CompilerFileMetadataMismatch =
    "Error: [S-8196]\n"
    ++ "Compiler file metadata mismatch, ignoring cache."
  displayException GlobalPackageCacheFileMetadataMismatch =
    "Error: [S-5378]\n"
    ++ "Global package cache file metadata mismatch, ignoring cache."
  displayException GlobalDumpParseFailure =
    "Error: [S-2673]\n"
    ++ "Global dump did not parse correctly."
  displayException
    (CompilerCacheArchitectureInvalid compilerCacheArch) = concat
      [ "Error: [S-8441]\n"
      , "Invalid arch: "
      , show compilerCacheArch
      ]

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll"
      ]
      [persistLowerCase|
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
  parent PrecompiledCacheParentId sql="precompiled_cache_id" OnDeleteCascade
  value FilePath sql="sub_lib"
  UniquePrecompiledCacheSubLib parent value
  deriving Show

PrecompiledCacheExe
  parent PrecompiledCacheParentId sql="precompiled_cache_id" OnDeleteCaseCascade
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
initUserStorage ::
     HasLogFunc env
  => Path Abs File -- ^ storage file
  -> (UserStorage -> RIO env a)
  -> RIO env a
initUserStorage fp f = handleMigrationException $
  SQLite.initStorage "Stack" migrateAll fp $ f . UserStorage

-- | Run an action in a database transaction
withUserStorage ::
     (HasConfig env, HasLogFunc env)
  => ReaderT SqlBackend (RIO env) a
  -> RIO env a
withUserStorage inner = do
  storage <- view (configL . to configUserStorage . to unUserStorage)
  SQLite.withStorage_ storage inner

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
    pure (parentId, PrecompiledCache {..})

-- | Load 'PrecompiledCache' from the database.
loadPrecompiledCache ::
     (HasConfig env, HasLogFunc env)
  => PrecompiledCacheKey
  -> RIO env (Maybe (PrecompiledCache Rel))
loadPrecompiledCache key =
  withUserStorage $ fmap snd <$> readPrecompiledCache key

-- | Insert or update 'PrecompiledCache' to the database.
savePrecompiledCache ::
     (HasConfig env, HasLogFunc env)
  => PrecompiledCacheKey
  -> PrecompiledCache Rel
  -> RIO env ()
savePrecompiledCache
  key@( UniquePrecompiledCacheParent
          precompiledCacheParentPlatformGhcDir
          precompiledCacheParentCompiler
          precompiledCacheParentCabalVersion
          precompiledCacheParentPackageKey
          precompiledCacheParentOptionsHash
          precompiledCacheParentHaddock
      )
  new
  = withUserStorage $ do
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
            pure (parentId, Just old)
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
loadDockerImageExeCache imageId exePath exeTimestamp = withUserStorage $
  fmap (dockerImageExeCacheCompatible . entityVal) <$>
  getBy (DockerImageExeCacheUnique imageId (toFilePath exePath) exeTimestamp)

-- | Sets the record of whether an executable is compatible with a Docker image
saveDockerImageExeCache ::
     (HasConfig env, HasLogFunc env)
  => Text
  -> Path Abs File
  -> UTCTime
  -> Bool
  -> RIO env ()
saveDockerImageExeCache imageId exePath exeTimestamp compatible = void $
  withUserStorage $
    upsert
      ( DockerImageExeCache
          imageId
          (toFilePath exePath)
          exeTimestamp
          compatible
      )
      []

-- | Type-restricted version of 'fromIntegral' to ensure we're making the value
-- bigger, not smaller.
sizeToInt64 :: COff -> Int64
sizeToInt64 (COff i) = fromIntegral i -- fromIntegral added for 32-bit systems

-- | Type-restricted version of 'fromIntegral' to ensure we're making the value
-- bigger, not smaller.
timeToInt64 :: CTime -> Int64
timeToInt64 (CTime i) = fromIntegral i -- fromIntegral added for 32-bit systems

-- | Load compiler information, if available, and confirm that the referenced
-- files are unchanged. May throw exceptions!
loadCompilerPaths ::
     HasConfig env
  => Path Abs File -- ^ compiler executable
  -> CompilerBuild
  -> Bool -- ^ sandboxed?
  -> RIO env (Maybe CompilerPaths)
loadCompilerPaths compiler build sandboxed = do
  mres <- withUserStorage $ getBy $ UniqueCompilerInfo $ toFilePath compiler
  for mres $ \(Entity _ CompilerCache {..}) -> do
    compilerStatus <- liftIO $ getFileStatus $ toFilePath compiler
    when
      (  compilerCacheGhcSize /= sizeToInt64 (fileSize compilerStatus)
      || compilerCacheGhcModified /=
           timeToInt64 (modificationTime compilerStatus)
      )
      (throwIO CompilerFileMetadataMismatch)
    globalDbStatus <-
      liftIO $ getFileStatus $ compilerCacheGlobalDb FP.</> "package.cache"
    when
      (  compilerCacheGlobalDbCacheSize /= sizeToInt64 (fileSize globalDbStatus)
      || compilerCacheGlobalDbCacheModified /=
           timeToInt64 (modificationTime globalDbStatus)
      )
      (throwIO GlobalPackageCacheFileMetadataMismatch)

    -- We could use parseAbsFile instead of resolveFile' below to bypass some
    -- system calls, at the cost of some really wonky error messages in case
    -- someone screws up their GHC installation
    pkgexe <- resolveFile' compilerCacheGhcPkgPath
    runghc <- resolveFile' compilerCacheRunghcPath
    haddock <- resolveFile' compilerCacheHaddockPath
    globaldb <- resolveDir' compilerCacheGlobalDb

    cabalVersion <- parseVersionThrowing $ T.unpack compilerCacheCabalVersion
    globalDump <-
      case readMaybe $ T.unpack compilerCacheGlobalDump of
        Nothing -> throwIO GlobalDumpParseFailure
        Just globalDump -> pure globalDump
    arch <-
      case simpleParse $ T.unpack compilerCacheArch of
        Nothing -> throwIO $ CompilerCacheArchitectureInvalid compilerCacheArch
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
saveCompilerPaths ::
     HasConfig env
  => CompilerPaths
  -> RIO env ()
saveCompilerPaths CompilerPaths {..} = withUserStorage $ do
  deleteBy $ UniqueCompilerInfo $ toFilePath cpCompiler
  compilerStatus <- liftIO $ getFileStatus $ toFilePath cpCompiler
  globalDbStatus <-
    liftIO $
      getFileStatus $ toFilePath $ cpGlobalDB </> $(mkRelFile "package.cache")
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
    , compilerCacheGlobalDbCacheModified =
        timeToInt64 $ modificationTime globalDbStatus
    , compilerCacheInfo = cpGhcInfo
    , compilerCacheGlobalDump = tshow cpGlobalDump
    , compilerCacheArch = T.pack $ Distribution.Text.display cpArch
    }

-- | How many upgrade checks have occurred since the given timestamp?
upgradeChecksSince :: HasConfig env => UTCTime -> RIO env Int
upgradeChecksSince since = withUserStorage $ count
  [ LastPerformedAction ==. UpgradeCheck
  , LastPerformedTimestamp >=. since
  ]

-- | Log in the database that an upgrade check occurred at the given time.
logUpgradeCheck :: HasConfig env => UTCTime -> RIO env ()
logUpgradeCheck time = withUserStorage $ void $ upsert
  (LastPerformed UpgradeCheck time)
  [LastPerformedTimestamp =. time]
