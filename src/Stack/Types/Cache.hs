{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Stack.Types.Cache
License     : BSD-3-Clause
-}

module Stack.Types.Cache
  ( FileCache
  , BuildFileCache (..)
  , FileCacheInfo (..)
  , ConfigCache (..)
  , CachePkgSrc (..)
  , PrecompiledCache (..)
  , ConfigCacheType (..)
  , Action (..)
  ) where

import           Data.Aeson
                   ( ToJSON (..), FromJSON (..), (.=), (.:), object, withObject
                   )
import qualified Data.ByteString as S
import qualified Data.Text as T
import           Database.Persist.Sql
                   ( PersistField (..), PersistFieldSql (..), PersistValue (..)
                   , SqlType (..)
                   )
import           Stack.Prelude
import           Stack.Types.ConfigureOpts ( ConfigureOpts )
import           Stack.Types.GhcPkgId
                   ( GhcPkgId, ghcPkgIdToText, parseGhcPkgId )

-- | Type representing types of cache in the Stack project SQLite database.
data ConfigCacheType
  = ConfigCacheTypeConfig
    -- ^ Cabal configuration cache.
  | ConfigCacheTypeFlagLibrary GhcPkgId
    -- ^ Library Cabal flag cache.
  | ConfigCacheTypeFlagExecutable PackageIdentifier
    -- ^ Executable Cabal flag cache.
  deriving (Eq, Show)

instance PersistField ConfigCacheType where
  toPersistValue ConfigCacheTypeConfig = PersistText "config"
  toPersistValue (ConfigCacheTypeFlagLibrary v) =
    PersistText $ "lib:" <> ghcPkgIdToText v
  toPersistValue (ConfigCacheTypeFlagExecutable v) =
    PersistText $ "exe:" <> T.pack (packageIdentifierString v)
  fromPersistValue (PersistText t) =
    fromMaybe (Left $ "Unexpected ConfigCacheType value: " <> t) $
    config <|> fmap lib (T.stripPrefix "lib:" t) <|>
    fmap exe (T.stripPrefix "exe:" t)
   where
    config
      | t == "config" = Just (Right ConfigCacheTypeConfig)
      | otherwise = Nothing
    lib v = do
      ghcPkgId <- mapLeft tshow (parseGhcPkgId v)
      Right $ ConfigCacheTypeFlagLibrary ghcPkgId
    exe v = do
      pkgId <-
        maybe (Left $ "Unexpected ConfigCacheType value: " <> t) Right $
        parsePackageIdentifier (T.unpack v)
      Right $ ConfigCacheTypeFlagExecutable pkgId
  fromPersistValue _ = Left "Unexpected ConfigCacheType type"

instance PersistFieldSql ConfigCacheType where
  sqlType _ = SqlString

-- | Type representing actions for which the last time the action was performed
-- should be cached.
data Action
  = UpgradeCheck
  deriving (Eq, Ord, Show)

instance PersistField Action where
  toPersistValue UpgradeCheck = PersistInt64 1
  fromPersistValue (PersistInt64 1) = Right UpgradeCheck
  fromPersistValue x = Left $ T.pack $ "Invalid Action: " ++ show x

instance PersistFieldSql Action where
  sqlType _ = SqlInt64

-- | Type synonym representing caches of files and information about them
-- sufficient to identify if they have changed subsequently.
type FileCache = Map FilePath FileCacheInfo

-- | Type representing caches of information about files sufficient to identify
-- if they have changed subsequently. Stored on disk.
newtype BuildFileCache = BuildFileCache
  { fileCache :: FileCache
  }
  deriving (Eq, FromJSON, Generic, Show, ToJSON, Typeable)

instance NFData BuildFileCache

-- | Type representing information about a file sufficient to identify if
-- it has changed subsequently.
newtype FileCacheInfo = FileCacheInfo
  { hash :: SHA256
    -- ^ SHA-256 hash of file contents.
  }
  deriving (Eq, Generic, Show, Typeable)

instance NFData FileCacheInfo

-- Provided for storing the t'BuildFileCache' values in a file. But maybe
-- JSON/YAML isn't the right choice here, worth considering.
instance ToJSON FileCacheInfo where
  toJSON fileCacheInfo = object
    [ "hash" .= fileCacheInfo.hash
    ]

instance FromJSON FileCacheInfo where
  parseJSON = withObject "FileCacheInfo" $ \o -> FileCacheInfo
    <$> o .: "hash"

-- | Stored in the project's SQLite database to know whether the Cabal
-- configuration has changed or libarary or executable Cabal flags have changed.
data ConfigCache = ConfigCache
  { configureOpts :: !ConfigureOpts
    -- ^ All Cabal configure options used for this package.
  , deps :: !(Set GhcPkgId)
    -- ^ The GhcPkgIds of all of the dependencies. Since Cabal doesn't take
    -- the complete GhcPkgId (only a PackageIdentifier) in the configure
    -- options, just using the previous value is insufficient to know if
    -- dependencies have changed.
  , components :: !(Set S.ByteString)
    -- ^ The components to be built. It's a bit of a hack to include this in
    -- here, as it's not a configure option (just a build option), but this
    -- is a convenient way to force compilation when the components change.
  , buildHaddocks :: !Bool
    -- ^ Is Haddock documentation to be built?
  , pkgSrc :: !CachePkgSrc
    -- ^ The origin of the package's source code.
  , pathEnvVar :: !Text
    -- ^ Value of the PATH environment variable. See
    -- <https://github.com/commercialhaskell/stack/issues/3138>
  }
  deriving (Data, Eq, Generic, Show, Typeable)

instance NFData ConfigCache

data CachePkgSrc
  = CacheSrcUpstream
  | CacheSrcLocal FilePath
  deriving (Data, Eq, Generic, Read, Show, Typeable)

instance NFData CachePkgSrc

instance PersistField CachePkgSrc where
  toPersistValue CacheSrcUpstream = PersistText "upstream"
  toPersistValue (CacheSrcLocal fp) = PersistText ("local:" <> T.pack fp)
  fromPersistValue (PersistText t) =
    if t == "upstream"
      then Right CacheSrcUpstream
      else case T.stripPrefix "local:" t of
        Just fp -> Right $ CacheSrcLocal (T.unpack fp)
        Nothing -> Left $ "Unexpected CachePkgSrc value: " <> t
  fromPersistValue _ = Left "Unexpected CachePkgSrc type"

instance PersistFieldSql CachePkgSrc where
  sqlType _ = SqlString

-- | Information on a compiled package: the library .conf file (if relevant),
-- the sub-libraries (if present) and all of the executable paths.
data PrecompiledCache base = PrecompiledCache
  { library :: !(Maybe (Path base File))
    -- ^ .conf file inside the package database
  , subLibs :: ![Path base File]
    -- ^ .conf file inside the package database, for each of the sub-libraries
  , exes    :: ![Path base File]
    -- ^ Full paths to executables
  }
  deriving (Eq, Generic, Show, Typeable)

instance NFData (PrecompiledCache Abs)

instance NFData (PrecompiledCache Rel)
