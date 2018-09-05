{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Stack.StoreTH
  ( decodeConfigCache
  , encodeConfigCache

  , decodePrecompiledCache
  , encodePrecompiledCache

  , decodeOrLoadInstalledCache
  , encodeInstalledCache

  , decodeOrLoadLoadedSnapshot
  ) where

import Data.Store.Version
import Stack.Prelude
import Stack.Types.Build
import Stack.Types.BuildPlan
import Stack.Types.PackageDump

decodeConfigCache
  :: HasLogFunc env
  => Path Abs File
  -> RIO env (Maybe ConfigCache)
decodeConfigCache = $(versionedDecodeFile configCacheVC)

encodeConfigCache
  :: HasLogFunc env
  => Path Abs File
  -> ConfigCache
  -> RIO env ()
encodeConfigCache = $(versionedEncodeFile configCacheVC)

decodePrecompiledCache
  :: HasLogFunc env
  => Path Abs File
  -> RIO env (Maybe PrecompiledCache)
decodePrecompiledCache = $(versionedDecodeFile precompiledCacheVC)

encodePrecompiledCache
  :: HasLogFunc env
  => Path Abs File
  -> PrecompiledCache
  -> RIO env ()
encodePrecompiledCache = $(versionedEncodeFile precompiledCacheVC)

decodeOrLoadInstalledCache
  :: HasLogFunc env
  => Path Abs File
  -> RIO env InstalledCacheInner
  -> RIO env InstalledCacheInner
decodeOrLoadInstalledCache = $(versionedDecodeOrLoad installedCacheVC)

encodeInstalledCache
  :: HasLogFunc env
  => Path Abs File
  -> InstalledCacheInner
  -> RIO env ()
encodeInstalledCache = $(versionedEncodeFile installedCacheVC)

decodeOrLoadLoadedSnapshot
  :: HasLogFunc env
  => Path Abs File
  -> RIO env LoadedSnapshot
  -> RIO env LoadedSnapshot
decodeOrLoadLoadedSnapshot = $(versionedDecodeOrLoad loadedSnapshotVC)
