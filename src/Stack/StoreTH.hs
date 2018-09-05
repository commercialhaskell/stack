{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Stack.StoreTH
  ( decodeBuildCache
  , encodeBuildCache

  , decodeConfigCache
  , encodeConfigCache

  , decodeModTime
  , encodeModTime

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
import Stack.Types.Package
import Stack.Types.PackageDump

decodeBuildCache
  :: HasLogFunc env
  => Path Abs File
  -> RIO env (Maybe BuildCache)
encodeBuildCache = $(versionedEncodeFile buildCacheVC)

encodeBuildCache
  :: HasLogFunc env
  => Path Abs File
  -> BuildCache
  -> RIO env ()
decodeBuildCache = $(versionedDecodeFile buildCacheVC)

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

decodeModTime
  :: HasLogFunc env
  => Path Abs File
  -> RIO env (Maybe ModTime)
decodeModTime = $(versionedDecodeFile modTimeVC)

encodeModTime
  :: HasLogFunc env
  => Path Abs File
  -> ModTime
  -> RIO env ()
encodeModTime = $(versionedEncodeFile modTimeVC)

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
