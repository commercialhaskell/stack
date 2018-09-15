{-# LANGUAGE NoImplicitPrelude #-}
-- | A sourcemap maps a package name to how it should be built,
-- including source code, flags, options, etc. This module contains
-- various stages of source map construction. See the
-- @build-overview.md@ doc for details on these stages.
module Stack.Types.SourceMap
  ( -- * Different source map types
    SMWanted (..)
  , SMActual (..)
  , SMTargets (..)
  , SourceMap (..)
    -- * Helper types
  , DepPackage (..)
  , ProjectPackage (..)
  , CommonPackage (..)
  , GlobalPackage (..)
  , SourceMapHash (..)
    -- * Functions
  , hashSourceMap
  ) where

import Stack.Prelude
import Stack.Types.Compiler
import Distribution.PackageDescription (GenericPackageDescription)

-- | Common settings for both dependency and project package.
data CommonPackage = CommonPackage
  { cpGPD :: !(IO GenericPackageDescription)
  , cpName :: !PackageName
  , cpFlags :: !(Map FlagName Bool)
  -- ^ overrides default flags
  , cpGhcOptions :: ![Text]
  }

-- | A view of a dependency package, specified in stack.yaml
data DepPackage = DepPackage
  { dpCommon :: !CommonPackage
  , dpLocation :: !PackageLocation
  , dpHidden :: !Bool
  -- ^ Should the package be hidden after registering?
  }

-- | A view of a project package needed for resolving components
data ProjectPackage = ProjectPackage
  { ppCommon :: !CommonPackage
  , ppCabalFP    :: !(Path Abs File)
  , ppResolvedDir :: !(ResolvedPath Dir)
  }

-- | A view of a package installed in the global package database.
data GlobalPackage = GlobalPackage
  {
  }

-- | A source map with information on the wanted (but not actual)
-- compiler. This is derived by parsing the @stack.yaml@ file for
-- @packages@, @extra-deps@, their configuration (e.g., flags and
-- options), and parsing the snapshot it refers to. It does not
-- include global packages or any information from the command line.
--
-- Invariant: a @PackageName@ appears in either 'smwProject' or
-- 'smwDeps', but not both.
data SMWanted = SMWanted
  { smwCompiler :: !WantedCompiler
  , smwProject :: !(Map PackageName ProjectPackage)
  , smwDeps :: !(Map PackageName DepPackage)
  }

-- | Adds in actual compiler information to 'SMWanted', in particular
-- the contents of the global package database.
--
-- Invariant: a @PackageName@ appears in only one of the @Map@s.
data SMActual = SMActual
  { smaCompiler :: !ActualCompiler
  , smaProject :: !(Map PackageName ProjectPackage)
  , smaDeps :: !(Map PackageName DepPackage)
  , smaGlobal :: !(Map PackageName GlobalPackage)
  }

-- | Builds on an 'SMActual' by resolving the targets specified on the
-- command line, potentially adding in new dependency packages in the
-- process.
data SMTargets = SMTargets
  {
  }

-- | The final source map, taking an 'SMTargets' and applying all
-- command line flags and GHC options.
data SourceMap = SourceMap
  { smCompiler :: !ActualCompiler
  }

-- | A unique hash for the immutable portions of a 'SourceMap'.
newtype SourceMapHash = SourceMapHash SHA256

-- | Get a 'SourceMapHash' for a given 'SourceMap'
hashSourceMap :: SourceMap -> SourceMapHash
hashSourceMap = undefined
