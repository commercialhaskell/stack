{-# LANGUAGE NoImplicitPrelude #-}

-- | A sourcemap maps a package name to how it should be built,
-- including source code, flags, options, etc. This module contains
-- various stages of source map construction. See the
-- @build_overview.md@ doc for details on these stages.
module Stack.Types.SourceMap
  ( -- * Different source map types
    SMWanted (..)
  , SMActual (..)
  , Target (..)
  , PackageType (..)
  , SMTargets (..)
  , SourceMap (..)
    -- * Helper types
  , FromSnapshot (..)
  , DepPackage (..)
  , ProjectPackage (..)
  , CommonPackage (..)
  , GlobalPackageVersion (..)
  , GlobalPackage (..)
  , isReplacedGlobal
  , SourceMapHash (..)
  , smRelDir
  ) where

import qualified Data.Text as T
import           Distribution.PackageDescription ( GenericPackageDescription )
import qualified Pantry.SHA256 as SHA256
import           Path
import           Stack.Prelude
import           Stack.Types.Compiler
import           Stack.Types.NamedComponent

-- | Common settings for both dependency and project package.
data CommonPackage = CommonPackage
  { cpGPD :: !(IO GenericPackageDescription)
  , cpName :: !PackageName
  , cpFlags :: !(Map FlagName Bool)
  -- ^ overrides default flags
  , cpGhcOptions :: ![Text] -- also lets us know if we're doing profiling
  , cpCabalConfigOpts :: ![Text]
  , cpHaddocks :: !Bool
  }

-- | Flag showing if package comes from a snapshot
-- needed to ignore dependency bounds between such packages
data FromSnapshot
    = FromSnapshot
    | NotFromSnapshot
    deriving Show

-- | A view of a dependency package, specified in stack.yaml
data DepPackage = DepPackage
  { dpCommon :: !CommonPackage
  , dpLocation :: !PackageLocation
  , dpHidden :: !Bool
  -- ^ Should the package be hidden after registering?
  -- Affects the script interpreter's module name import parser.
  , dpFromSnapshot :: !FromSnapshot
  -- ^ Needed to ignore bounds between snapshot packages
  -- See https://github.com/commercialhaskell/stackage/issues/3185
  }

-- | A view of a project package needed for resolving components
data ProjectPackage = ProjectPackage
  { ppCommon :: !CommonPackage
  , ppCabalFP    :: !(Path Abs File)
  , ppResolvedDir :: !(ResolvedPath Dir)
  }

-- | A view of a package installed in the global package database also
-- could include marker for a replaced global package (could be replaced
-- because of a replaced dependency)
data GlobalPackage
  = GlobalPackage !Version
  | ReplacedGlobalPackage ![PackageName]
  deriving Eq

isReplacedGlobal :: GlobalPackage -> Bool
isReplacedGlobal (ReplacedGlobalPackage _) = True
isReplacedGlobal (GlobalPackage _) = False

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
  , smwSnapshotLocation :: !RawSnapshotLocation
  -- ^ Where this snapshot is loaded from.
  }

-- | Adds in actual compiler information to 'SMWanted', in particular
-- the contents of the global package database.
--
-- Invariant: a @PackageName@ appears in only one of the @Map@s.
data SMActual global = SMActual
  { smaCompiler :: !ActualCompiler
  , smaProject :: !(Map PackageName ProjectPackage)
  , smaDeps :: !(Map PackageName DepPackage)
  , smaGlobal :: !(Map PackageName global)
  }

newtype GlobalPackageVersion = GlobalPackageVersion Version

-- | How a package is intended to be built
data Target
  = TargetAll !PackageType
  -- ^ Build all of the default components.
  | TargetComps !(Set NamedComponent)
  -- ^ Only build specific components

data PackageType = PTProject | PTDependency
  deriving (Eq, Show)

-- | Builds on an 'SMActual' by resolving the targets specified on the
-- command line, potentially adding in new dependency packages in the
-- process.
data SMTargets = SMTargets
  { smtTargets :: !(Map PackageName Target)
  , smtDeps :: !(Map PackageName DepPackage)
  }

-- | The final source map, taking an 'SMTargets' and applying all
-- command line flags and GHC options.
data SourceMap = SourceMap
  { smTargets :: !SMTargets
    -- ^ Doesn't need to be included in the hash, does not affect the
    -- source map.
  , smCompiler :: !ActualCompiler
    -- ^ Need to hash the compiler version _and_ its installation
    -- path.  Ideally there would be some kind of output from GHC
    -- telling us some unique ID for the compiler itself.
  , smProject :: !(Map PackageName ProjectPackage)
    -- ^ Doesn't need to be included in hash, doesn't affect any of
    -- the packages that get stored in the snapshot database.
  , smDeps :: !(Map PackageName DepPackage)
    -- ^ Need to hash all of the immutable dependencies, can ignore
    -- the mutable dependencies.
  , smGlobal :: !(Map PackageName GlobalPackage)
    -- ^ Doesn't actually need to be hashed, implicitly captured by
    -- smCompiler. Can be broken if someone installs new global
    -- packages. We can document that as not supported, _or_ we could
    -- actually include all of this in the hash and make Stack more
    -- resilient.
  }

-- | A unique hash for the immutable portions of a 'SourceMap'.
newtype SourceMapHash = SourceMapHash SHA256

-- | Returns relative directory name with source map's hash
smRelDir :: (MonadThrow m) => SourceMapHash -> m (Path Rel Dir)
smRelDir (SourceMapHash smh) = parseRelDir $ T.unpack $ SHA256.toHexText smh
