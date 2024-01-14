{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

-- | A sourcemap maps a package name to how it should be built, including source
-- code, flags, options, etc. This module contains various stages of source map
-- construction. See the @build_overview.md@ doc for details on these stages.
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
  , ppComponents
  , ppComponentsMaybe
  , ppGPD
  , ppRoot
  , ppVersion
  , CommonPackage (..)
  , GlobalPackageVersion (..)
  , GlobalPackage (..)
  , isReplacedGlobal
  , SourceMapHash (..)
  , smRelDir
  ) where

import qualified Data.Set as Set
import qualified Data.Text as T
import           Distribution.PackageDescription ( GenericPackageDescription )
import qualified Distribution.PackageDescription as C
import qualified Pantry.SHA256 as SHA256
import           Path ( parent, parseRelDir )
import           Stack.Prelude
import           Stack.Types.Compiler ( ActualCompiler )
import           Stack.Types.NamedComponent ( NamedComponent (..) )

-- | Settings common to dependency packages ('Stack.Types.SourceMap.DepPackage')
-- and project packages ('Stack.Types.SourceMap.ProjectPackage').
data CommonPackage = CommonPackage
  { gpd :: !(IO GenericPackageDescription)
  , name :: !PackageName
  , flags :: !(Map FlagName Bool)
    -- ^ overrides default flags
  , ghcOptions :: ![Text]
    -- also lets us know if we're doing profiling
  , cabalConfigOpts :: ![Text]
  , haddocks :: !Bool
    -- ^ Should Haddock documentation be built for this package?
  }

-- | Flag showing if package comes from a snapshot needed to ignore dependency
-- bounds between such packages
data FromSnapshot
  = FromSnapshot
  | NotFromSnapshot
  deriving Show

-- | A view of a dependency package, specified in stack.yaml
data DepPackage = DepPackage
  { common :: !CommonPackage
  , location :: !PackageLocation
  , hidden :: !Bool
    -- ^ Should the package be hidden after registering? Affects the script
    -- interpreter's module name import parser.
  , fromSnapshot :: !FromSnapshot
    -- ^ Needed to ignore bounds between snapshot packages
    -- See https://github.com/commercialhaskell/stackage/issues/3185
  }

-- | A view of a project package needed for resolving components
data ProjectPackage = ProjectPackage
  { ppCommon :: !CommonPackage
  , ppCabalFP :: !(Path Abs File)
  , ppResolvedDir :: !(ResolvedPath Dir)
  }

-- | A view of a package installed in the global package database also could
-- include marker for a replaced global package (could be replaced because of a
-- replaced dependency)
data GlobalPackage
  = GlobalPackage !Version
  | ReplacedGlobalPackage ![PackageName]
  deriving Eq

isReplacedGlobal :: GlobalPackage -> Bool
isReplacedGlobal (ReplacedGlobalPackage _) = True
isReplacedGlobal (GlobalPackage _) = False

-- | A source map with information on the wanted (but not actual) compiler. This
-- is derived by parsing the @stack.yaml@ file for @packages@, @extra-deps@,
-- their configuration (e.g., flags and options), and parsing the snapshot it
-- refers to. It does not include global packages or any information from the
-- command line.
--
-- Invariant: a @PackageName@ appears in either 'smwProject' or 'smwDeps', but
-- not both.
data SMWanted = SMWanted
  { smwCompiler :: !WantedCompiler
  , smwProject :: !(Map PackageName ProjectPackage)
  , smwDeps :: !(Map PackageName DepPackage)
  , smwSnapshotLocation :: !RawSnapshotLocation
    -- ^ Where this snapshot is loaded from.
  }

-- | Adds in actual compiler information to 'SMWanted', in particular the
-- contents of the global package database.
--
-- Invariant: a @PackageName@ appears in only one of the @Map@s.
data SMActual global = SMActual
  { smaCompiler :: !ActualCompiler
  , smaProject :: !(Map PackageName ProjectPackage)
  , smaDeps :: !(Map PackageName DepPackage)
  , smaGlobal :: !(Map PackageName global)
  }

newtype GlobalPackageVersion
  = GlobalPackageVersion Version

-- | How a package is intended to be built
data Target
  = TargetAll !PackageType
  -- ^ Build all of the default components.
  | TargetComps !(Set NamedComponent)
  -- ^ Only build specific components

data PackageType = PTProject | PTDependency
  deriving (Eq, Show)

-- | Builds on an 'SMActual' by resolving the targets specified on the command
-- line, potentially adding in new dependency packages in the process.
data SMTargets = SMTargets
  { smtTargets :: !(Map PackageName Target)
  , smtDeps :: !(Map PackageName DepPackage)
  }

-- | The final source map, taking an 'SMTargets' and applying all command line
-- flags and GHC options.
data SourceMap = SourceMap
  { smTargets :: !SMTargets
    -- ^ Doesn't need to be included in the hash, does not affect the source
    -- map.
  , smCompiler :: !ActualCompiler
    -- ^ Need to hash the compiler version _and_ its installation path. Ideally
    -- there would be some kind of output from GHC telling us some unique ID for
    -- the compiler itself.
  , smProject :: !(Map PackageName ProjectPackage)
    -- ^ Doesn't need to be included in hash, doesn't affect any of the packages
    -- that get stored in the snapshot database.
  , smDeps :: !(Map PackageName DepPackage)
    -- ^ Need to hash all of the immutable dependencies, can ignore the mutable
    -- dependencies.
  , smGlobal :: !(Map PackageName GlobalPackage)
    -- ^ Doesn't actually need to be hashed, implicitly captured by smCompiler.
    -- Can be broken if someone installs new global packages. We can document
    -- that as not supported, _or_ we could actually include all of this in the
    -- hash and make Stack more resilient.
  }

-- | A unique hash for the immutable portions of a 'SourceMap'.
newtype SourceMapHash
  = SourceMapHash SHA256

-- | Returns relative directory name with source map's hash
smRelDir :: (MonadThrow m) => SourceMapHash -> m (Path Rel Dir)
smRelDir (SourceMapHash smh) = parseRelDir $ T.unpack $ SHA256.toHexText smh

ppGPD :: MonadIO m => ProjectPackage -> m GenericPackageDescription
ppGPD = liftIO . (.ppCommon.gpd)

-- | Root directory for the given 'ProjectPackage'
ppRoot :: ProjectPackage -> Path Abs Dir
ppRoot = parent . (.ppCabalFP)

-- | All components available in the given 'ProjectPackage'
ppComponents :: MonadIO m => ProjectPackage -> m (Set NamedComponent)
ppComponents = ppComponentsMaybe Just

ppComponentsMaybe ::
     MonadIO m
  => (NamedComponent -> Maybe NamedComponent)
  -> ProjectPackage
  -> m (Set NamedComponent)
ppComponentsMaybe compType pp = do
  gpd <- ppGPD pp
  pure $ Set.fromList $ concat
    [ maybe [] (const $ catMaybes [compType CLib]) (C.condLibrary gpd)
    , go (compType . CExe) (fst <$> C.condExecutables gpd)
    , go (compType . CTest) (fst <$> C.condTestSuites gpd)
    , go (compType . CBench) (fst <$> C.condBenchmarks gpd)
    ]
 where
  go ::
       (T.Text -> Maybe NamedComponent)
    -> [C.UnqualComponentName]
    -> [NamedComponent]
  go wrapper = mapMaybe (wrapper . T.pack . C.unUnqualComponentName)

-- | Version for the given 'ProjectPackage
ppVersion :: MonadIO m => ProjectPackage -> m Version
ppVersion = fmap gpdVersion . ppGPD
