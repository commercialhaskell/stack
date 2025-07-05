{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors  #-}

{-|
Module      : Stack.Types.GhciPkg
License     : BSD-3-Clause
-}

module Stack.Types.GhciPkg
  ( GhciPkgInfo (..)
  , ModuleMap
  , unionModuleMaps
  , GhciPkgDesc (..)
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Stack.Ghci.Script ( ModuleName )
import           Stack.Prelude
import           Stack.Types.NamedComponent ( NamedComponent (..) )
import           Stack.Types.Package ( BuildInfoOpts, Package )
import           Stack.Types.SourceMap ( Target )

-- | Type representing information required to load a package or its components.
--
-- NOTE: v'GhciPkgInfo' has paths as list instead of a t'Data.Set.Set' to
-- preserve the order of files as a workaround for bug
-- https://ghc.haskell.org/trac/ghc/ticket/13786
data GhciPkgInfo = GhciPkgInfo
  { name :: !PackageName
  , opts :: ![(NamedComponent, BuildInfoOpts)]
  , dir :: !(Path Abs Dir)
  , modules :: !ModuleMap
  , cFiles :: ![Path Abs File] -- ^ C files.
  , mainIs :: !(Map NamedComponent [Path Abs File])
  , targetFiles :: !(Maybe [Path Abs File])
  , package :: !Package
  }
  deriving Show

-- | Type synonym representing maps from a module name to a map with all of the
-- paths that use that name. Each of those paths is associated with a set of
-- components that contain it.

-- The purpose of this complex structure is for use
-- in 'checkForDuplicateModules'.
type ModuleMap =
  Map ModuleName (Map (Path Abs File) (Set (PackageName, NamedComponent)))

unionModuleMaps :: [ModuleMap] -> ModuleMap
unionModuleMaps = M.unionsWith (M.unionWith S.union)

-- | Type representing loaded package description and related information.
data GhciPkgDesc = GhciPkgDesc
  { package :: !Package
  , cabalFP :: !(Path Abs File)
  , target :: !Target
  }
