{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Stack.Types.CompilerPaths
License     : BSD-3-Clause
-}

module Stack.Types.CompilerPaths
  ( CompilerPaths (..)
  , GhcPkgExe (..)
  , HasCompiler (..)
  , cabalVersionL
  , compilerVersionL
  , cpWhich
  , getCompilerPath
  , getGhcPkgExe
  ) where

import           Distribution.System ( Arch )
import           Stack.Prelude
import           Stack.Types.Compiler
                   ( ActualCompiler, WhichCompiler, whichCompiler )
import           Stack.Types.CompilerBuild ( CompilerBuild )
import           Stack.Types.DumpPackage ( DumpPackage )

-- | Paths on the filesystem for the compiler we're using
data CompilerPaths = CompilerPaths
  { compilerVersion :: !ActualCompiler
  , arch :: !Arch
  , build :: !CompilerBuild
  , compiler :: !(Path Abs File)
  , pkg :: !GhcPkgExe
    -- ^ ghc-pkg or equivalent
  , interpreter :: !(Path Abs File)
    -- ^ runghc
  , haddock :: !(Path Abs File)
    -- ^ haddock, in 'IO' to allow deferring the lookup
  , sandboxed :: !Bool
    -- ^ Is this a Stack-sandboxed installation?
  , cabalVersion :: !Version
    -- ^ This is the version of Cabal that Stack will use to compile Setup.hs
    -- files in the build process.
    --
    -- Note that this is not necessarily the same version as the one that Stack
    -- depends on as a library and which is displayed when running
    -- @stack ls dependencies | grep Cabal@ in the Stack project.
  , globalDB :: !(Path Abs Dir)
    -- ^ Global package database
  , ghcInfo :: !ByteString
    -- ^ Output of @ghc --info@
  , globalDump :: !(Map PackageName DumpPackage)
  }
  deriving Show

-- | An environment which ensures that the given compiler is available on the
-- PATH
class HasCompiler env where
  compilerPathsL :: SimpleGetter env CompilerPaths

instance HasCompiler CompilerPaths where
  compilerPathsL = id

-- | Location of the ghc-pkg executable
newtype GhcPkgExe
  = GhcPkgExe (Path Abs File)
  deriving Show

cabalVersionL :: HasCompiler env => SimpleGetter env Version
cabalVersionL = compilerPathsL . to (.cabalVersion)

compilerVersionL :: HasCompiler env => SimpleGetter env ActualCompiler
compilerVersionL = compilerPathsL . to (.compilerVersion)

cpWhich :: (MonadReader env m, HasCompiler env) => m WhichCompiler
cpWhich = view $ compilerPathsL . to (whichCompiler . (.compilerVersion))

-- | Get the path for the given compiler ignoring any local binaries.
--
-- https://github.com/commercialhaskell/stack/issues/1052
getCompilerPath :: HasCompiler env => RIO env (Path Abs File)
getCompilerPath = view $ compilerPathsL . to (.compiler)

-- | Get the t'GhcPkgExe' from a 'HasCompiler' environment
getGhcPkgExe :: HasCompiler env => RIO env GhcPkgExe
getGhcPkgExe = view $ compilerPathsL . to (.pkg)
