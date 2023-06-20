{-# LANGUAGE NoImplicitPrelude #-}

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
  { cpCompilerVersion :: !ActualCompiler
  , cpArch :: !Arch
  , cpBuild :: !CompilerBuild
  , cpCompiler :: !(Path Abs File)
  , cpPkg :: !GhcPkgExe
    -- ^ ghc-pkg or equivalent
  , cpInterpreter :: !(Path Abs File)
    -- ^ runghc
  , cpHaddock :: !(Path Abs File)
    -- ^ haddock, in 'IO' to allow deferring the lookup
  , cpSandboxed :: !Bool
    -- ^ Is this a Stack-sandboxed installation?
  , cpCabalVersion :: !Version
    -- ^ This is the version of Cabal that Stack will use to compile Setup.hs
    -- files in the build process.
    --
    -- Note that this is not necessarily the same version as the one that Stack
    -- depends on as a library and which is displayed when running
    -- @stack ls dependencies | grep Cabal@ in the Stack project.
  , cpGlobalDB :: !(Path Abs Dir)
    -- ^ Global package database
  , cpGhcInfo :: !ByteString
    -- ^ Output of @ghc --info@
  , cpGlobalDump :: !(Map PackageName DumpPackage)
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
cabalVersionL = compilerPathsL.to cpCabalVersion

compilerVersionL :: HasCompiler env => SimpleGetter env ActualCompiler
compilerVersionL = compilerPathsL.to cpCompilerVersion

cpWhich :: (MonadReader env m, HasCompiler env) => m WhichCompiler
cpWhich = view $ compilerPathsL.to (whichCompiler.cpCompilerVersion)

-- | Get the path for the given compiler ignoring any local binaries.
--
-- https://github.com/commercialhaskell/stack/issues/1052
getCompilerPath :: HasCompiler env => RIO env (Path Abs File)
getCompilerPath = view $ compilerPathsL.to cpCompiler

-- | Get the 'GhcPkgExe' from a 'HasCompiler' environment
getGhcPkgExe :: HasCompiler env => RIO env GhcPkgExe
getGhcPkgExe = view $ compilerPathsL.to cpPkg
