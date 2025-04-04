{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | A module providing the types that represent different sorts of components
-- of a package (library and sub-library, foreign library, executable, test
-- suite and benchmark).
module Stack.Types.Component
  ( StackLibrary (..)
  , StackForeignLibrary (..)
  , StackExecutable (..)
  , StackTestSuite (..)
  , StackBenchmark (..)
  , StackUnqualCompName (..)
  , StackBuildInfo (..)
  , HasName
  , HasBuildInfo
  , HasComponentInfo
  ) where

import           Distribution.Compiler ( PerCompilerFlavor )
import           Distribution.ModuleName ( ModuleName )
import           Distribution.PackageDescription
                   ( BenchmarkInterface, Dependency, TestSuiteInterface )
import           Distribution.Simple ( Extension, Language )
import           Distribution.Utils.Path ( Pkg, Source, SymbolicPath )
import qualified Distribution.Utils.Path as Cabal
import           GHC.Records ( HasField (..) )
import           Stack.Prelude
import           Stack.Types.ComponentUtils
                   ( StackUnqualCompName (..), emptyCompName )
import           Stack.Types.Dependency ( DepValue )
import           Stack.Types.NamedComponent ( NamedComponent (..) )

-- | A type representing (unnamed) main library or sub-library components of a
-- package.
--
-- Cabal-syntax uses data constructors
-- 'Distribution.Types.LibraryName.LMainLibName' and
-- 'Distribution.Types.LibraryName.LSubLibName' to distinguish main libraries
-- and sub-libraries. We do not do so, as the \'missing\' name in the case of a
-- main library can be represented by the empty string.
--
-- The corresponding Cabal-syntax type is 'Distribution.Types.Library.Library'.
data StackLibrary = StackLibrary
  { name :: StackUnqualCompName
  , buildInfo :: !StackBuildInfo
  , exposedModules :: [ModuleName]
    -- |^ This is only used for gathering the files related to this component.
  }
  deriving (Show, Typeable)

-- | A type representing foreign library components of a package.
--
-- The corresponding Cabal-syntax type is
-- 'Distribution.Types.Foreign.Libraries.ForeignLib'.
data StackForeignLibrary = StackForeignLibrary
  { name :: StackUnqualCompName
  , buildInfo :: !StackBuildInfo
  }
  deriving (Show, Typeable)

-- | A type representing executable components of a package.
--
-- The corresponding Cabal-syntax type is
-- 'Distribution.Types.Executable.Executable'.
data StackExecutable = StackExecutable
  { name :: StackUnqualCompName
  , buildInfo :: !StackBuildInfo
  , modulePath :: FilePath
  }
  deriving (Show, Typeable)

-- | A type representing test suite components of a package.
--
-- The corresponding Cabal-syntax type is
-- 'Distribution.Types.TestSuite.TestSuite'.
data StackTestSuite = StackTestSuite
  { name :: StackUnqualCompName
  , buildInfo :: !StackBuildInfo
  , interface :: !TestSuiteInterface
  }
  deriving (Show, Typeable)

-- | A type representing benchmark components of a package.
--
-- The corresponding Cabal-syntax type is
-- 'Distribution.Types.Benchmark.Benchmark'.
data StackBenchmark = StackBenchmark
  { name :: StackUnqualCompName
  , buildInfo :: StackBuildInfo
  , interface :: BenchmarkInterface
    -- ^ This is only used for gathering the files related to this component.
  }
  deriving (Show, Typeable)

-- | Type representing the name of an executable.
newtype ExeName = ExeName Text
  deriving (Data, Eq, Hashable, IsString, Generic, NFData, Ord, Show, Typeable)

-- | Type representing information needed to build. The file gathering-related
-- fields are lazy because they are not always needed.
--
-- The corresponding Cabal-syntax type is
-- 'Distribution.Types.BuildInfo.BuildInfo'.

-- We don't use the Cabal-syntax type because Cabal provides a list of
-- dependencies, and Stack needs a Map and only a small subset of all the
-- information in Cabal-syntax type.
data StackBuildInfo = StackBuildInfo
  { buildable :: !Bool
    -- ^ Corresponding to Cabal-syntax's
    -- 'Distribution.Types.BuildInfo.buildable'. The component is buildable
    -- here.
  , dependency :: !(Map PackageName DepValue)
    -- ^ Corresponding to Cabal-syntax's
    -- 'Distribution.Types.BuildInfo.targetBuildDepends'. Dependencies specific
    -- to a library or executable target.
  , unknownTools :: Set Text
    -- ^ From Cabal-syntax's 'Distribution.Types.BuildInfo.buildTools'. We only
    -- keep the legacy build tool depends that we know (from a hardcoded list).
    -- We only use the deduplication aspect of the Set here, as this field is
    -- only used for error reporting in the end. This is lazy because it's an
    -- error reporting field only.
  , otherModules :: [ModuleName]
    -- ^ Only used in file gathering. See usage in "Stack.ComponentFile" module.
  , jsSources :: [FilePath]
    -- ^ Only used in file gathering. See usage in "Stack.ComponentFile" module.
  , hsSourceDirs :: [SymbolicPath Pkg (Cabal.Dir Source)]
    -- ^ Only used in file & opts gathering. See usage in "Stack.ComponentFile"
    -- module for fle gathering.
  , cSources :: [FilePath]
    -- ^ Only used in file gathering. See usage in "Stack.ComponentFile" module.
  , cppOptions :: [String]
    -- ^ Only used in opts gathering. See usage in "Stack.Package" module.
  , targetBuildDepends :: [Dependency]
    -- ^ Only used in opts gathering.
  , options :: PerCompilerFlavor [String]
    -- ^ Only used in opts gathering.
  , allLanguages :: [Language]
    -- ^ Only used in opts gathering.
  , usedExtensions :: [Extension]
    -- ^ Only used in opts gathering.
  , includeDirs :: [FilePath]
    -- ^ Only used in opts gathering.
  , extraLibs :: [String]
    -- ^ Only used in opts gathering.
  , extraLibDirs :: [String]
    -- ^ Only used in opts gathering.
  , frameworks :: [String]
    -- ^ Only used in opts gathering.
  }
  deriving (Show)

-- | Type synonym for a 'HasField' constraint.
type HasName component = HasField "name" component StackUnqualCompName

-- | Type synonym for a 'HasField' constraint.
type HasBuildInfo component = HasField "buildInfo" component StackBuildInfo

instance HasField "qualifiedName" StackLibrary NamedComponent where
  getField v
    | rawName == emptyCompName = CLib
    | otherwise = CSubLib rawName
    where
      rawName = v.name

instance HasField "qualifiedName" StackForeignLibrary NamedComponent where
  getField = CFlib . (.name)

instance HasField "qualifiedName" StackExecutable NamedComponent where
  getField = CExe . (.name)

instance HasField "qualifiedName" StackTestSuite NamedComponent where
  getField = CTest . (.name)

instance HasField "qualifiedName" StackBenchmark NamedComponent where
  getField = CBench . (.name)

-- | Type synonym for a 'HasField' constraint which represent a virtual field,
-- computed from the type, the NamedComponent constructor and the name.
type HasQualiName component = HasField "qualifiedName" component NamedComponent

-- | Type synonym for a 'HasField' constraint for all the common component
-- fields i.e. @name@, @buildInfo@ and @qualifiedName@.
type HasComponentInfo component =
  (HasName component, HasBuildInfo component, HasQualiName component)
