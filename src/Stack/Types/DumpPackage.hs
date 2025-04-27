{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Stack.Types.DumpPackage
License     : BSD-3-Clause
-}

module Stack.Types.DumpPackage
  ( DumpPackage (..)
  , SublibDump (..)
  , DumpedGlobalPackage
  , sublibParentPkgId
  ) where

import qualified Distribution.License as C
import           Distribution.ModuleName ( ModuleName )
import           Stack.Prelude
import           Stack.Types.Component ( StackUnqualCompName )
import           Stack.Types.GhcPkgId ( GhcPkgId )

-- | Type representing dump information for a single installed package, as
-- output by the @ghc-pkg describe@ command.
data DumpPackage = DumpPackage
  { ghcPkgId :: !GhcPkgId
    -- ^ The @id@ field.
  , packageIdent :: !PackageIdentifier
    -- ^ The @name@ and @version@ fields. The @name@ field is the munged package
    -- name. If the package is not for a sub-library, its munged name is its
    -- name.
  , sublib :: !(Maybe SublibDump)
    -- ^ The sub-library information, if it is a sub-library.
  , license :: !(Maybe C.License)
  , libDirs :: ![FilePath]
    -- ^ The @library-dirs@ field.
  , libraries :: ![Text]
    -- ^ The @hs-libraries@ field.
  , hasExposedModules :: !Bool
  , exposedModules :: !(Set ModuleName)
  , depends :: ![GhcPkgId]
    -- ^ The @depends@ field (packages on which this package depends).
  , haddockInterfaces :: ![FilePath]
  , haddockHtml :: !(Maybe FilePath)
  , isExposed :: !Bool
  }
  deriving (Eq, Read, Show)

-- | An installed package for a sub-library of a Cabal package has additional
-- fields.
data SublibDump = SublibDump
  { packageName :: PackageName
    -- ^ The @package-name@ field.
  , libraryName :: StackUnqualCompName
    -- ^ The @lib-name@ field.
  }
  deriving (Eq, Read, Show)

-- | Type synonym representing dump information for a single installed package
-- in the global package database.
type DumpedGlobalPackage = DumpPackage

-- | If the given t'DumpPackage' is for a sub-library of a Cabal package, yields
-- the package identifier of the Cabal package.
sublibParentPkgId :: DumpPackage -> Maybe PackageIdentifier
sublibParentPkgId dp = dp.sublib <&> \subLibDump ->
  PackageIdentifier subLibDump.packageName dp.packageIdent.pkgVersion
